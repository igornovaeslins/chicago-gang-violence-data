# ============================================================================
# 48_regressoes_chicago.R
#
# Cross-sectional regressions on community-area means.
#
#   1. OLS, 3 nested specifications (bivariate, +disadvantage, +race+exposure)
#      with HC1 robust SEs and a wild cluster bootstrap (9999 reps)
#   2. Top-10 community areas ranked by DV per 100k residents
#   3. Invariance test of gender composition across gang families
#      (individual-level logit + Wald test)
#   4. Balance table: top vs bottom gang-coverage tertile, pre-2015 means
#   5. Moran's I on OLS residuals (Queen contiguity)
#   6. Spatial-lag (SAR) model, run only if Moran's I is significant
#
# Input  : data/processed/chicago_painel_completo.csv, data/intermediate/,
#          data/raw_boundaries/chicago_community_areas.geojson
# Outputs (results/):
#   tabela4_ols_com_controles.csv
#   tabela2_per_capita.csv
#   tabela3_invariancia_test.csv
#   balance_treated_control.csv
#   morans_i_chicago.csv
#   spatial_lag_chicago.csv  (conditional)
# ============================================================================


suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(stringr)
  library(sandwich); library(lmtest); library(fixest); library(broom)
  library(sf); library(spdep); library(spatialreg)
})

# ---- portable paths (replication package) -------------------------------
# Resolve package root from this script's own location.
.this_file <- tryCatch(
  normalizePath(sub("^--file=", "",
    grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1])),
  error = function(e) NA_character_)
ROOT <- if (!is.na(.this_file)) dirname(dirname(.this_file)) else normalizePath(".")
INT    <- file.path(ROOT, "data", "intermediate")
PROC   <- file.path(ROOT, "data", "processed")
RES    <- file.path(ROOT, "results")
CA_GEO <- file.path(ROOT, "data", "raw_boundaries", "chicago_community_areas.geojson")
dir.create(RES, showWarnings = FALSE, recursive = TRUE)
# -------------------------------------------------------------------------

set.seed(1789)
B_BOOT <- 9999

cat("==============================================================\n")
cat("48_regressoes_chicago.R\n\n")

# ---------------------------------------------------------------------------
# 1. Painel + médias por CA (exclui 2015)
# ---------------------------------------------------------------------------
df <- read_csv(file.path(PROC, "chicago_painel_completo.csv"),
               show_col_types = FALSE) |>
  filter(year != 2015)

cat("Painel sem 2015:", nrow(df), "linhas |",
    length(unique(df$community_area)), "CAs |",
    paste(range(df$year), collapse = "–"), "\n")

# Community-area means (cross-section)
ca_means <- df |>
  group_by(community_area, community_area_name) |>
  summarise(
    avg_pct_gang    = mean(pct_area_gang_control, na.rm = TRUE),
    avg_n_gangs     = mean(n_gangs_present, na.rm = TRUE),
    avg_dv          = mean(n_dv_crimes, na.rm = TRUE),
    avg_fem_shoot   = mean(n_fem_shoot, na.rm = TRUE),
    avg_fem_fatal   = mean(n_fem_shoot_fatal, na.rm = TRUE),
    avg_homicides   = mean(n_homicides, na.rm = TRUE),
    avg_total_pop   = mean(total_pop, na.rm = TRUE),
    avg_pop_female  = mean(pop_female, na.rm = TRUE),
    avg_density     = mean(pop_density_per_km2, na.rm = TRUE),
    avg_pct_black   = mean(pct_black, na.rm = TRUE),
    avg_pct_latino  = mean(pct_latino, na.rm = TRUE),
    avg_pct_pov     = mean(pct_poverty, na.rm = TRUE),
    avg_med_inc     = mean(median_hh_income, na.rm = TRUE),
    avg_unemp_y     = mean(pct_unemp_young, na.rm = TRUE),
    avg_bach        = mean(pct_bachelor_plus, na.rm = TRUE),
    gang_family     = {
      tt <- table(gang_family)
      if (length(tt) == 0) NA_character_ else names(sort(tt, decreasing = TRUE))[1]
    },
    .groups = "drop"
  ) |>
  mutate(
    avg_dv_per_100k       = avg_dv / avg_total_pop * 1e5,
    avg_fem_shoot_per_100k_f = avg_fem_shoot / avg_pop_female * 1e5,
    log_pop  = log(avg_total_pop),
    log_inc  = log(avg_med_inc)
  )

cat("CA-means:", nrow(ca_means), "CAs com agregados\n\n")

# ---------------------------------------------------------------------------
# 2. Cross-sectional OLS with nested controls
# ---------------------------------------------------------------------------
# Especificações:
#   M1 (bivariado): avg_dv ~ avg_pct_gang
#   M2 (+disadvantage): + pov, unemp_y, log_inc, bach
#   M3 (+race+exposure): + pct_black, pct_latino, log_pop, avg_homicides
specs <- list(
  M1 = avg_dv ~ avg_pct_gang,
  M2 = avg_dv ~ avg_pct_gang + avg_pct_pov + avg_unemp_y + log_inc + avg_bach,
  M3 = avg_dv ~ avg_pct_gang + avg_pct_pov + avg_unemp_y + log_inc + avg_bach +
                avg_pct_black + avg_pct_latino + log_pop + avg_homicides
)
specs_fem <- list(
  M1 = avg_fem_shoot ~ avg_pct_gang,
  M2 = avg_fem_shoot ~ avg_pct_gang + avg_pct_pov + avg_unemp_y + log_inc + avg_bach,
  M3 = avg_fem_shoot ~ avg_pct_gang + avg_pct_pov + avg_unemp_y + log_inc + avg_bach +
                       avg_pct_black + avg_pct_latino + log_pop + avg_homicides
)
specs_perk <- list(
  M1 = avg_dv_per_100k ~ avg_pct_gang,
  M2 = avg_dv_per_100k ~ avg_pct_gang + avg_pct_pov + avg_unemp_y + log_inc + avg_bach,
  M3 = avg_dv_per_100k ~ avg_pct_gang + avg_pct_pov + avg_unemp_y + log_inc + avg_bach +
                         avg_pct_black + avg_pct_latino + log_pop + avg_homicides
)

run_spec <- function(formula, data, label_outcome, label_spec, R = B_BOOT) {
  mod <- lm(formula, data = data)
  cof_hc <- coeftest(mod, vcov = vcovHC(mod, type = "HC1"))
  cof_b  <- tryCatch({
    bs_v <- vcovBS(mod, type = "wild", R = R, cluster = NULL)
    coeftest(mod, vcov = bs_v)
  }, error = function(e) NULL)

  tibble(
    outcome = label_outcome,
    model   = label_spec,
    term    = rownames(cof_hc),
    estimate = cof_hc[, "Estimate"],
    se_hc1  = cof_hc[, "Std. Error"],
    p_hc1   = cof_hc[, "Pr(>|t|)"],
    se_wild = if (!is.null(cof_b)) cof_b[, "Std. Error"] else NA_real_,
    p_wild  = if (!is.null(cof_b)) cof_b[, "Pr(>|t|)"] else NA_real_,
    n       = nobs(mod),
    r2      = summary(mod)$r.squared,
    r2_adj  = summary(mod)$adj.r.squared
  )
}

cat("--- TABELA 4: OLS Cross-Sectional com controles (3 outcomes × 3 specs) ---\n")
t4_dv  <- bind_rows(lapply(names(specs),
                           function(s) run_spec(specs[[s]],  ca_means, "DV crimes (mean/yr)", s)))
t4_fem <- bind_rows(lapply(names(specs_fem),
                           function(s) run_spec(specs_fem[[s]], ca_means,
                                                "Female shootings (mean/yr)", s)))
t4_perk<- bind_rows(lapply(names(specs_perk),
                           function(s) run_spec(specs_perk[[s]], ca_means,
                                                "DV / 100k pop", s)))
tabela4 <- bind_rows(t4_dv, t4_fem, t4_perk)
write_csv(tabela4, file.path(RES, "tabela4_ols_com_controles.csv"))

# Resumo do coeficiente de interesse
foco <- tabela4 |> filter(term == "avg_pct_gang") |>
  mutate(across(c(estimate, se_hc1, p_hc1, se_wild, p_wild, r2),
                ~ round(., 4)))
cat("\nCoeficiente em avg_pct_gang através das especificações:\n")
print(as.data.frame(foco), row.names = FALSE)

# ---------------------------------------------------------------------------
# 3. Top-10 community areas by DV per 100k
# ---------------------------------------------------------------------------
top10_volume <- ca_means |>
  arrange(desc(avg_dv)) |>
  select(community_area_name, avg_dv, avg_total_pop, avg_dv_per_100k) |>
  slice(1:10) |>
  mutate(rank_volume = row_number())

top10_perk <- ca_means |>
  arrange(desc(avg_dv_per_100k)) |>
  select(community_area_name, avg_dv, avg_total_pop, avg_dv_per_100k) |>
  slice(1:10) |>
  mutate(rank_perk = row_number())

tabela2 <- full_join(top10_volume, top10_perk,
                     by = c("community_area_name","avg_dv","avg_total_pop","avg_dv_per_100k"))
write_csv(tabela2, file.path(RES, "tabela2_per_capita.csv"))

cat("\n--- TABELA 2: Top 10 por VOLUME vs PER CAPITA ---\n")
cat("Top 10 por volume absoluto:\n")
print(as.data.frame(top10_volume |>
                      mutate(avg_dv = round(avg_dv), avg_total_pop = round(avg_total_pop),
                             avg_dv_per_100k = round(avg_dv_per_100k))), row.names = FALSE)
cat("\nTop 10 por DV/100k:\n")
print(as.data.frame(top10_perk |>
                      mutate(avg_dv = round(avg_dv), avg_total_pop = round(avg_total_pop),
                             avg_dv_per_100k = round(avg_dv_per_100k))), row.names = FALSE)

# ---------------------------------------------------------------------------
# 4. Invariance test (individual-level logit)
# ---------------------------------------------------------------------------
# Carregar shootings agregados a nível individual + gang_family por CA-ano
sh <- read_csv(file.path(INT, "shootings_with_gang_territory_2008_2024.csv"),
               show_col_types = FALSE) |>
  mutate(year = as.integer(year),
         sex  = toupper(trimws(as.character(sex))),
         ca_name = toupper(trimws(as.character(community_area)))) |>
  filter(year != 2015, !is.na(sex), sex %in% c("M","F"))

ca_fam <- df |> distinct(community_area_name, gang_family) |>
  mutate(ca_name = toupper(trimws(community_area_name)))

sh <- sh |>
  inner_join(ca_fam |> select(ca_name, gang_family), by = "ca_name") |>
  filter(gang_family %in% c("Folk Nation","People Nation")) |>
  mutate(is_female = as.integer(sex == "F"),
         year = factor(year),
         ca_name = factor(ca_name),
         gang_family = factor(gang_family, levels = c("People Nation","Folk Nation")))

cat("\n--- TABELA 3: Invariância (logit individual) ---\n")
cat("N vítimas em CAs Folk/People (sem 2015):", nrow(sh),
    "| Female:", sum(sh$is_female), "\n")

# Logit with year FE, then with community-area FE (gang_family varies little
# within a CA, so the CA-FE version is included as a robustness check only).
m_logit_year <- glm(is_female ~ gang_family + year,
                    data = sh, family = binomial)
m_logit_full <- glm(is_female ~ gang_family + year + ca_name,
                    data = sh, family = binomial)

# Teste de Wald sobre gang_family
wald_year <- coeftest(m_logit_year)
wald_full <- coeftest(m_logit_full)

t3 <- tibble(
  modelo = c("Logit + Year FE", "Logit + Year FE + CA FE"),
  coef_FolkVsPeople = c(coef(m_logit_year)["gang_familyFolk Nation"],
                        coef(m_logit_full)["gang_familyFolk Nation"]),
  se = c(wald_year["gang_familyFolk Nation","Std. Error"],
         wald_full["gang_familyFolk Nation","Std. Error"]),
  z  = c(wald_year["gang_familyFolk Nation","z value"],
         wald_full["gang_familyFolk Nation","z value"]),
  p  = c(wald_year["gang_familyFolk Nation","Pr(>|z|)"],
         wald_full["gang_familyFolk Nation","Pr(>|z|)"]),
  OR = exp(c(coef(m_logit_year)["gang_familyFolk Nation"],
             coef(m_logit_full)["gang_familyFolk Nation"]))
) |> mutate(across(where(is.numeric), ~ round(., 4)))

print(as.data.frame(t3), row.names = FALSE)
write_csv(t3, file.path(RES, "tabela3_invariancia_test.csv"))

# % feminino observado nas duas famílias (descritivo)
desc3 <- sh |>
  group_by(gang_family) |>
  summarise(n_total = n(), n_female = sum(is_female),
            pct_female = n_female / n_total)
cat("\nDescritivos (% feminino por família):\n")
print(as.data.frame(desc3), row.names = FALSE)

# ---------------------------------------------------------------------------
# 5. BALANCE TABLE — Treated (top tercile pct_gang pre-2015) vs Control
# ---------------------------------------------------------------------------
pre <- df |> filter(year <= 2014) |>
  group_by(community_area, community_area_name) |>
  summarise(pre_pct_gang = mean(pct_area_gang_control, na.rm = TRUE),
            pre_dv = mean(n_dv_crimes, na.rm = TRUE),
            pre_hom = mean(n_homicides, na.rm = TRUE),
            pre_pop = mean(total_pop, na.rm = TRUE),
            pre_pct_black = mean(pct_black, na.rm = TRUE),
            pre_pct_latino = mean(pct_latino, na.rm = TRUE),
            pre_pct_poverty = mean(pct_poverty, na.rm = TRUE),
            pre_med_inc = mean(median_hh_income, na.rm = TRUE),
            pre_unemp_y = mean(pct_unemp_young, na.rm = TRUE),
            pre_bach = mean(pct_bachelor_plus, na.rm = TRUE),
            pre_density = mean(pop_density_per_km2, na.rm = TRUE),
            .groups = "drop")

q33 <- quantile(pre$pre_pct_gang, 1/3, na.rm = TRUE)
q67 <- quantile(pre$pre_pct_gang, 2/3, na.rm = TRUE)
pre <- pre |> mutate(group = case_when(
  pre_pct_gang <= q33 ~ "Control",
  pre_pct_gang >= q67 ~ "Treated",
  TRUE                 ~ "Middle"
))

cat("\n--- BALANCE: Treated (top tercil) vs Control (bottom tercil) ---\n")
cat(sprintf("Quantis: 33%% = %.3f | 67%% = %.3f\n", q33, q67))

balance_vars <- c("pre_dv","pre_hom","pre_pop","pre_density",
                  "pre_pct_black","pre_pct_latino","pre_pct_poverty",
                  "pre_med_inc","pre_unemp_y","pre_bach","pre_pct_gang")

bal <- lapply(balance_vars, function(v) {
  d <- pre |> filter(group %in% c("Control","Treated"))
  x_c <- d[[v]][d$group == "Control"]
  x_t <- d[[v]][d$group == "Treated"]
  tt <- t.test(x_t, x_c)
  smd <- (mean(x_t, na.rm=TRUE) - mean(x_c, na.rm=TRUE)) /
         sqrt((var(x_t, na.rm=TRUE) + var(x_c, na.rm=TRUE))/2)
  tibble(variable = v,
         mean_control = mean(x_c, na.rm=TRUE),
         mean_treated = mean(x_t, na.rm=TRUE),
         diff         = mean(x_t, na.rm=TRUE) - mean(x_c, na.rm=TRUE),
         t_stat       = tt$statistic,
         p_value      = tt$p.value,
         smd_cohen_d  = smd)
}) |> bind_rows() |>
  mutate(across(where(is.numeric), ~ round(., 4)))

print(as.data.frame(bal), row.names = FALSE)
write_csv(bal, file.path(RES, "balance_treated_control.csv"))

# ---------------------------------------------------------------------------
# 6. Moran's I: spatial autocorrelation of the OLS residuals
# ---------------------------------------------------------------------------
cat("\n--- MORAN'S I (resíduos do OLS Tabela 4 — M3 completa) ---\n")
ca_sf <- st_read(CA_GEO, quiet = TRUE) |> st_make_valid()
area_col <- intersect(c("area_numbe","area_num_1","AREA_NUMBE"), names(ca_sf))[1]
ca_sf$ca_num <- as.integer(ca_sf[[area_col]])

merged <- ca_means |>
  inner_join(st_drop_geometry(ca_sf) |>
               select(ca_num) |> rename(community_area = ca_num),
             by = "community_area") |>
  inner_join(ca_sf |> select(ca_num) |>
               rename(community_area = ca_num),
             by = "community_area") |>
  st_as_sf()

merged_nona <- merged |>
  filter(!is.na(avg_dv), !is.na(avg_pct_gang), !is.na(avg_pct_pov),
         !is.na(avg_unemp_y), !is.na(log_inc), !is.na(avg_bach),
         !is.na(avg_pct_black), !is.na(avg_pct_latino),
         !is.na(log_pop), !is.na(avg_homicides))

mod_full <- lm(specs$M3, data = merged_nona)
resid_v  <- residuals(mod_full)

# Pesos Queen
nb <- poly2nb(merged_nona, queen = TRUE)
# Trata ilhas (CAs sem vizinhos — ex: O'Hare)
n_islands <- sum(card(nb) == 0)
if (n_islands > 0) {
  cat("Atenção:", n_islands, "CAs sem vizinhos Queen; usando style='W' com zero.policy=TRUE\n")
}
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

moran_res <- moran.test(resid_v, lw, zero.policy = TRUE)

tabela_moran <- tibble(
  estatistica   = c("Moran's I","E[I]","Var(I)","Std deviate","p-value (one-tailed)"),
  valor         = c(moran_res$estimate["Moran I statistic"],
                    moran_res$estimate["Expectation"],
                    moran_res$estimate["Variance"],
                    moran_res$statistic,
                    moran_res$p.value)
) |> mutate(valor = round(valor, 5))

print(as.data.frame(tabela_moran), row.names = FALSE)
write_csv(tabela_moran, file.path(RES, "morans_i_chicago.csv"))

# ---------------------------------------------------------------------------
# 7. Spatial lag (SAR) se Moran sig.
# ---------------------------------------------------------------------------
if (moran_res$p.value < 0.10) {
  cat("\n--- SAR (spatial lag) — Moran sig. p =", round(moran_res$p.value, 4), "---\n")
  sar <- spatialreg::lagsarlm(specs$M3, data = merged_nona,
                              listw = lw, zero.policy = TRUE)
  sar_summ <- summary(sar)
  sar_tab <- tibble(
    term     = rownames(sar_summ$Coef),
    estimate = sar_summ$Coef[, "Estimate"],
    se       = sar_summ$Coef[, "Std. Error"],
    z        = sar_summ$Coef[, "z value"],
    p        = sar_summ$Coef[, "Pr(>|z|)"]
  ) |> bind_rows(tibble(term = "rho_lag", estimate = sar_summ$rho,
                        se = sar_summ$rho.se, z = sar_summ$rho/sar_summ$rho.se,
                        p = 2*(1 - pnorm(abs(sar_summ$rho/sar_summ$rho.se))))) |>
    mutate(across(where(is.numeric), ~ round(., 4)))
  print(as.data.frame(sar_tab), row.names = FALSE)
  write_csv(sar_tab, file.path(RES, "spatial_lag_chicago.csv"))
} else {
  cat("\nMoran's I NÃO significativo a 10%; SAR não estimado.\n")
}

cat("\n==============================================================\n")
cat("Outputs:\n")
cat("  results/tabela4_ols_com_controles.csv\n")
cat("  results/tabela2_per_capita.csv\n")
cat("  results/tabela3_invariancia_test.csv\n")
cat("  results/balance_treated_control.csv\n")
cat("  results/morans_i_chicago.csv\n")
if (moran_res$p.value < 0.10) cat("  results/spatial_lag_chicago.csv\n")
cat("==============================================================\n")
