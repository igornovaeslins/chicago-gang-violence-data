# ============================================================================
# 50_robustez_matching_chicago.R
#
# Entropy-balanced robustness check.
#
# Re-estimates the main DiD and event study after entropy balancing, building a
# control group whose pre-treatment moments match the treated group.
#
# Algoritmo:
#   1. Construir pre (CA-level médias pré-2015 das covariáveis)
#   2. WeightIt(method="ebal") com estimand="ATT" — pesa o controle
#      para reproduzir os momentos do treated em:
#         pre_pct_black, pre_pct_latino, pre_pct_poverty, pre_med_inc,
#         pre_unemp_y, pre_bach, pre_density, pre_pop
#   3. cobalt::bal.tab para avaliar balanço pré e pós-pesos
#   4. Mesclar pesos no painel e reestimar DiD e event study com weights
#
# Outputs:
#   results/balance_post_ebal.csv      (balanço pós-entropy balancing)
#   results/did_ebal_chicago.csv       (coeficientes DiD reestimados)
#   results/event_study_ebal.csv       (event study reestimado)
#   results/event_study_ebal.pdf       (figura comparando original vs ebal)
# ============================================================================


suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(stringr)
  library(WeightIt); library(cobalt); library(fixest); library(broom)
  library(ggplot2); library(lmtest); library(sandwich)
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

cat("==============================================================\n")
cat("50_robustez_matching_chicago.R\n\n")

# ---------------------------------------------------------------------------
# 1. Panel + treatment (same definition as script 49)
# ---------------------------------------------------------------------------
df <- read_csv(file.path(PROC, "chicago_painel_completo.csv"),
               show_col_types = FALSE) |>
  filter(year != 2015, year >= 2010)

pre <- df |>
  filter(year <= 2014) |>
  group_by(community_area, community_area_name) |>
  summarise(pre_pct_gang = mean(pct_area_gang_control, na.rm = TRUE),
            pre_pct_black = mean(pct_black, na.rm = TRUE),
            pre_pct_latino = mean(pct_latino, na.rm = TRUE),
            pre_pct_poverty = mean(pct_poverty, na.rm = TRUE),
            pre_med_inc = mean(median_hh_income, na.rm = TRUE),
            pre_unemp_y = mean(pct_unemp_young, na.rm = TRUE),
            pre_bach = mean(pct_bachelor_plus, na.rm = TRUE),
            pre_density = mean(pop_density_per_km2, na.rm = TRUE),
            pre_pop = mean(total_pop, na.rm = TRUE),
            pre_dv = mean(n_dv_crimes, na.rm = TRUE),
            pre_hom = mean(n_homicides, na.rm = TRUE),
            .groups = "drop")

q33 <- quantile(pre$pre_pct_gang, 1/3, na.rm = TRUE)
q67 <- quantile(pre$pre_pct_gang, 2/3, na.rm = TRUE)
pre <- pre |>
  mutate(group = case_when(
    pre_pct_gang <= q33 ~ "Control",
    pre_pct_gang >= q67 ~ "Treated",
    TRUE                 ~ "Middle"),
    treat = as.integer(group == "Treated"))

ebal_data <- pre |>
  filter(group %in% c("Control","Treated")) |>
  drop_na(pre_pct_black, pre_pct_latino, pre_pct_poverty, pre_med_inc,
          pre_unemp_y, pre_bach, pre_density, pre_pop)

cat("Amostra para entropy balancing:",
    sum(ebal_data$group == "Treated"), "treated +",
    sum(ebal_data$group == "Control"), "control CAs\n\n")

# ---------------------------------------------------------------------------
# 2. Entropy balancing (estimand = ATT)
# ---------------------------------------------------------------------------
# --- DIAGNÓSTICO DE OVERLAP ---
# Antes de pesar, verificamos sobreposição entre tratados e controles.
# Without overlap (treated outside the control envelope) entropy balancing
# fails to converge; this is reported.
cat("--- Diagnóstico de OVERLAP (treated vs control) ---\n")
overlap_diag <- ebal_data |>
  group_by(group) |>
  summarise(across(c(pre_pct_black, pre_pct_poverty, pre_med_inc,
                     pre_unemp_y, pre_bach, pre_density),
                   list(min = ~min(.x, na.rm=TRUE),
                        max = ~max(.x, na.rm=TRUE),
                        mean = ~mean(.x, na.rm=TRUE)),
                   .names = "{.col}__{.fn}"))
print(as.data.frame(overlap_diag |>
        pivot_longer(-group) |>
        separate(name, c("var","stat"), sep="__") |>
        pivot_wider(names_from = c(group, stat), values_from = value)) |>
      mutate(across(where(is.numeric), ~ round(., 3))), row.names = FALSE)

# Spec preferida: disadvantage + raça (todos)
covs_full <- c("pre_pct_black","pre_pct_poverty","pre_med_inc","pre_density")
# Spec fallback: só desvantagem (sem raça, onde overlap é mínimo)
covs_min  <- c("pre_pct_poverty","pre_med_inc")

try_ebal <- function(covs, data, label) {
  cat(sprintf("\n--- Tentativa ebal (%s): %s ---\n",
              label, paste(covs, collapse = " + ")))
  W <- tryCatch(
    weightit(as.formula(paste("treat ~", paste(covs, collapse = " + "))),
             data = data, method = "ebal", estimand = "ATT",
             maxit = 10000),
    error = function(e) { cat("  erro:", conditionMessage(e), "\n"); NULL }
  )
  if (is.null(W)) return(NULL)
  if (all(W$weights[data$treat == 0] == 0, na.rm = TRUE)) {
    cat("  AVISO: convergência degenerada — todos os pesos do controle = 0\n")
    return(NULL)
  }
  cat("  OK; effective sample size do controle =",
      round(sum(W$weights[data$treat==0])^2 /
              sum(W$weights[data$treat==0]^2), 2), "/",
      sum(data$treat == 0), "\n")
  W
}

W <- try_ebal(covs_full, ebal_data, "completo")
covs_used <- covs_full
if (is.null(W)) {
  W <- try_ebal(covs_min, ebal_data, "mínimo (sem raça)")
  covs_used <- covs_min
}
if (is.null(W)) {
  stop("Entropy balancing falhou em todas as especificações. ",
       "Lack of common support entre treated e control — relatar como achado.")
}

ebal_data$w_ebal <- W$weights

# ---------------------------------------------------------------------------
# 3. Avaliar balanço pré e pós (cobalt)
# ---------------------------------------------------------------------------
bal <- bal.tab(W, un = TRUE, m.threshold = 0.1)
cat("--- BALANÇO Treated vs Control: pré e pós entropy balancing ---\n")
print(bal)

bal_df <- as.data.frame(bal$Balance) |>
  tibble::rownames_to_column("variable") |>
  select(variable, mean_diff_un = Diff.Un, mean_diff_w = Diff.Adj) |>
  mutate(across(where(is.numeric), ~ round(., 4)))
write_csv(bal_df, file.path(RES, "balance_post_ebal.csv"))

# ---------------------------------------------------------------------------
# 4. Mesclar pesos no painel
# ---------------------------------------------------------------------------
df_w <- df |>
  inner_join(ebal_data |> select(community_area, w_ebal, group, treat),
             by = "community_area") |>
  mutate(post = as.integer(year >= 2016),
         did  = treat * post)

# ---------------------------------------------------------------------------
# 5. Reestimar DiD com weights
# ---------------------------------------------------------------------------
fit_did <- function(outcome) {
  fm <- as.formula(sprintf("%s ~ did | community_area + year", outcome))
  m_un <- feols(fm, data = df_w, cluster = ~community_area)
  m_w  <- feols(fm, data = df_w, weights = ~w_ebal, cluster = ~community_area)
  tibble(
    outcome = outcome,
    spec    = c("Unweighted","Entropy-balanced"),
    beta    = c(coef(m_un)["did"], coef(m_w)["did"]),
    se      = c(sqrt(diag(vcov(m_un)))["did"], sqrt(diag(vcov(m_w)))["did"]),
    p_value = c(coeftest(m_un)["did","Pr(>|t|)"],
                coeftest(m_w)["did","Pr(>|t|)"]),
    n = c(nobs(m_un), nobs(m_w))
  )
}

did_results <- bind_rows(
  fit_did("n_fem_shoot"),
  fit_did("n_fem_shoot_fatal"),
  fit_did("n_fem_shoot_pub"),
  fit_did("n_fem_shoot_res")
) |> mutate(across(c(beta, se, p_value), ~ round(., 4)))

cat("\n--- DiD reestimado (Unweighted vs Entropy-balanced) ---\n")
print(as.data.frame(did_results), row.names = FALSE)
write_csv(did_results, file.path(RES, "did_ebal_chicago.csv"))

# ---------------------------------------------------------------------------
# 6. Event study com weights
# ---------------------------------------------------------------------------
es_un <- feols(n_fem_shoot ~ i(year, treat, ref = 2014) | community_area + year,
               data = df_w, cluster = ~community_area)
es_w  <- feols(n_fem_shoot ~ i(year, treat, ref = 2014) | community_area + year,
               data = df_w, weights = ~w_ebal, cluster = ~community_area)
es_fat_un <- feols(n_fem_shoot_fatal ~ i(year, treat, ref = 2014) |
                     community_area + year,
                   data = df_w, cluster = ~community_area)
es_fat_w  <- feols(n_fem_shoot_fatal ~ i(year, treat, ref = 2014) |
                     community_area + year,
                   data = df_w, weights = ~w_ebal, cluster = ~community_area)

# Pré-trends F-test ponderado
pt_w <- tryCatch({
  wald(es_w, keep = "year::201[0-3]:treat")
}, error = function(e) list(stat = NA, p = NA))
pt_fat_w <- tryCatch({
  wald(es_fat_w, keep = "year::201[0-3]:treat")
}, error = function(e) list(stat = NA, p = NA))

cat(sprintf("\nPré-trends test (Female shootings) | unweighted vs ebal:\n"))
pt_u <- wald(es_un, keep = "year::201[0-3]:treat")
cat(sprintf("  Unweighted: F=%.2f  p=%.3f\n",
            unname(pt_u$stat), unname(pt_u$p)))
cat(sprintf("  Ebal:       F=%.2f  p=%.3f\n",
            unname(pt_w$stat), unname(pt_w$p)))
cat(sprintf("\nPré-trends test (Fatal female) | unweighted vs ebal:\n"))
pt_fu <- wald(es_fat_un, keep = "year::201[0-3]:treat")
cat(sprintf("  Unweighted: F=%.2f  p=%.3f\n",
            unname(pt_fu$stat), unname(pt_fu$p)))
cat(sprintf("  Ebal:       F=%.2f  p=%.3f\n",
            unname(pt_fat_w$stat), unname(pt_fat_w$p)))

# Event-study table, weighted vs unweighted
extract_es <- function(m, label, spec) {
  ct <- tidy(m, conf.int = TRUE, conf.level = 0.95) |>
    filter(grepl("year::[0-9]+:treat", term)) |>
    mutate(year = suppressWarnings(as.integer(gsub(".*::([0-9]+):.*","\\1", term))),
           outcome = label, spec = spec) |>
    select(outcome, spec, year, estimate, std.error, conf.low, conf.high, p.value)
  ct
}

es_tab <- bind_rows(
  extract_es(es_un,    "Female shootings",       "Unweighted"),
  extract_es(es_w,     "Female shootings",       "Entropy-balanced"),
  extract_es(es_fat_un,"Fatal female shootings", "Unweighted"),
  extract_es(es_fat_w, "Fatal female shootings", "Entropy-balanced")
)
write_csv(es_tab, file.path(RES, "event_study_ebal.csv"))

# Plot lado-a-lado
pdf_path <- file.path(RES, "event_study_ebal.pdf")
p <- ggplot(es_tab, aes(x = year, y = estimate, color = spec, group = spec)) +
  geom_hline(yintercept = 0, color = "gray60", linetype = "dashed") +
  geom_vline(xintercept = 2014.5, color = "gray40", linetype = "dotted") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = 0.4), size = 0.4) +
  facet_wrap(~ outcome, scales = "free_y", ncol = 1) +
  scale_color_manual(values = c("Unweighted" = "black",
                                "Entropy-balanced" = "#c62828")) +
  scale_x_continuous(breaks = seq(2010, 2024, 2)) +
  labs(
    title = "Event study: original vs entropy-balanced",
    subtitle = "Ref = 2014; weights balance pre-2015 disadvantage + race",
    x = "Year", y = "Coefficient (Treat × Year, 95% CI)", color = ""
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top",
        strip.text = element_text(face = "bold"))

ggsave(pdf_path, p, width = 8, height = 7)
cat("\nPlot salvo em:", pdf_path, "\n")

cat("\n==============================================================\n")
cat("Outputs:\n")
cat("  results/balance_post_ebal.csv\n")
cat("  results/did_ebal_chicago.csv\n")
cat("  results/event_study_ebal.csv\n")
cat("  results/event_study_ebal.pdf\n")
cat("==============================================================\n")
