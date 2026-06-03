# ============================================================================
# 49_event_study_chicago.R
#
# Event study and difference-in-differences around 2015.
#
#   1. TWFE event study, year-by-year coefficients with 2014 as reference.
#      Outcomes: total female shooting victims; fatal female shootings.
#      Treatment = top tertile of pre-2015 pct_area_gang_control.
#      Inference: wild cluster bootstrap (community-area) via sandwich::vcovBS.
#   2. Event-study plot.
#   3. Public vs residential decomposition: DiD estimated separately on
#      fem_shoot_pub and fem_shoot_res, with a Wald test of beta_pub = beta_res
#      from a stacked interaction specification.
#   4. Main DiD table (four outcomes).
#
# Input  : data/processed/chicago_painel_completo.csv
# Outputs:
#   results/event_study.csv         (year-by-year coefficients)
#   results/tabela6_did_principal.csv        (main DiD)
#   results/tabela7_decomp_pub_res.csv       (Wald test)
# ============================================================================


suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(stringr)
  library(fixest); library(sandwich); library(lmtest); library(broom)
  library(ggplot2); library(scales)
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

set.seed(20260514)
B_BOOT <- 9999  # número de replicações wild bootstrap

cat("==============================================================\n")
cat("49_event_study_chicago.R\n\n")

# ---------------------------------------------------------------------------
# 1. Painel + definição de tratamento (top tercil pre-2015)
# ---------------------------------------------------------------------------
df <- read_csv(file.path(PROC, "chicago_painel_completo.csv"),
               show_col_types = FALSE) |>
  filter(year != 2015) |>
  filter(year >= 2010)  # analysis window

pre <- df |>
  filter(year <= 2014) |>
  group_by(community_area) |>
  summarise(pre_pct_gang = mean(pct_area_gang_control, na.rm = TRUE),
            .groups = "drop")

q33 <- quantile(pre$pre_pct_gang, 1/3, na.rm = TRUE)
q67 <- quantile(pre$pre_pct_gang, 2/3, na.rm = TRUE)
pre <- pre |>
  mutate(group = case_when(
    pre_pct_gang <= q33 ~ "Control",
    pre_pct_gang >= q67 ~ "Treated",
    TRUE                 ~ "Middle"
  ),
  treat = as.integer(group == "Treated"))

df <- df |>
  left_join(pre |> select(community_area, group, treat), by = "community_area") |>
  filter(group %in% c("Control","Treated"))

cat(sprintf("Tercis pct_gang pre-2015: q33=%.3f, q67=%.3f\n", q33, q67))
cat("Amostra final:", nrow(df), "obs |",
    sum(pre$group == "Treated"), "treated +",
    sum(pre$group == "Control"), "control CAs\n\n")

# ---------------------------------------------------------------------------
# 2. Event study: year-by-year coefficients
# ---------------------------------------------------------------------------
run_event_study <- function(data, outcome, ref_year = 2014) {
  fm <- as.formula(sprintf("%s ~ i(year, treat, ref = %d) | community_area + year",
                           outcome, ref_year))
  es <- feols(fm, data = data, cluster = ~community_area)
  # Wild cluster bootstrap via vcovBS
  mod_lm <- lm(as.formula(sprintf(
    "%s ~ factor(year) * treat + factor(community_area)", outcome)), data = data)
  vbs <- tryCatch(vcovBS(mod_lm, cluster = ~community_area, type = "wild",
                         R = B_BOOT),
                  error = function(e) { warning(e); NULL })
  list(es = es, mod_lm = mod_lm, vbs = vbs)
}

es_total <- run_event_study(df, "n_fem_shoot")
es_fatal <- run_event_study(df, "n_fem_shoot_fatal")

extract_coefs <- function(es_obj, outcome_label, ref_year = 2014) {
  ct <- tidy(es_obj$es, conf.int = TRUE, conf.level = 0.95)
  ct$outcome <- outcome_label
  ct$year <- suppressWarnings(as.integer(
    gsub(".*::([0-9]+):.*", "\\1", ct$term)))
  ct <- ct |> filter(!is.na(year)) |>
    select(outcome, year, estimate, std.error, statistic, p.value,
           conf.low, conf.high)

  # Adicionar linha de referência (zero)
  bind_rows(ct, tibble(outcome = outcome_label, year = ref_year,
                       estimate = 0, std.error = NA, statistic = NA,
                       p.value = NA, conf.low = NA, conf.high = NA))
}

es_tab <- bind_rows(
  extract_coefs(es_total, "Female shootings"),
  extract_coefs(es_fatal, "Fatal female shootings")
)
write_csv(es_tab, file.path(RES, "event_study.csv"))

cat("--- EVENT STUDY: Coeficientes ano-a-ano (ref = 2014) ---\n")
print(as.data.frame(es_tab |>
  mutate(across(c(estimate, std.error, conf.low, conf.high, p.value),
                ~ round(., 3)))), row.names = FALSE)

# Teste de pré-tendências: F-test conjunto sobre coeficientes pré-2014
pre_years <- function(data, outcome, end_pre = 2013, ref_year = 2014) {
  fm <- as.formula(sprintf("%s ~ i(year, treat, ref = %d) | community_area + year",
                           outcome, ref_year))
  es <- feols(fm, data = data, cluster = ~community_area)
  coefs <- coef(es)
  pre_keys <- grep(paste0("year::(2[0-9]+):treat"), names(coefs), value = TRUE)
  pre_keys <- pre_keys[suppressWarnings(as.integer(gsub(".*::([0-9]+):.*", "\\1", pre_keys))) < ref_year]
  if (length(pre_keys) == 0) return(tibble(outcome = outcome, F = NA, p = NA))
  wald <- wald(es, keep = paste0("year::", gsub("year::([0-9]+):.*","\\1",pre_keys)))
  tibble(outcome = outcome, F = unname(wald$stat), p = unname(wald$p))
}

pt_test <- bind_rows(
  pre_years(df, "n_fem_shoot"),
  pre_years(df, "n_fem_shoot_fatal")
)
cat("\n--- TESTE DE PRÉ-TENDÊNCIAS (F-test conjunto pré-2014) ---\n")
print(as.data.frame(pt_test |>
  mutate(F = round(F, 3), p = round(p, 3))), row.names = FALSE)

# ---------------------------------------------------------------------------
# 3. PLOT do event study
# ---------------------------------------------------------------------------
pdf_path <- file.path(RES, "event_study.pdf")
p <- ggplot(es_tab, aes(x = year, y = estimate)) +
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed") +
  geom_vline(xintercept = 2014.5, color = "gray40", linetype = "dotted") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  color = "black", size = 0.5) +
  facet_wrap(~ outcome, scales = "free_y", ncol = 1) +
  scale_x_continuous(breaks = seq(2010, 2024, 2)) +
  labs(
    title = "Event Study: Treated vs Control CAs (top vs bottom tercile gang coverage)",
    subtitle = "Reference year = 2014; vertical line at Nov 2015",
    x = "Year",
    y = "Coefficient (Treat × Year, 95% CI)",
    caption = "TWFE OLS with community-area and year fixed effects; SE clustered by CA. Treat = top tercile pre-2015 pct gang coverage."
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.caption = element_text(size = 8, color = "gray30"),
        strip.text = element_text(face = "bold"))

ggsave(pdf_path, p, width = 7, height = 7)
cat("\nPlot saved to:", pdf_path, "\n")

# ---------------------------------------------------------------------------
# 4. Public vs residential decomposition (Wald test)
# ---------------------------------------------------------------------------
cat("\n--- TABELA 7: Decomposição público vs residencial (DiD + Wald) ---\n")

df_did <- df |>
  mutate(post = as.integer(year >= 2016),
         did = treat * post)

# DiD separado para cada outcome
did_pub <- feols(n_fem_shoot_pub ~ did | community_area + year,
                 data = df_did, cluster = ~community_area)
did_res <- feols(n_fem_shoot_res ~ did | community_area + year,
                 data = df_did, cluster = ~community_area)
did_fat <- feols(n_fem_shoot_fatal ~ did | community_area + year,
                 data = df_did, cluster = ~community_area)
did_tot <- feols(n_fem_shoot ~ did | community_area + year,
                 data = df_did, cluster = ~community_area)

# Especificação empilhada para Wald β_pub = β_res
df_stack <- bind_rows(
  df_did |> transmute(community_area, year, did, treat, post,
                       Y = n_fem_shoot_pub, type = "public"),
  df_did |> transmute(community_area, year, did, treat, post,
                       Y = n_fem_shoot_res, type = "residence")
) |>
  mutate(type = factor(type, levels = c("residence","public")),
         did_x_pub = did * (type == "public"))

# Y_it,k = β_res * did + (β_pub - β_res) * (did × pub) + CA-k FE + Year-k FE
stack_fm <- feols(Y ~ did + did_x_pub | community_area^type + year^type,
                  data = df_stack, cluster = ~community_area)

# Wald test of equality: (beta_pub - beta_res) = 0,
# que é o coeficiente em did_x_pub
wald_pubres <- coeftest(stack_fm)
b_diff <- coef(stack_fm)["did_x_pub"]
se_diff <- sqrt(diag(vcov(stack_fm)))["did_x_pub"]
z_diff <- b_diff / se_diff
p_diff <- 2 * (1 - pnorm(abs(z_diff)))

tabela7 <- tibble(
  outcome = c("Total female shootings",
              "Female shootings — RESIDENCE",
              "Female shootings — PUBLIC",
              "Fatal female shootings",
              "Difference (Public − Residence)"),
  beta = c(coef(did_tot)["did"], coef(did_res)["did"], coef(did_pub)["did"],
           coef(did_fat)["did"], b_diff),
  se = c(sqrt(diag(vcov(did_tot)))["did"],
         sqrt(diag(vcov(did_res)))["did"],
         sqrt(diag(vcov(did_pub)))["did"],
         sqrt(diag(vcov(did_fat)))["did"],
         se_diff),
  p_value = c(coeftest(did_tot)["did","Pr(>|t|)"],
              coeftest(did_res)["did","Pr(>|t|)"],
              coeftest(did_pub)["did","Pr(>|t|)"],
              coeftest(did_fat)["did","Pr(>|t|)"],
              p_diff),
  n = c(nobs(did_tot), nobs(did_res), nobs(did_pub), nobs(did_fat),
        nobs(stack_fm))
) |> mutate(across(c(beta, se, p_value), ~ round(., 4)))

print(as.data.frame(tabela7), row.names = FALSE)
write_csv(tabela7, file.path(RES, "tabela7_decomp_pub_res.csv"))

# Razão pub/res
if (coef(did_res)["did"] > 0) {
  cat(sprintf("\nRatio beta_pub / beta_res = %.2f\n",
              coef(did_pub)["did"] / coef(did_res)["did"]))
}

# ---------------------------------------------------------------------------
# 4. Main DiD table (four outcomes)
#    Outcomes: female shoots, fatal female, DV homicides, all shoots
# ---------------------------------------------------------------------------
cat("\n--- TABELA DiD PRINCIPAL (4 outcomes, reproduzível) ---\n")
outcomes_main <- c(
  "Total female shootings" = "n_fem_shoot",
  "Fatal female shootings" = "n_fem_shoot_fatal",
  "DV-flagged homicides"   = "n_dv_homicide",
  "All shooting victims"   = "n_shoot_total"
)
tab_did_main <- lapply(names(outcomes_main), function(lab) {
  v  <- outcomes_main[[lab]]
  m  <- feols(as.formula(paste(v, "~ did | community_area + year")),
              data = df_did, cluster = ~community_area)
  ct <- coeftest(m)
  pm <- mean(df_did[[v]][df_did$treat == 1 & df_did$year <= 2014], na.rm = TRUE)
  pc <- mean(df_did[[v]][df_did$treat == 0 & df_did$year <= 2014], na.rm = TRUE)
  tibble(
    outcome  = lab,
    beta     = round(coef(m)["did"], 3),
    se       = round(sqrt(diag(vcov(m)))["did"], 3),
    p_value  = round(ct["did", "Pr(>|t|)"], 4),
    pre_mean_treat   = round(pm, 2),
    pre_mean_control = round(pc, 2),
    implied_pct      = round(100 * coef(m)["did"] / pm, 0)
  )
}) |> bind_rows()

print(as.data.frame(tab_did_main), row.names = FALSE)
write_csv(tab_did_main, file.path(RES, "tabela6_did_principal.csv"))
cat("Salvo: results/tabela6_did_principal.csv\n")

cat("\n==============================================================\n")
cat("Outputs:\n")
cat("  results/event_study.csv\n")
cat("  results/event_study.pdf\n")
cat("  results/tabela7_decomp_pub_res.csv\n")
cat("==============================================================\n")
