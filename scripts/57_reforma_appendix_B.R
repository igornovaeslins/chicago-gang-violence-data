# ============================================================================
# 57_reforma_appendix_B.R
#
# Reforma do paper "Chicago VAW" — estimativas para os novos apêndices de
# robustez B.6–B.8 e um complemento à seção 6.5.
#
# Replica EXATAMENTE a amostra e a especificação do DiD principal de
# 49_event_study_chicago.R:
#   - Painel: data_processed/chicago_painel_completo.csv
#   - Janela DiD: 2010–2024, 2015 excluído; pre = 2010–2014; post = 2016–2024
#   - Tratamento: tercis de pre_pct_gang (média pre-2015 de
#     pct_area_gang_control por CA); Treated = tercil superior,
#     Control = tercil inferior (24 + 24 CAs)
#   - TWFE: outcome ~ did | community_area + year, cluster = ~community_area
#
# Estima e grava (APENAS arquivos novos, prefixo reforma_):
#   B.7  results/reforma_b7_placebo_male.csv   — placebo: vítimas masculinas
#        (+ coeficientes femininos re-estimados na mesma rodada, p/ contexto)
#   B.7  results/reforma_b7_female_share.csv   — share feminino dos baleados
#        (não ponderado e ponderado por n_shoot_total)
#   B.6  results/reforma_b6_dose_continua.csv  — dose contínua:
#        pre_pct_gang × Post
#   B.8  results/reforma_b8_poisson.csv        — fepois das contagens do DiD
#   §6.5 results/reforma_65_fragdose_fem.csv   — fepois n_fem_shoot ~
#        n_gangs_present | CA + year (painel completo 2008–2024, ex-2015)
#
# Este script NÃO modifica nenhum arquivo pré-existente em results/.
# ============================================================================



suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr)
  library(fixest); library(broom)
})

# ---- portable paths (replication package) -------------------------------
# Resolve package root from this script's own location.
.this_file <- tryCatch(
  normalizePath(sub("^--file=", "",
    grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1])),
  error = function(e) NA_character_)
ROOT <- if (!is.na(.this_file)) dirname(dirname(.this_file)) else normalizePath(".")
INT  <- file.path(ROOT, "data", "intermediate")
PROC <- file.path(ROOT, "data", "processed")
RES  <- file.path(ROOT, "results")
dir.create(RES, showWarnings = FALSE, recursive = TRUE)
# -------------------------------------------------------------------------

cat("==============================================================\n")
cat("57_reforma_appendix_B.R — apêndices B.6–B.8 + complemento §6.5\n\n")

# ---------------------------------------------------------------------------
# 0. Helpers: captura de warnings/notes do fixest + extração tidy
# ---------------------------------------------------------------------------
capture_fixest <- function(expr) {
  notes <- character(0)
  m <- withCallingHandlers(
    expr,
    warning = function(w) {
      notes <<- c(notes, paste("WARNING:", conditionMessage(w)))
      invokeRestart("muffleWarning")
    },
    message = function(msg) {
      notes <<- c(notes, paste("NOTE:", trimws(conditionMessage(msg))))
      invokeRestart("muffleMessage")
    }
  )
  list(model = m, fixest_notes = notes)
}

tidy_model <- function(cm, outcome_label, model_label, extra_notes = "") {
  m  <- cm$model
  ct <- as.data.frame(coeftable(m))
  stopifnot(nrow(ct) >= 1)                       # tabela de coeficientes não-vazia
  stopifnot(!any(is.na(ct[[1]])), !any(is.na(ct[[2]])))
  all_notes <- paste(c(extra_notes, cm$fixest_notes), collapse = " | ")
  tibble(
    outcome    = outcome_label,
    model      = model_label,
    term       = rownames(ct),
    estimate   = ct[[1]],
    se         = ct[[2]],
    statistic  = ct[[3]],
    p_value    = ct[[4]],
    n_obs      = nobs(m),
    n_clusters = unname(m$fixef_sizes[["community_area"]]),
    notes      = all_notes
  )
}

print_model <- function(cm, header) {
  cat("\n---", header, "---\n")
  print(coeftable(cm$model))
  if (length(cm$fixest_notes) > 0)
    cat("fixest notes/warnings:\n ", paste(cm$fixest_notes, collapse = "\n  "), "\n")
}

# ---------------------------------------------------------------------------
# 1. Painel + amostra do DiD principal (idêntico a 49_event_study_chicago.R)
# ---------------------------------------------------------------------------
df_raw <- read_csv(file.path(PROC, "chicago_painel_completo.csv"),
                   show_col_types = FALSE)

req_cols <- c("community_area", "year", "n_fem_shoot", "n_fem_shoot_fatal",
              "n_male_shoot", "n_shoot_total", "n_gangs_present",
              "pct_area_gang_control")
stopifnot(all(req_cols %in% names(df_raw)))

d <- df_raw |>
  filter(year != 2015) |>
  filter(year >= 2010)      # janela do paper (como em 49)

pre <- d |>
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

df_did <- d |>
  left_join(pre |> select(community_area, pre_pct_gang, group, treat),
            by = "community_area") |>
  filter(group %in% c("Control", "Treated")) |>
  mutate(post = as.integer(year >= 2016),
         did  = treat * post)

# --- Asserts duros da amostra DiD ---
stopifnot(sum(pre$group == "Treated") == 24)
stopifnot(sum(pre$group == "Control") == 24)
stopifnot(n_distinct(df_did$community_area) == 48)
stopifnot(!(2015 %in% df_did$year))
stopifnot(min(df_did$year) == 2010, max(df_did$year) == 2024)
stopifnot(nrow(df_did) == 48 * 14)               # 672 obs, sem explosão de NA
stopifnot(!any(is.na(df_did$n_fem_shoot)),
          !any(is.na(df_did$n_male_shoot)),
          !any(is.na(df_did$n_fem_shoot_fatal)),
          !any(is.na(df_did$n_shoot_total)))

cat(sprintf("Tercis pct_gang pre-2015: q33=%.3f, q67=%.3f\n", q33, q67))
cat("Amostra DiD:", nrow(df_did), "obs |",
    sum(pre$group == "Treated"), "treated +",
    sum(pre$group == "Control"), "control CAs\n")

pre_mean <- function(v, tr) {
  mean(df_did[[v]][df_did$treat == tr & df_did$year <= 2014], na.rm = TRUE)
}

# ===========================================================================
# B.7 (i) — PLACEBO: vítimas masculinas de tiroteio
#   Mesma especificação do DiD principal; a coluna "all victims" existente
#   contém as mulheres, então o placebo limpo é n_male_shoot.
#   Inclui os coeficientes femininos re-estimados na mesma rodada (contexto).
# ===========================================================================
cat("\n==============================================================\n")
cat("B.7 (i) — Placebo masculino (TWFE OLS, spec do DiD principal)\n")

m_male <- capture_fixest(
  feols(n_male_shoot ~ did | community_area + year,
        data = df_did, cluster = ~community_area))
m_fem  <- capture_fixest(
  feols(n_fem_shoot ~ did | community_area + year,
        data = df_did, cluster = ~community_area))
m_fat  <- capture_fixest(
  feols(n_fem_shoot_fatal ~ did | community_area + year,
        data = df_did, cluster = ~community_area))

print_model(m_male, "Placebo: MALE shooting victims")
print_model(m_fem,  "Contexto: FEMALE shooting victims (DiD principal)")
print_model(m_fat,  "Contexto: FATAL female shootings (DiD principal)")

note_b7 <- paste0("Spec identica ao DiD principal (49): treat=top vs bottom ",
                  "tercil pre-2015 pct gang; 2010-2024 ex-2015; ",
                  "CA+year FE; cluster CA.")
tab_b7 <- bind_rows(
  tidy_model(m_male, "Male shooting victims (placebo)", "TWFE OLS tercile DiD",
             paste0(note_b7, sprintf(" Pre-2015 mean: treat=%.2f, ctrl=%.2f.",
                                     pre_mean("n_male_shoot", 1),
                                     pre_mean("n_male_shoot", 0)))),
  tidy_model(m_fem, "Female shooting victims", "TWFE OLS tercile DiD",
             paste0(note_b7, sprintf(" Pre-2015 mean: treat=%.2f, ctrl=%.2f.",
                                     pre_mean("n_fem_shoot", 1),
                                     pre_mean("n_fem_shoot", 0)))),
  tidy_model(m_fat, "Fatal female shootings", "TWFE OLS tercile DiD",
             paste0(note_b7, sprintf(" Pre-2015 mean: treat=%.2f, ctrl=%.2f.",
                                     pre_mean("n_fem_shoot_fatal", 1),
                                     pre_mean("n_fem_shoot_fatal", 0))))
)
write_csv(tab_b7, file.path(RES, "reforma_b7_placebo_male.csv"))
cat("\nSalvo: results/reforma_b7_placebo_male.csv\n")

# ===========================================================================
# B.7 (ii) — SHARE FEMININO dos baleados
#   fem_share = n_fem_shoot / n_shoot_total. Célula CA-ano com zero baleados
#   tem share indefinido -> excluída explicitamente (contagem reportada).
#   Duas versões: não ponderada e ponderada por n_shoot_total (a ponderada
#   dá peso proporcional à informação de cada célula; as células de zero
#   teriam peso 0 de toda forma).
# ===========================================================================
cat("\n==============================================================\n")
cat("B.7 (ii) — Female share dos baleados\n")

n_zero_cells <- sum(df_did$n_shoot_total == 0)
df_share <- df_did |>
  filter(n_shoot_total > 0) |>
  mutate(fem_share = n_fem_shoot / n_shoot_total)

stopifnot(nrow(df_share) == nrow(df_did) - n_zero_cells)
stopifnot(!any(is.na(df_share$fem_share)))
stopifnot(all(df_share$fem_share >= 0 & df_share$fem_share <= 1))
cat(sprintf("Células CA-ano com n_shoot_total==0 excluídas: %d de %d\n",
            n_zero_cells, nrow(df_did)))

m_share_u <- capture_fixest(
  feols(fem_share ~ did | community_area + year,
        data = df_share, cluster = ~community_area))
m_share_w <- capture_fixest(
  feols(fem_share ~ did | community_area + year,
        data = df_share, weights = ~n_shoot_total, cluster = ~community_area))

print_model(m_share_u, "Female share — não ponderado")
print_model(m_share_w, "Female share — ponderado por n_shoot_total")

note_share <- sprintf(paste0("fem_share = n_fem_shoot/n_shoot_total; %d ",
                             "celulas CA-ano com zero baleados excluidas ",
                             "(share indefinido). %s"), n_zero_cells, note_b7)
tab_share <- bind_rows(
  tidy_model(m_share_u, "Female share of shooting victims",
             "TWFE OLS tercile DiD (unweighted)", note_share),
  tidy_model(m_share_w, "Female share of shooting victims",
             "TWFE OLS tercile DiD (weighted by n_shoot_total)", note_share)
)
write_csv(tab_share, file.path(RES, "reforma_b7_female_share.csv"))
cat("\nSalvo: results/reforma_b7_female_share.csv\n")

# ===========================================================================
# B.6 — DOSE CONTÍNUA: pre_pct_gang × Post
#   pre_pct_gang computado como em 49 (média 2010-2014 de
#   pct_area_gang_control por CA). Amostra principal: todas as CAs com dose
#   pré definida (exclui CAs com pct_area_gang_control ausente em todo o
#   pré-período); check adicional na amostra de tercis (48 CAs).
# ===========================================================================
cat("\n==============================================================\n")
cat("B.6 — Dose contínua (pre_pct_gang × Post)\n")

df_dose <- d |>
  left_join(pre |> select(community_area, pre_pct_gang, group),
            by = "community_area") |>
  filter(!is.na(pre_pct_gang) & !is.nan(pre_pct_gang)) |>
  mutate(post      = as.integer(year >= 2016),
         dose_post = pre_pct_gang * post)

n_ca_dose  <- n_distinct(df_dose$community_area)
n_ca_nodef <- n_distinct(d$community_area) - n_ca_dose
stopifnot(n_ca_dose >= 48)
stopifnot(nrow(df_dose) == n_ca_dose * 14)
cat(sprintf("Amostra dose: %d CAs (%d CAs sem dose pré definida excluídas), %d obs\n",
            n_ca_dose, n_ca_nodef, nrow(df_dose)))

df_dose48 <- df_dose |> filter(group %in% c("Control", "Treated"))
stopifnot(n_distinct(df_dose48$community_area) == 48)

dose_outcomes <- c("Female shooting victims" = "n_fem_shoot",
                   "Fatal female shootings"  = "n_fem_shoot_fatal",
                   "Male shooting victims (placebo)" = "n_male_shoot")

tab_dose <- bind_rows(lapply(names(dose_outcomes), function(lab) {
  v <- dose_outcomes[[lab]]
  cm_all <- capture_fixest(
    feols(as.formula(paste(v, "~ dose_post | community_area + year")),
          data = df_dose, cluster = ~community_area))
  cm_48 <- capture_fixest(
    feols(as.formula(paste(v, "~ dose_post | community_area + year")),
          data = df_dose48, cluster = ~community_area))
  print_model(cm_all, paste0("Dose contínua (todas as CAs): ", lab))
  print_model(cm_48,  paste0("Dose contínua (amostra 48 CAs): ", lab))
  bind_rows(
    tidy_model(cm_all, lab, "Continuous dose x Post (all CAs)",
               sprintf(paste0("dose = pre-2015 mean pct_area_gang_control ",
                              "(como em 49); %d CAs sem dose pre definida ",
                              "excluidas; 2010-2024 ex-2015; CA+year FE; ",
                              "cluster CA."), n_ca_nodef)),
    tidy_model(cm_48, lab, "Continuous dose x Post (tercile sample, 48 CAs)",
               paste0("Mesma dose, restrita as 24+24 CAs do DiD principal; ",
                      "2010-2024 ex-2015; CA+year FE; cluster CA."))
  )
}))
write_csv(tab_dose, file.path(RES, "reforma_b6_dose_continua.csv"))
cat("\nSalvo: results/reforma_b6_dose_continua.csv\n")

# ===========================================================================
# B.8 — POISSON (fepois) das contagens do DiD principal
# ===========================================================================
cat("\n==============================================================\n")
cat("B.8 — fepois do DiD de tercis (contagens)\n")

tab_pois <- bind_rows(lapply(names(dose_outcomes), function(lab) {
  v <- dose_outcomes[[lab]]
  cm <- capture_fixest(
    fepois(as.formula(paste(v, "~ did | community_area + year")),
           data = df_did, cluster = ~community_area))
  print_model(cm, paste0("fepois: ", lab))
  tidy_model(cm, lab, "Poisson TWFE tercile DiD (fepois)",
             paste0(note_b7, " Coeficiente em escala log ",
                    "(exp(beta)-1 = efeito proporcional)."))
}))
write_csv(tab_pois, file.path(RES, "reforma_b8_poisson.csv"))
cat("\nSalvo: results/reforma_b8_poisson.csv\n")

# ===========================================================================
# §6.5 — FRAGMENTAÇÃO-DOSE em baleadas femininas
#   fepois(n_fem_shoot ~ n_gangs_present | CA + year), painel completo
#   2008-2024 ex-2015 — mesmo tratamento da regressão de gang-count sobre DV
#   já existente no paper (55_mechanism_tests_chicago.R, Tabela 8).
# ===========================================================================
cat("\n==============================================================\n")
cat("§6.5 — Fragmentação-dose (n_gangs_present) em baleadas femininas\n")

panel_full <- df_raw |>
  filter(year != 2015) |>
  arrange(community_area, year)

stopifnot(min(panel_full$year) == 2008, max(panel_full$year) == 2024)
stopifnot(!(2015 %in% panel_full$year))
stopifnot(nrow(panel_full) == 77 * 16)
n_na_gangs <- sum(is.na(panel_full$n_gangs_present))
cat(sprintf("Painel completo: %d obs; %d com n_gangs_present ausente (descartadas pelo fixest)\n",
            nrow(panel_full), n_na_gangs))

frag_outcomes <- c("Female shooting victims" = "n_fem_shoot",
                   "Fatal female shootings"  = "n_fem_shoot_fatal")

tab_frag <- bind_rows(lapply(names(frag_outcomes), function(lab) {
  v <- frag_outcomes[[lab]]
  cm <- capture_fixest(
    fepois(as.formula(paste(v, "~ n_gangs_present | community_area + year")),
           data = panel_full, cluster = ~community_area))
  print_model(cm, paste0("fepois fragmentação-dose: ", lab))
  tidy_model(cm, lab, "Poisson TWFE, n_gangs_present continuous",
             sprintf(paste0("Painel completo 2008-2024 ex-2015 (77 CAs); %d ",
                            "obs com n_gangs_present NA descartadas; CA+year ",
                            "FE; cluster CA. Mesmo tratamento da regressao ",
                            "gang-count sobre DV (script 55, Tabela 8)."),
                     n_na_gangs))
}))
write_csv(tab_frag, file.path(RES, "reforma_65_fragdose_fem.csv"))
cat("\nSalvo: results/reforma_65_fragdose_fem.csv\n")

# ---------------------------------------------------------------------------
# Fechamento
# ---------------------------------------------------------------------------
out_files <- c("reforma_b7_placebo_male.csv", "reforma_b7_female_share.csv",
               "reforma_b6_dose_continua.csv", "reforma_b8_poisson.csv",
               "reforma_65_fragdose_fem.csv")
stopifnot(all(file.exists(file.path(RES, out_files))))

cat("\n==============================================================\n")
cat("Outputs (todos novos, prefixo reforma_):\n")
for (f in out_files) cat("  results/", f, "\n", sep = "")
cat("==============================================================\n")
