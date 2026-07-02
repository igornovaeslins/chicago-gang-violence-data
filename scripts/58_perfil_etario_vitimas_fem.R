# ============================================================================
# 58_perfil_etario_vitimas_fem.R
#
# Teste indireto da alternativa "papeis femininos de rua mudaram" (reforma G2).
# Pergunta: o perfil ETARIO das vitimas femininas nas areas tratadas mudou
# apos 2015 na direcao do perfil de conflito de gangue (jovem, como os homens)?
#   - Se SIM: consistente com envolvimento feminino crescente nos conflitos.
#   - Se NAO (perfil estavel ou mais espalhado, com volume +51%): a alternativa
#     de envolvimento perde forca frente a leitura da isencao revogada.
#
# Dados: data_intermediate/chicago/chicago_shootings_2008_2024.csv (local).
#   - community_area = NOME em maiusculas -> join via community_area_name do painel
#   - age = FAIXAS nativas ("0-19","20-29",...,"80+","UNKNOWN")
# Amostra DiD identica a 49/57: tercis de pct gang pre-2015 (24+24 CAs),
# pre = 2010-2014, post = 2016-2024, 2015 excluido.
# Grava APENAS results/reforma_age_profile_fem.csv (novo).
# ============================================================================


suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr)
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
cat("58_perfil_etario_vitimas_fem.R — perfil etario das vitimas\n\n")

# --- 1. Tercis identicos a 57 (a partir do painel) -------------------------
panel <- read_csv(file.path(PROC, "chicago_painel_completo.csv"), show_col_types = FALSE)
pre <- panel |>
  filter(year >= 2010, year <= 2014) |>
  group_by(community_area, community_area_name) |>
  summarise(pre_pct_gang = mean(pct_area_gang_control, na.rm = TRUE), .groups = "drop")
q33 <- quantile(pre$pre_pct_gang, 1/3, na.rm = TRUE)
q67 <- quantile(pre$pre_pct_gang, 2/3, na.rm = TRUE)
pre <- pre |> mutate(group = case_when(
  pre_pct_gang <= q33 ~ "Control",
  pre_pct_gang >= q67 ~ "Treated",
  TRUE ~ "Middle"),
  name_up = toupper(trimws(community_area_name)))
stopifnot(sum(pre$group == "Treated") == 24, sum(pre$group == "Control") == 24)

# --- 2. Vitimas (nivel individual), join por NOME ---------------------------
v <- read_csv(file.path(INT, "shootings_age_sex_2008_2024.csv"),
              show_col_types = FALSE, guess_max = 60000)
stopifnot(all(c("community_area", "year", "sex", "age") %in% names(v)))

v <- v |>
  mutate(name_up = toupper(trimws(community_area))) |>
  left_join(pre |> select(name_up, group), by = "name_up")

n_unmatched <- sum(is.na(v$group) & !is.na(v$name_up) & v$name_up != "")
match_rate <- mean(!is.na(v$group))
cat(sprintf("Join por nome: %.1f%% das vitimas casadas a alguma CA do painel\n", 100 * match_rate))
un <- v |> filter(is.na(group)) |> count(name_up, sort = TRUE) |> head(5)
if (nrow(un) > 0) { cat("Top nomes sem match (esperado: CAs do tercil do meio ausentes do 'pre'? NAO — 'pre' tem as 77):\n"); print(un) }
stopifnot(match_rate > 0.95)   # painel tem as 77 CAs, quase tudo deve casar

v <- v |>
  filter(group %in% c("Treated", "Control"),
         year >= 2010, year <= 2024, year != 2015,
         sex %in% c("F", "M"))

# --- 3. Fidelidade: contagem feminina tratada pre bate com o painel? --------
chk_raw <- v |> filter(sex == "F", group == "Treated", year <= 2014) |> nrow()
chk_panel <- panel |>
  mutate(name_up = toupper(trimws(community_area_name))) |>
  left_join(pre |> select(name_up, group), by = "name_up") |>
  filter(group == "Treated", year >= 2010, year <= 2014) |>
  summarise(n = sum(n_fem_shoot)) |> pull(n)
cat(sprintf("Check fidelidade (fem, treated, pre): raw=%d painel=%d\n", chk_raw, chk_panel))
stopifnot(abs(chk_raw - chk_panel) <= 0.02 * chk_panel)

# --- 4. Perfil etario (faixas nativas) ---------------------------------------
v <- v |> mutate(period = ifelse(year <= 2014, "pre", "post"))
age_levels <- c("0-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
va <- v |> filter(age %in% age_levels) |>
  mutate(band = factor(age, levels = age_levels),
         young = band %in% c("0-19", "20-29"))
n_age_unk <- nrow(v) - nrow(va)
cat(sprintf("Vitimas na amostra DiD: %d | idade UNKNOWN/ausente: %d (%.1f%%)\n",
            nrow(v), n_age_unk, 100 * n_age_unk / nrow(v)))

prof <- va |>
  count(group, sex, period, band, .drop = FALSE) |>
  group_by(group, sex, period) |>
  mutate(share = n / sum(n), total = sum(n)) |>
  ungroup()

young_sum <- va |>
  group_by(group, sex, period) |>
  summarise(share_young = mean(young), n = n(), .groups = "drop")

# Qui-quadrado: perfil etario fem TRATADA pre vs post (e benchmark masculino)
chi_of <- function(sx) {
  tb <- va |> filter(group == "Treated", sex == sx) |>
    count(period, band, .drop = FALSE) |>
    pivot_wider(names_from = band, values_from = n, values_fill = 0)
  m <- as.matrix(tb[, -1]); rownames(m) <- tb$period
  m <- m[, colSums(m) > 0, drop = FALSE]
  suppressWarnings(chisq.test(m))
}
chi_f <- chi_of("F"); chi_m <- chi_of("M")

cat("\n--- Share de vitimas jovens (0-29), amostra DiD ---\n")
print(as.data.frame(young_sum |> arrange(group, sex, desc(period))), digits = 3)
cat("\n--- Shares por faixa (Treated) ---\n")
print(as.data.frame(prof |> filter(group == "Treated") |>
        select(sex, period, band, n, share) |> arrange(sex, desc(period), band)), digits = 3)
cat(sprintf("\nQui-quadrado FEM treated pre vs post: X2=%.2f df=%d p=%.4f\n",
            chi_f$statistic, chi_f$parameter, chi_f$p.value))
cat(sprintf("Qui-quadrado MASC treated pre vs post: X2=%.2f df=%d p=%.4f\n",
            chi_m$statistic, chi_m$parameter, chi_m$p.value))

out <- bind_rows(
  prof |> mutate(stat = "band_share", band = as.character(band)) |>
    rename(value = share) |> select(stat, group, sex, period, band, n, value, total),
  young_sum |> mutate(stat = "share_young_0_29", band = "0-29", total = n) |>
    rename(value = share_young) |> select(stat, group, sex, period, band, n, value, total)
) |>
  mutate(chi2_fem_treated = unname(chi_f$statistic), p_fem_treated = chi_f$p.value,
         chi2_masc_treated = unname(chi_m$statistic), p_masc_treated = chi_m$p.value,
         notes = sprintf(paste0("Amostra DiD 24+24 CAs, 2010-2024 ex-2015, join por nome de CA. ",
                                "Idade em faixas nativas. UNKNOWN excluido do perfil: %d obs."), n_age_unk))

write_csv(out, file.path(RES, "reforma_age_profile_fem.csv"))
cat("\nSalvo: results/reforma_age_profile_fem.csv\n")
