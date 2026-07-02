# ============================================================================
# 59_sao_share_feminino_reus_arma.R
#
# FONTE NOVA (aprovada pelo Igor em 02/07/2026): Cook County State's Attorney,
# Felony Cases — Initiation (case-participant-charge level), Socrata
# datacatalog.cookcountyil.gov, dataset 7mck-ehwz.
#
# Pergunta (reforma G2, alternativa "papeis femininos de rua mudaram"):
# o share FEMININO entre reus de crime de arma em incidentes de Chicago
# subiu apos 2015? Se ficou plano enquanto o share feminino de VITIMAS de
# tiro subiu (~10% -> ~16%), a historia de envolvimento perde forca.
#
# Desenho:
#   - Unidade: reu (primary_charge = true, 1 linha por participante-caso)
#   - Filtros: incident_city = 'Chicago', gender em (Male, Female)
#   - Ano: date_extract_y(arrest_date)
#   - Series: (a) UUW - Unlawful Use of Weapon (porte; proxy de envolvimento
#     armado de rua), (b) conjunto DISPARO (Aggravated Battery With A Firearm,
#     Aggravated Discharge Firearm, Reckless Discharge of Firearm, Armed
#     Violence), (c) TODOS os felonies (benchmark)
#   - Agregacao server-side (SoQL); queries gravadas no CSV para reproducao
#
# LIMITES declarados: citywide (sem contraste tratado/controle); reus, nao
# autores (funil de enforcement); cobertura SAO comeca ~2011.
# Grava APENAS results/reforma_sao_gun_gender.csv (novo).
# ============================================================================


suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(jsonlite)
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
BASE <- "https://datacatalog.cookcountyil.gov/resource/7mck-ehwz.json"

soql_pull <- function(where_extra, label) {
  soql <- paste0(
    "$select=date_extract_y(arrest_date) as yr, gender, count(*) as n",
    "&$where=primary_charge = true AND incident_city = 'Chicago' AND ",
    "arrest_date IS NOT NULL AND gender in ('Male','Female')",
    where_extra,
    "&$group=yr, gender&$order=yr&$limit=5000"
  )
  url <- paste0(BASE, "?", URLencode(soql))
  df <- tryCatch(fromJSON(url), error = function(e) stop("API: ", conditionMessage(e)))
  stopifnot(nrow(df) > 0)
  df |>
    mutate(yr = as.integer(yr), n = as.integer(n), series = label,
           query = soql)
}

cat("==============================================================\n")
cat("59_sao_share_feminino_reus_arma.R — fonte: SAO Cook County (7mck-ehwz)\n\n")

uuw <- soql_pull(" AND offense_category = 'UUW - Unlawful Use of Weapon'", "UUW (porte)")
dis <- soql_pull(paste0(" AND offense_category in ('Aggravated Battery With A Firearm',",
                        "'Aggravated Discharge Firearm','Reckless Discharge of Firearm',",
                        "'Armed Violence')"), "Disparo/armed violence")
all <- soql_pull("", "Todos os felonies (benchmark)")

tab <- bind_rows(uuw, dis, all) |>
  filter(yr >= 2011, yr <= 2024) |>
  pivot_wider(names_from = gender, values_from = n, values_fill = 0L) |>
  mutate(total = Male + Female, fem_share = Female / total) |>
  arrange(series, yr)

# Asserts de sanidade
stopifnot(all(tab$total > 0))
stopifnot(all(tab$fem_share >= 0 & tab$fem_share <= 1))
n_years <- tab |> count(series)
stopifnot(all(n_years$n >= 12))                      # cobertura 2011-2024 quase completa
uuw_tot <- tab |> filter(series == "UUW (porte)") |> summarise(s = sum(total)) |> pull(s)
stopifnot(uuw_tot > 40000)   # ~47.8k reus com UUW como acusacao PRIMARIA (Chicago,
                             # arrest_date valido, 2011-2024); a maioria das acusacoes
                             # UUW e secundaria (168k) e nao conta reu aqui

# Comparacao pre/post 2015 (pre = 2011-2014, post = 2016-2024, 2015 fora,
# espelhando a janela do paper)
summ <- tab |>
  filter(yr != 2015) |>
  mutate(period = ifelse(yr <= 2014, "pre", "post")) |>
  group_by(series, period) |>
  summarise(Female = sum(Female), Male = sum(Male), .groups = "drop") |>
  mutate(total = Female + Male, fem_share = Female / total)

# Teste de proporcao pre vs post por serie
tests <- summ |>
  select(series, period, Female, total) |>
  pivot_wider(names_from = period, values_from = c(Female, total)) |>
  rowwise() |>
  mutate(pt = list(prop.test(c(Female_post, Female_pre), c(total_post, total_pre))),
         diff_pp = 100 * (Female_post / total_post - Female_pre / total_pre),
         p_value = pt$p.value) |>
  ungroup() |> select(series, diff_pp, p_value)

cat("--- Share feminino por serie e ano ---\n")
print(as.data.frame(tab |> select(series, yr, Female, Male, fem_share)), digits = 3)
cat("\n--- Pre (2011-2014) vs Post (2016-2024) ---\n")
print(as.data.frame(summ), digits = 3)
cat("\n--- Diferenca post-pre (pp) e prop.test ---\n")
print(as.data.frame(tests), digits = 3)

out <- tab |>
  left_join(summ |> select(series, period, fem_share_period = fem_share) |>
              pivot_wider(names_from = period, values_from = fem_share_period),
            by = "series") |>
  left_join(tests, by = "series") |>
  mutate(fonte = "Cook County SAO, Initiation (7mck-ehwz), acesso 2026-07-02",
         unidade = "reu (primary_charge=true), incident_city=Chicago, ano da prisao")

write_csv(out, file.path(RES, "reforma_sao_gun_gender.csv"))
cat("\nSalvo: results/reforma_sao_gun_gender.csv\n")

# ---------------------------------------------------------------------------
# Triangulacao: IDADE das res de arma (UUW) — as res sao jovens enquanto as
# vitimas femininas adicionais envelhecem? Populacoes divergentes = o
# envolvimento nao explica a vitimizacao.
# ---------------------------------------------------------------------------
soql_age <- paste0(
  "$select=date_extract_y(arrest_date) as yr, gender, age_at_incident, count(*) as n",
  "&$where=primary_charge = true AND incident_city = 'Chicago' AND ",
  "arrest_date IS NOT NULL AND gender in ('Male','Female') AND ",
  "age_at_incident IS NOT NULL AND ",
  "offense_category = 'UUW - Unlawful Use of Weapon'",
  "&$group=yr, gender, age_at_incident&$limit=20000"
)
ag <- fromJSON(paste0(BASE, "?", URLencode(soql_age))) |>
  mutate(yr = as.integer(yr), age = as.numeric(age_at_incident), n = as.integer(n)) |>
  filter(yr >= 2011, yr <= 2024, yr != 2015, age >= 10, age <= 90) |>
  mutate(period = ifelse(yr <= 2014, "pre", "post"))
stopifnot(nrow(ag) > 100)

age_summ <- ag |>
  rename(w = n) |>
  group_by(gender, period) |>
  summarise(
    mean_age = sum(age * w) / sum(w),
    share_young_0_29 = sum(w[age <= 29]) / sum(w),
    share_30_49 = sum(w[age >= 30 & age <= 49]) / sum(w),
    n = sum(w),
    .groups = "drop")
stopifnot(all(age_summ$mean_age > 15 & age_summ$mean_age < 60))

cat("\n--- Idade das/os reus de UUW (Chicago, primary charge) ---\n")
print(as.data.frame(age_summ |> arrange(gender, desc(period))), digits = 3)

write_csv(age_summ |>
            mutate(fonte = "Cook County SAO, Initiation (7mck-ehwz), acesso 2026-07-02",
                   query = soql_age),
          file.path(RES, "reforma_sao_uuw_age_gender.csv"))
cat("Salvo: results/reforma_sao_uuw_age_gender.csv\n")
