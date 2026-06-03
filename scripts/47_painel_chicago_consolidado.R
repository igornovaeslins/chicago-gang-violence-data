# ============================================================================
# 47_painel_chicago_consolidado.R
#
# Builds the consolidated community-area x year panel by joining:
#   1. Base panel (chicago_vcm_community_area_year.csv): DV, homicides,
#      arrests, gang_dominant, n_gangs_present, pct_area_gang_control
#   2. ACS demographic controls interpolated to CA x year (chicago_acs_ca_year.csv)
#   3. Shooting-victim decomposition (residence / public / fatal; by sex)
#   4. DV-crime decomposition (residence / public; arrest flag)
#   5. Diagnostics:
#      - distribution of switches in gang_dominant (motivates modal assignment)
#      - record count per year (motivates dropping 2015)
#      - imputation flag for 2011/2013 (nearest-year map fallback)
#
# Inputs : data/intermediate/, data/processed/
# Outputs:
#   data/processed/chicago_painel_completo.csv
#   results/diagnostico_gang_switching_chicago.csv
#   results/diagnostico_n_por_ano_chicago.csv
#   results/diagnostico_2011_2013_imputacao.csv
# ============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
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
cat("47_painel_chicago_consolidado.R\n\n")

# ---------------------------------------------------------------------------
# 1. Painel base
# ---------------------------------------------------------------------------
base <- read_csv(file.path(INT, "chicago_vcm_community_area_year.csv"),
                 show_col_types = FALSE) |>
  mutate(community_area = as.integer(community_area),
         year = as.integer(year))

cat("Painel base:", nrow(base), "linhas |",
    length(unique(base$community_area)), "CAs |",
    paste(range(base$year), collapse = "–"), "\n")

# ---------------------------------------------------------------------------
# 2. Diagnostic: switches in gang_dominant over time
# ---------------------------------------------------------------------------
sw <- base |>
  filter(!is.na(gang_dominant)) |>
  arrange(community_area, year) |>
  group_by(community_area, community_area_name) |>
  summarise(
    n_anos_observados   = n(),
    gangs_distintas     = n_distinct(gang_dominant),
    n_switches          = sum(gang_dominant != lag(gang_dominant), na.rm = TRUE),
    gang_modal          = names(which.max(table(gang_dominant))),
    pct_anos_modal      = max(table(gang_dominant)) / n(),
    .groups = "drop"
  )

dist_switches <- sw |>
  count(n_switches, name = "n_CAs") |>
  mutate(pct_CAs = n_CAs / sum(n_CAs))

cat("\n--- diagnostic: switches in gang_dominant ---\n")
print(dist_switches)
write_csv(sw,            file.path(RES, "diagnostico_gang_switching_chicago.csv"))
write_csv(dist_switches, file.path(RES, "diagnostico_switches_distribuicao.csv"))

# Community areas with at least one switch (the event-study estimation sample)
ca_event_eligible <- sw |> filter(n_switches >= 1) |> pull(community_area)
cat("CAs elegíveis para event study (com >=1 switch):",
    length(ca_event_eligible), "de", nrow(sw), "\n")

# ---------------------------------------------------------------------------
# 3. Diagnostic: record count per year (motivates dropping 2015)
# ---------------------------------------------------------------------------
n_ano <- base |>
  group_by(year) |>
  summarise(
    n_CAs = n_distinct(community_area),
    n_dv_total = sum(n_dv_crimes, na.rm = TRUE),
    n_homicidios_total = sum(n_homicides, na.rm = TRUE),
    n_arrests_total = sum(n_arrests, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n--- diagnostic: record count per year ---\n")
print(n_ano)
write_csv(n_ano, file.path(RES, "diagnostico_n_por_ano_chicago.csv"))

# Quão anômalo é 2015?
med_dv <- median(n_ano$n_dv_total[n_ano$year != 2015])
dv_2015 <- n_ano$n_dv_total[n_ano$year == 2015]
cat(sprintf("DV em 2015: %d | Mediana outros anos: %.0f | Razão: %.2f\n",
            dv_2015, med_dv, dv_2015 / med_dv))

# ---------------------------------------------------------------------------
# 4. Decomposição de shootings (residence vs public vs fatal)
# ---------------------------------------------------------------------------
# Lookup name → numeric ID (a partir do painel base)
ca_lookup <- base |>
  distinct(community_area_name, community_area) |>
  mutate(ca_name_upper = toupper(trimws(as.character(community_area_name))))

sh <- read_csv(file.path(INT, "shootings_with_gang_territory_2008_2024.csv"),
               show_col_types = FALSE) |>
  mutate(ca_name_upper = toupper(trimws(as.character(community_area))),
         year = suppressWarnings(as.integer(year)),
         sex = toupper(trimws(as.character(sex))),
         loc = toupper(as.character(location_description)),
         vprim = toupper(as.character(victimization_primary)),
         iprim = toupper(as.character(incident_primary))) |>
  select(-community_area) |>
  left_join(ca_lookup |> select(ca_name_upper, community_area), by = "ca_name_upper") |>
  filter(!is.na(community_area), !is.na(year)) |>
  mutate(
    is_F            = as.integer(sex == "F"),
    loc_residence   = as.integer(str_detect(loc, "RESIDENCE|APARTMENT|HOUSE|HOME|YARD|PORCH")),
    loc_public      = as.integer(str_detect(loc, "STREET|SIDEWALK|ALLEY|PARK|VEHICLE|HIGHWAY")),
    fatal           = as.integer(str_detect(vprim, "FATAL|HOMICIDE|MURDER") |
                                 str_detect(iprim, "FATAL|HOMICIDE|MURDER"))
  )

sh_agg <- sh |>
  group_by(community_area, year) |>
  summarise(
    n_shoot_total       = n(),
    n_fem_shoot         = sum(is_F, na.rm = TRUE),
    n_fem_shoot_res     = sum(is_F == 1 & loc_residence == 1, na.rm = TRUE),
    n_fem_shoot_pub     = sum(is_F == 1 & loc_public    == 1, na.rm = TRUE),
    n_fem_shoot_fatal   = sum(is_F == 1 & fatal == 1, na.rm = TRUE),
    n_male_shoot        = sum(is_F == 0, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n--- Shootings agregados (CA × ano) ---\n")
cat("Linhas:", nrow(sh_agg), "| Total female shootings:",
    sum(sh_agg$n_fem_shoot), "\n")

# ---------------------------------------------------------------------------
# 5. Decomposição de DV crimes (residence vs public; arrest flag)
# ---------------------------------------------------------------------------
dv <- read_csv(file.path(INT, "chicago_dv_2008_2024.csv"),
               show_col_types = FALSE,
               col_select = c(community_area, date, location_description,
                              primary_type, arrest, year = any_of(c("year")))) |>
  mutate(community_area = suppressWarnings(as.integer(community_area)))

# Alguns CSVs não têm coluna 'year' explícita; derivar de date se necessário
if (!"year" %in% names(dv)) {
  dv$year <- suppressWarnings(as.integer(substr(as.character(dv$date), 1, 4)))
}
dv$year <- suppressWarnings(as.integer(dv$year))

dv <- dv |>
  filter(!is.na(community_area), !is.na(year), year >= 2008, year <= 2024) |>
  mutate(
    loc = toupper(as.character(location_description)),
    ptype = toupper(as.character(primary_type)),
    loc_residence = as.integer(str_detect(loc, "RESIDENCE|APARTMENT|HOUSE|HOME|YARD|PORCH")),
    loc_public    = as.integer(str_detect(loc, "STREET|SIDEWALK|ALLEY|PARK|VEHICLE|HIGHWAY")),
    is_dv_homicide = as.integer(str_detect(ptype, "HOMICIDE")),
    arrested      = as.integer(arrest == TRUE | arrest == "true" | arrest == "True")
  )

dv_agg <- dv |>
  group_by(community_area, year) |>
  summarise(
    n_dv_check       = n(),
    n_dv_res         = sum(loc_residence, na.rm = TRUE),
    n_dv_pub         = sum(loc_public,    na.rm = TRUE),
    n_dv_homicide    = sum(is_dv_homicide, na.rm = TRUE),
    n_dv_arrested    = sum(arrested,      na.rm = TRUE),
    .groups = "drop"
  )

cat("\n--- DV agregado (CA × ano) ---\n")
cat("Linhas:", nrow(dv_agg), "| Total DV:", sum(dv_agg$n_dv_check),
    "| Residência:", sum(dv_agg$n_dv_res),
    "| Público:", sum(dv_agg$n_dv_pub),
    "| Arrest:", sum(dv_agg$n_dv_arrested), "\n")

# ---------------------------------------------------------------------------
# 6. Flag de imputação para anos sem mapa (2011, 2013)
# ---------------------------------------------------------------------------
# Anos com mapa CLEARMAP direto: 2008,2009,2010,2012,2014-2024
anos_com_mapa <- c(2008:2010, 2012, 2014:2024)
base <- base |>
  mutate(
    gang_map_year_used = case_when(
      year == 2011 ~ 2010L,
      year == 2013 ~ 2012L,
      TRUE         ~ as.integer(year)
    ),
    imputed_gang_map = as.integer(!(year %in% anos_com_mapa))
  )

cobertura_imp <- base |>
  group_by(year, imputed_gang_map, gang_map_year_used) |>
  summarise(n_CAs = n_distinct(community_area), .groups = "drop")

write_csv(cobertura_imp, file.path(RES, "diagnostico_2011_2013_imputacao.csv"))

# ---------------------------------------------------------------------------
# 7. Merge com ACS panel
# ---------------------------------------------------------------------------
acs <- read_csv(file.path(PROC, "chicago_acs_ca_year.csv"), show_col_types = FALSE) |>
  rename(community_area = ca_num) |>
  select(-ca_name)

cat("\n--- ACS panel ---\n")
cat("Linhas:", nrow(acs), "| Vars:", paste(setdiff(names(acs), c("community_area","year")),
                                            collapse = ", "), "\n")

# ---------------------------------------------------------------------------
# 8. Painel consolidado
# ---------------------------------------------------------------------------
painel <- base |>
  left_join(sh_agg, by = c("community_area", "year")) |>
  left_join(dv_agg, by = c("community_area", "year")) |>
  left_join(acs,    by = c("community_area", "year")) |>
  mutate(
    across(c(n_shoot_total, n_fem_shoot, n_fem_shoot_res, n_fem_shoot_pub,
             n_fem_shoot_fatal, n_male_shoot, n_dv_res, n_dv_pub,
             n_dv_homicide, n_dv_arrested),
           ~ replace_na(., 0L)),
    # Sanidade: n_dv_crimes (do painel base) vs n_dv_check (agregado direto)
    dv_consistency_ratio = ifelse(n_dv_crimes > 0,
                                  n_dv_check / n_dv_crimes, NA_real_),
    # Outcomes derivados
    arrest_rate_dv = ifelse(n_dv_crimes > 0, n_dv_arrested / n_dv_crimes, NA_real_),
    dv_per_100k    = ifelse(total_pop > 0, n_dv_crimes / total_pop * 1e5, NA_real_),
    fem_shoot_per_100k_female = ifelse(pop_female > 0,
                                       n_fem_shoot / pop_female * 1e5, NA_real_),
    # Gang-family grouping (Folk Nation / People Nation)
    gang_family = case_when(
      str_detect(toupper(gang_dominant),
                 "GANGSTER DISCIPLES|BLACK DISCIPLES|NEW BREED|SATAN DISCIPLES|MANIAC LATIN DISCIPLES|TWO[ -]?SIX|SPANISH COBRAS|IMPERIAL GANGSTERS|LA FAMILIA STONES|BLACK SOULS") ~ "Folk Nation",
      str_detect(toupper(gang_dominant),
                 "LATIN KINGS|BLACK P STONE|FOUR CORNER HUSTLERS|TRAVELING VICE LORDS|MICKEY COBRAS|LATIN COUNTS|LATIN DRAGONS|SPANISH FOUR CORNER HUSTLERS") ~ "People Nation",
      is.na(gang_dominant) ~ NA_character_,
      TRUE ~ "Other/Unaffiliated"
    )
  )

cat("\nPainel consolidado:\n")
cat("  Linhas:", nrow(painel), "| Cols:", ncol(painel), "\n")
cat("  Vars principais:", paste(head(names(painel), 12), collapse = ", "), "...\n")

# Verificação de consistência DV
inconsistencias <- painel |>
  filter(!is.na(dv_consistency_ratio),
         abs(dv_consistency_ratio - 1) > 0.05) |>
  nrow()
cat("  Inconsistências DV (>5% diff entre fontes):", inconsistencias, "/",
    nrow(painel), "\n")

# Distribuição gang_family
cat("\nDistribuição gang_family (CA × ano):\n")
print(table(painel$gang_family, useNA = "ifany"))

write_csv(painel, file.path(PROC, "chicago_painel_completo.csv"))
cat("\nPainel salvo em:", file.path(PROC, "chicago_painel_completo.csv"), "\n")

cat("\n==============================================================\n")
cat("DIAGNOSTICS WRITTEN:\n")
cat("  results/diagnostico_gang_switching_chicago.csv    (por CA)\n")
cat("  results/diagnostico_switches_distribuicao.csv      (resumo)\n")
cat("  results/diagnostico_n_por_ano_chicago.csv          (N por ano)\n")
cat("  results/diagnostico_2011_2013_imputacao.csv        (mapa imputado)\n")
cat("==============================================================\n")
