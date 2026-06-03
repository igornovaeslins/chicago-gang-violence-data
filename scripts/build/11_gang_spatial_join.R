# ============================================================================
# build/11_gang_spatial_join.R
#
# Treatment construction. Builds the gang-territory panel and assigns each
# crime/shooting record to a gang territory by point-in-polygon, then
# aggregates to the community-area x year panel the analysis reads.
#
# This is the load-bearing step: it defines gang_dominant, n_gangs_present,
# and pct_area_gang_control, and it produces shootings_with_gang_territory.
#
# Inputs (require the frozen raw data — see scripts/build/README.md):
#   data/raw/chicago/boundaries/gang_territories/gang_boundaries_YYYY.geojson
#   data/raw/chicago/boundaries/chicago_community_areas.geojson
#   data/intermediate/chicago_dv_2008_2024.csv         (from 07_consolidate)
#   data/intermediate/chicago_shootings_2008_2024.csv  (from 07_consolidate)
#   data/intermediate/chicago_homicides_2008_2024.csv  (from 07_consolidate)
#   data/raw/chicago/police_operations/arrests_2008_2024.csv
#
# Outputs (data/intermediate/):
#   gang_boundaries_panel_2007_2024.csv
#   gang_control_by_community_area_year.csv
#   dv_crimes_with_gang_territory_2008_2024.csv
#   shootings_with_gang_territory_2008_2024.csv   (treatment join, victim-level)
#   chicago_vcm_community_area_year.csv            (the analysis panel base)
#
# Method: areas computed in EPSG:26916 (UTM 16N); point-in-polygon uses the
# WGS84 (EPSG:4326) coordinates with the "within" predicate; gang maps are
# missing for 2011 and 2013, filled by the nearest available year (2010, 2012).
#
# NOTE: this script needs the raw downloads and is NOT part of the offline
# analysis path. The analysis (scripts/47..56) runs from the shipped
# intermediate files without it.
# ============================================================================

# ---- portable paths -------------------------------------------------------
.this_file <- tryCatch(
  normalizePath(sub("^--file=", "",
    grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1])),
  error = function(e) NA_character_)
ROOT <- if (!is.na(.this_file)) dirname(dirname(dirname(.this_file))) else normalizePath(".")
RAW  <- file.path(ROOT, "data", "raw", "chicago")
INT  <- file.path(ROOT, "data", "intermediate")
LOG  <- file.path(ROOT, "logs")
dir.create(INT, showWarnings = FALSE, recursive = TRUE)
dir.create(LOG, showWarnings = FALSE, recursive = TRUE)
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(sf); library(dplyr); library(tidyr); library(readr)
  library(stringr); library(purrr)
})
sf::sf_use_s2(FALSE)   # planar overlay/within, matches the original GEOS-based build

UTM  <- 26916          # EPSG:26916, UTM Zone 16N (area calculations)
WGS  <- 4326           # EPSG:4326, WGS84 (point-in-polygon)
GANG_DIR <- file.path(RAW, "boundaries", "gang_territories")
CA_FILE  <- file.path(RAW, "boundaries", "chicago_community_areas.geojson")

cat("==============================================================\n")
cat("build/11_gang_spatial_join.R\n\n")

# ---------------------------------------------------------------------------
# Community areas: id <-> name, and per-CA area in UTM
# ---------------------------------------------------------------------------
ca <- st_read(CA_FILE, quiet = TRUE) |> st_make_valid() |>
  transmute(community_area_id   = as.integer(area_numbe),
            community_area_name  = str_squish(toupper(community)),
            geometry)
ca_id_to_name <- setNames(ca$community_area_name, ca$community_area_id)
ca_utm  <- st_transform(ca, UTM)
ca_area <- setNames(as.numeric(st_area(ca_utm)), ca_utm$community_area_id)

# ---------------------------------------------------------------------------
# 1. Gang-boundary panel (all mapped years stacked), area in km2 (UTM)
# ---------------------------------------------------------------------------
gang_files <- list.files(GANG_DIR, pattern = "^gang_boundaries_\\d{4}\\.geojson$",
                         full.names = TRUE)
read_gang_year <- function(f) {
  yr <- as.integer(str_extract(basename(f), "\\d{4}"))
  g  <- st_read(f, quiet = TRUE) |> st_make_valid()
  g$GANG_NAME <- str_squish(toupper(as.character(g$GANG_NAME)))
  g$year <- yr
  g[, c("GANG_NAME", "year", "geometry")]
}
gang_panel <- do.call(rbind, lapply(gang_files, read_gang_year))
st_crs(gang_panel) <- WGS
gp_utm <- st_transform(gang_panel, UTM)
gang_panel$area_km2 <- as.numeric(st_area(gp_utm)) / 1e6
write_csv(st_drop_geometry(gang_panel),
          file.path(INT, "gang_boundaries_panel_2007_2024.csv"))
available_years <- sort(unique(gang_panel$year))
cat("Gang panel:", nrow(gang_panel), "territory-years |",
    length(available_years), "mapped years\n")

# nearest available gang-map year (ties resolve to the earlier year, matching
# Python's min() over a sorted vector: 2011 -> 2010, 2013 -> 2012)
nearest_year <- function(y) available_years[which.min(abs(available_years - y))]

# ---------------------------------------------------------------------------
# 2. Gang control by community area x year (area-weighted)
# ---------------------------------------------------------------------------
control_rows <- list()
for (yr in available_years) {
  gy <- gp_utm[gp_utm$year == yr, c("GANG_NAME", "geometry")]
  if (nrow(gy) == 0) next
  inter <- suppressWarnings(st_intersection(ca_utm[, c("community_area_id",
                                                       "community_area_name")], gy))
  if (nrow(inter) == 0) next
  inter$ia <- as.numeric(st_area(inter))
  d <- st_drop_geometry(inter) |>
    mutate(ca_area = ca_area[as.character(community_area_id)],
           pct_area = ia / ca_area)
  agg <- d |> group_by(community_area_id, community_area_name) |>
    summarise(
      gang_dominant         = GANG_NAME[which.max(pct_area)],
      n_gangs_present       = n_distinct(GANG_NAME),
      pct_area_gang_control = min(sum(pct_area), 1.0),
      .groups = "drop") |>
    mutate(year = yr)
  control_rows[[as.character(yr)]] <- agg
}
gang_ca <- bind_rows(control_rows)
write_csv(gang_ca, file.path(INT, "gang_control_by_community_area_year.csv"))
cat("Gang control rows:", nrow(gang_ca), "\n")

# ---------------------------------------------------------------------------
# 3. Point-in-polygon join of crime/shooting records to gang territory
# ---------------------------------------------------------------------------
join_to_gangs <- function(df, lat = "latitude", lon = "longitude", yrcol = "year") {
  df$.row <- seq_len(nrow(df))
  has_xy <- !is.na(df[[lat]]) & !is.na(df[[lon]])
  out_terr <- rep("No Coordinates", nrow(df))
  out_gyr  <- rep(NA_integer_, nrow(df))
  valid <- df[has_xy, , drop = FALSE]
  for (cy in sort(unique(valid[[yrcol]]))) {
    gy_year <- nearest_year(cy)
    sub <- valid[valid[[yrcol]] == cy, , drop = FALSE]
    pts <- st_as_sf(sub, coords = c(lon, lat), crs = WGS, remove = FALSE)
    polys <- gang_panel[gang_panel$year == gy_year, c("GANG_NAME", "geometry")]
    jx <- st_within(pts, polys)                 # list: indices of containing polys
    first <- vapply(jx, function(i) if (length(i)) i[1] else NA_integer_, integer(1))
    terr <- ifelse(is.na(first), "No Gang Territory", polys$GANG_NAME[first])
    out_terr[sub$.row] <- terr
    out_gyr[sub$.row]  <- gy_year
  }
  df$gang_territory <- out_terr
  df$gang_data_year <- out_gyr
  df$.row <- NULL
  df
}

# ---- DV crimes ----
dv <- read_csv(file.path(INT, "chicago_dv_2008_2024.csv"), show_col_types = FALSE)
dv <- join_to_gangs(dv)
write_csv(dv, file.path(INT, "dv_crimes_with_gang_territory_2008_2024.csv"))
cat("DV joined:", nrow(dv), "| coverage",
    sprintf("%.1f%%", 100*mean(dv$gang_territory[dv$gang_territory != "No Coordinates"]
                               != "No Gang Territory")), "\n")

# ---- shooting victims ----
sh <- read_csv(file.path(INT, "chicago_shootings_2008_2024.csv"), show_col_types = FALSE)
sh <- join_to_gangs(sh)
# Write a privacy-minimized public file: only the columns the analysis reads,
# plus coarse administrative units and the gang-join provenance. The victim
# name, exact coordinates, record IDs, minute timestamp, age, and race are
# dropped (the analysis is at the community-area x year level and uses none of
# them). The full in-memory `sh` is still used for the aggregation below.
sh_public_cols <- c("year", "community_area", "sex", "location_description",
                    "victimization_primary", "incident_primary",
                    "ward", "area", "district", "beat",
                    "gang_territory", "gang_data_year")
write_csv(sh[, intersect(sh_public_cols, names(sh))],
          file.path(INT, "shootings_with_gang_territory_2008_2024.csv"))
cat("Shootings joined:", nrow(sh), "\n")

# ---------------------------------------------------------------------------
# 4. Aggregate to community-area x year panel
#    (community_area in the shooting file is the NAME; map it to id via ca lookup
#     — this is the fix for the silent type-coercion in the original builder,
#     and it leaves every analysis-relevant column identical to the shipped file.)
# ---------------------------------------------------------------------------
hom <- read_csv(file.path(INT, "chicago_homicides_2008_2024.csv"), show_col_types = FALSE) |>
  mutate(community_area = suppressWarnings(as.integer(community_area)))

dv_id <- dv |> mutate(community_area = suppressWarnings(as.integer(community_area)))
dv_agg  <- dv_id  |> filter(!is.na(community_area)) |>
  count(community_area, year, name = "n_dv_crimes")
hom_agg <- hom    |> filter(!is.na(community_area)) |>
  count(community_area, year, name = "n_homicides")

name_to_id <- setNames(ca$community_area_id, ca$community_area_name)
sh_id <- sh |> mutate(community_area = unname(name_to_id[str_squish(toupper(community_area))]))
sh_agg  <- sh_id |> filter(!is.na(community_area)) |>
  count(community_area, year, name = "n_shootings")
fem_agg <- sh_id |> filter(!is.na(community_area), toupper(trimws(sex)) == "F") |>
  count(community_area, year, name = "n_female_victims")

# arrests linked to DV via case_number, inheriting the DV community area
arr <- read_csv(file.path(RAW, "police_operations", "arrests_2008_2024.csv"),
                show_col_types = FALSE)
arr$year <- as.integer(format(as.Date(substr(as.character(arr$arrest_date), 1, 10)), "%Y"))
dv_key <- dv_id |> select(case_number, community_area) |>
  filter(!is.na(case_number), !is.na(community_area)) |> distinct(case_number, .keep_all = TRUE)
arr_agg <- arr |> inner_join(dv_key, by = "case_number") |>
  filter(!is.na(community_area)) |> count(community_area, year, name = "n_arrests")

all_years <- sort(union(dv_agg$year, hom_agg$year))
base <- expand_grid(community_area = sort(unique(ca$community_area_id)), year = all_years) |>
  mutate(community_area_name = ca_id_to_name[as.character(community_area)]) |>
  left_join(dv_agg,  by = c("community_area", "year")) |>
  left_join(hom_agg, by = c("community_area", "year")) |>
  left_join(sh_agg,  by = c("community_area", "year")) |>
  left_join(fem_agg, by = c("community_area", "year")) |>
  left_join(arr_agg, by = c("community_area", "year")) |>
  left_join(gang_ca |> select(community_area = community_area_id, year,
                              gang_dominant, n_gangs_present, pct_area_gang_control),
            by = c("community_area", "year")) |>
  mutate(across(c(n_dv_crimes, n_homicides, n_shootings, n_female_victims, n_arrests),
                ~ as.integer(replace_na(., 0)))) |>
  select(community_area_name, community_area, year,
         gang_dominant, n_gangs_present, pct_area_gang_control,
         n_dv_crimes, n_homicides, n_shootings, n_female_victims, n_arrests)

write_csv(base, file.path(INT, "chicago_vcm_community_area_year.csv"))
cat("VCM panel:", nrow(base), "rows |",
    n_distinct(base$community_area), "CAs |",
    paste(range(base$year), collapse = "-"), "\n")
cat("\nDONE.\n")
