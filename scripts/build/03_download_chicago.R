# ============================================================================
# 03_download_chicago.R
#
# Downloads the raw Chicago inputs from the City of Chicago open-data portal
# (data.cityofchicago.org) via the Socrata API.
#
# Datasets fetched:
#   - Crimes flagged as domestic violence (dataset ijzp-q8t2)
#   - Homicides (dataset ijzp-q8t2, primary_type = HOMICIDE)
#   - Shooting victims (Violence Reduction dataset gumc-mgzr)
#   - Community-area boundaries (cauq-8yn6)
#   - Police-district boundaries (fthy-xz3r)
#   - Police-beat boundaries (aerh-rz74)
#   - Public-health statistics by community area (iqnk-2tcu)
#
# Inputs : none (pulls directly from the portal).
# Outputs: raw files written under data/raw/chicago/, plus a download
#          summary and log under logs/.
#
# Requires an internet connection. This is a one-time data-acquisition step
# and is NOT part of the offline analysis path; the raw files it produces are
# not shipped with the package and must be re-downloaded to rebuild the
# intermediate data.
# ============================================================================

# ---- portable paths (replication package) -------------------------------
.this_file <- tryCatch(
  normalizePath(sub("^--file=", "",
    grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1])),
  error = function(e) NA_character_)
ROOT <- if (!is.na(.this_file)) dirname(dirname(dirname(.this_file))) else normalizePath(".")
# -------------------------------------------------------------------------

# Dependencies
suppressPackageStartupMessages({
  library(httr)
})

# ── Paths ───────────────────────────────────────────────────────────────────
DATA_DIR <- file.path(ROOT, "data", "raw", "chicago")
LOG_DIR  <- file.path(ROOT, "logs")

dir.create(LOG_DIR,  recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(DATA_DIR, "crimes"),    recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(DATA_DIR, "shootings"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(DATA_DIR, "boundaries"),recursive = TRUE, showWarnings = FALSE)

LOG_FILE <- file.path(LOG_DIR, "download_chicago.log")

# Simple logger (timestamp + message, written to both console and file)
log_msg <- function(...) {
  msg <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " -- ", paste0(...))
  message(msg)
  cat(msg, "\n", file = LOG_FILE, append = TRUE)
}

# ── Dataset definitions ───────────────────────────────────────────────────────
SOCRATA_BASE <- "https://data.cityofchicago.org/resource"

DATASETS <- list(
  crimes_domestic_violence = list(
    endpoint = paste0(SOCRATA_BASE, "/ijzp-q8t2.csv"),
    params   = list(`$where` = "domestic='true'", `$limit` = 500000L, `$order` = "date DESC"),
    out      = file.path(DATA_DIR, "crimes", "chicago_domestic_violence_2001_present.csv"),
    desc     = "Crimes with domestic-violence flag"
  ),
  crimes_homicide = list(
    endpoint = paste0(SOCRATA_BASE, "/ijzp-q8t2.csv"),
    params   = list(`$where` = "primary_type='HOMICIDE'", `$limit` = 100000L, `$order` = "date DESC"),
    out      = file.path(DATA_DIR, "crimes", "chicago_homicides_2001_present.csv"),
    desc     = "Homicides recorded by CPD"
  ),
  shooting_victims = list(
    endpoint = paste0(SOCRATA_BASE, "/gumc-mgzr.csv"),
    params   = list(`$limit` = 500000L),
    out      = file.path(DATA_DIR, "shootings", "chicago_shooting_victims.csv"),
    desc     = "Shooting victims (CPD Violence Reduction)"
  ),
  community_areas = list(
    endpoint = paste0(SOCRATA_BASE, "/cauq-8yn6.geojson"),
    params   = list(),
    out      = file.path(DATA_DIR, "boundaries", "chicago_community_areas.geojson"),
    desc     = "Chicago community-area boundaries"
  ),
  police_districts = list(
    endpoint = paste0(SOCRATA_BASE, "/fthy-xz3r.geojson"),
    params   = list(),
    out      = file.path(DATA_DIR, "boundaries", "chicago_police_districts.geojson"),
    desc     = "Chicago police-district boundaries"
  ),
  police_beats = list(
    endpoint = paste0(SOCRATA_BASE, "/aerh-rz74.geojson"),
    params   = list(),
    out      = file.path(DATA_DIR, "boundaries", "chicago_police_beats.geojson"),
    desc     = "Chicago police-beat boundaries"
  ),
  public_health_community = list(
    endpoint = paste0(SOCRATA_BASE, "/iqnk-2tcu.csv"),
    params   = list(`$limit` = 10000L),
    out      = file.path(DATA_DIR, "crimes", "chicago_public_health_community.csv"),
    desc     = "Public-health statistics by community area"
  )
)

# ── Download function ─────────────────────────────────────────────────────────
download_dataset <- function(name, config) {
  out_path <- config$out

  # Make sure the parent directory exists
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

  # Skip if already present
  if (file.exists(out_path)) {
    size_kb <- file.info(out_path)$size / 1024
    log_msg("  Already exists: ", basename(out_path), " (", round(size_kb, 1), " KB)")
    return(data.frame(dataset = name, status = "already exists",
                      tamanho_kb = round(size_kb, 1), stringsAsFactors = FALSE))
  }

  log_msg("Downloading: ", name, " -- ", config$desc)
  log_msg("  URL: ", config$endpoint)

  result <- tryCatch({
    resp <- httr::GET(
      url     = config$endpoint,
      query   = if (length(config$params) > 0) config$params else NULL,
      httr::add_headers(Accept = "application/json"),
      httr::timeout(300)
    )

    httr::stop_for_status(resp)

    writeBin(httr::content(resp, as = "raw"), out_path)

    size_kb <- file.info(out_path)$size / 1024
    log_msg("  OK -- ", round(size_kb, 1), " KB")
    data.frame(dataset = name, status = "OK",
               tamanho_kb = round(size_kb, 1), stringsAsFactors = FALSE)

  }, error = function(e) {
    log_msg("  ERROR: ", conditionMessage(e))
    data.frame(dataset = name, status = paste0("ERROR: ", conditionMessage(e)),
               tamanho_kb = 0, stringsAsFactors = FALSE)
  })

  return(result)
}

# ── Main ──────────────────────────────────────────────────────────────────────
log_msg("=== Download Chicago Data Portal ===")

results <- vector("list", length(DATASETS))
for (i in seq_along(DATASETS)) {
  name   <- names(DATASETS)[i]
  config <- DATASETS[[i]]
  results[[i]] <- download_dataset(name, config)
  Sys.sleep(1)  # respect rate limit
}

# Save summary
summary_df <- do.call(rbind, results)
write.csv(summary_df,
          file.path(LOG_DIR, "download_chicago_summary.csv"),
          row.names = FALSE)

log_msg("\n=== Summary ===")
log_msg(paste(capture.output(print(summary_df)), collapse = "\n"))
log_msg("Chicago download complete.")
