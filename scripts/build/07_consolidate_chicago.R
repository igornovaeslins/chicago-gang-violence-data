# ============================================================================
# 07_consolidate_chicago.R
#
# Consolidates and filters the raw Chicago crime inputs to the 2008–2024
# window. Reads the downloaded portal files, extracts the calendar year from
# each record's date, restricts to the target window, deduplicates, and writes
# year-filtered intermediate tables plus a temporal-coverage summary.
#
# Inputs : raw files under data/raw/chicago/ (produced by 03_download_chicago.R)
#            - crimes/chicago_domestic_violence_*.csv
#            - crimes/chicago_homicides_2001_present.csv
#            - shootings/chicago_shooting_victims.csv
# Outputs: filtered tables under data/intermediate/
#            - chicago_dv_2008_2024.csv
#            - chicago_homicides_2008_2024.csv
#            - chicago_shootings_2008_2024.csv
#            - chicago_shootings_2010_2024_conservador.csv
#            - _cobertura_temporal_chicago.csv (coverage summary)
#
# Note: shooting victims for 2008-2009 have much sparser coverage than 2010+
# (~500 vs ~3000 records/year), likely a CPD methodological change. A
# conservative 2010-2024 version is also written for that reason.
#
# Requires the raw downloads to be present. This is a build step and is NOT
# part of the offline analysis path.
# ============================================================================

# ---- portable paths (replication package) -------------------------------
.this_file <- tryCatch(
  normalizePath(sub("^--file=", "",
    grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1])),
  error = function(e) NA_character_)
ROOT <- if (!is.na(.this_file)) dirname(dirname(dirname(.this_file))) else normalizePath(".")
# -------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

# ── Paths ───────────────────────────────────────────────────────────────────
RAW_CHI <- file.path(ROOT, "data", "raw", "chicago")
OUT_DIR <- file.path(ROOT, "data", "intermediate")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

YEAR_MIN <- 2008L
YEAR_MAX <- 2024L

# Simple logger
log_msg <- function(...) {
  message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " — ", paste0(...))
}

# Helper: extract the year from a date column
parse_year <- function(df, col = "date") {
  df[["year"]] <- as.integer(format(
    suppressWarnings(as.POSIXct(df[[col]], tryFormats = c(
      "%Y-%m-%dT%H:%M:%S", "%Y-%m-%d %H:%M:%S", "%m/%d/%Y %H:%M:%S %p",
      "%Y-%m-%d", "%m/%d/%Y"
    ), tz = "UTC")),
    "%Y"
  ))
  df
}

# ── 1. Domestic-violence crimes ───────────────────────────────────────────────
log_msg("=== DV crimes ===")

OUT_DV <- file.path(OUT_DIR, "chicago_dv_2008_2024.csv")

if (file.exists(OUT_DV)) {
  log_msg("  Skipping: ", basename(OUT_DV), " already exists")
} else {
  dv_files <- list(
    "2008_2014" = file.path(RAW_CHI, "crimes", "chicago_domestic_violence_2008_2014.csv"),
    "2001_present" = file.path(RAW_CHI, "crimes", "chicago_domestic_violence_2001_present.csv")
  )

  dv_chunks <- list()
  for (label in names(dv_files)) {
    fpath <- dv_files[[label]]
    if (!file.exists(fpath)) {
      log_msg("  Not found: ", basename(fpath))
      next
    }
    df <- read_csv(fpath, show_col_types = FALSE, progress = FALSE)
    df <- parse_year(df, col = "date")
    df <- df[!is.na(df$year) & df$year >= YEAR_MIN & df$year <= YEAR_MAX, ]
    log_msg("  ", label, ": ", nrow(df), " rows after filter ", YEAR_MIN, "–", YEAR_MAX)
    dv_chunks[[label]] <- df
  }

  if (length(dv_chunks) > 0) {
    dv <- bind_rows(dv_chunks)

    # Deduplicate on the "id" column
    if ("id" %in% names(dv)) {
      before <- nrow(dv)
      dv <- dv[!duplicated(dv[["id"]]), ]
      log_msg("  Dedup on 'id': ", before, " → ", nrow(dv))
    }

    write_csv(dv, OUT_DV)
    log_msg("  Saved: ", basename(OUT_DV), " (", nrow(dv), " rows)")
  } else {
    log_msg("  WARNING: No DV file found. Skipping.")
  }
}

# ── 2. Homicides ──────────────────────────────────────────────────────────────
log_msg("=== Homicides ===")

OUT_HOM <- file.path(OUT_DIR, "chicago_homicides_2008_2024.csv")

if (file.exists(OUT_HOM)) {
  log_msg("  Skipping: ", basename(OUT_HOM), " already exists")
} else {
  hom_file <- file.path(RAW_CHI, "crimes", "chicago_homicides_2001_present.csv")
  if (file.exists(hom_file)) {
    hom <- read_csv(hom_file, show_col_types = FALSE, progress = FALSE)
    hom <- parse_year(hom, col = "date")
    hom <- hom[!is.na(hom$year) & hom$year >= YEAR_MIN & hom$year <= YEAR_MAX, ]
    write_csv(hom, OUT_HOM)
    log_msg("  ", nrow(hom), " homicides 2008–2024")
    counts_hom <- sort(table(hom$year))
    log_msg("  By year:\n", paste(
      paste0("    ", names(counts_hom), ": ", as.integer(counts_hom)),
      collapse = "\n"
    ))
  } else {
    log_msg("  File not found: ", basename(hom_file))
  }
}

# ── 3. Shooting victims ───────────────────────────────────────────────────────
log_msg("=== Shooting victims ===")

OUT_SHOOT_FULL <- file.path(OUT_DIR, "chicago_shootings_2008_2024.csv")
OUT_SHOOT_CONS <- file.path(OUT_DIR, "chicago_shootings_2010_2024_conservador.csv")

if (file.exists(OUT_SHOOT_FULL) && file.exists(OUT_SHOOT_CONS)) {
  log_msg("  Skipping: shooting files already exist")
} else {
  shoot_file <- file.path(RAW_CHI, "shootings", "chicago_shooting_victims.csv")
  if (file.exists(shoot_file)) {
    shoot <- read_csv(shoot_file, show_col_types = FALSE, progress = FALSE)

    # Try to detect the date column
    date_col <- intersect(c("date", "DATE", "Date", "incident_date"), names(shoot))[1]
    if (is.na(date_col)) date_col <- names(shoot)[1]
    shoot <- parse_year(shoot, col = date_col)

    # Full version 2008-2024
    if (!file.exists(OUT_SHOOT_FULL)) {
      shoot_full <- shoot[!is.na(shoot$year) & shoot$year >= YEAR_MIN & shoot$year <= YEAR_MAX, ]
      write_csv(shoot_full, OUT_SHOOT_FULL)
      log_msg("  ", nrow(shoot_full), " victims 2008–2024")

      # Flag years with suspect coverage
      counts_yr <- table(shoot_full$year)
      counts_2010plus <- counts_yr[as.integer(names(counts_yr)) >= 2010]
      if (length(counts_2010plus) > 0) {
        median_2010 <- median(as.integer(counts_2010plus))
        low_idx     <- as.integer(names(counts_yr)) < 2010 &
                       as.integer(counts_yr) < median_2010 * 0.3
        low_years   <- names(counts_yr)[low_idx]
        if (length(low_years) > 0) {
          log_msg("  WARNING: Years with suspect coverage (<30% of the 2010+ median): ",
                  paste(low_years, collapse = ", "))
          log_msg("  Median 2010+: ", round(median_2010, 0),
                  " | Suspect years: ", paste(low_years, collapse = ", "))
        }
      }
    }

    # Conservative version 2010-2024
    if (!file.exists(OUT_SHOOT_CONS)) {
      shoot_cons <- shoot[!is.na(shoot$year) & shoot$year >= 2010L & shoot$year <= YEAR_MAX, ]
      write_csv(shoot_cons, OUT_SHOOT_CONS)
      log_msg("  Conservative version (2010–2024): ", nrow(shoot_cons), " victims")
    }
  } else {
    log_msg("  File not found: ", basename(shoot_file))
  }
}

# ── 4. Temporal-coverage summary ──────────────────────────────────────────────
log_msg("=== Coverage summary 2008–2024 ===")

checks <- list(
  list(file = "chicago_dv_2008_2024.csv",                      label = "DV crimes Chicago"),
  list(file = "chicago_homicides_2008_2024.csv",                label = "Homicides Chicago"),
  list(file = "chicago_shootings_2008_2024.csv",                label = "Shooting victims Chicago"),
  list(file = "chicago_shootings_2010_2024_conservador.csv",    label = "Shootings (conservative, 2010+)")
)

summary_rows <- list()
for (chk in checks) {
  fpath <- file.path(OUT_DIR, chk$file)
  if (file.exists(fpath)) {
    df_chk <- tryCatch(
      read_csv(fpath, show_col_types = FALSE, progress = FALSE),
      error = function(e) NULL
    )
    if (!is.null(df_chk) && "year" %in% names(df_chk)) {
      anos_presentes <- sort(unique(df_chk$year[!is.na(df_chk$year)]))
      anos_esperados <- seq(YEAR_MIN, YEAR_MAX)
      gap <- setdiff(anos_esperados, anos_presentes)
      gap_str <- if (length(gap) == 0) "none" else paste(gap, collapse = ", ")
      n_linhas <- nrow(df_chk)
    } else {
      gap_str  <- ""
      n_linhas <- if (!is.null(df_chk)) nrow(df_chk) else NA_integer_
    }
    summary_rows[[length(summary_rows) + 1]] <- data.frame(
      dataset           = chk$label,
      janela_alvo       = paste0(YEAR_MIN, "–", YEAR_MAX),
      linhas_disponiveis = n_linhas,
      anos_com_gap      = gap_str,
      nota              = "",
      stringsAsFactors  = FALSE
    )
  }
}

if (length(summary_rows) > 0) {
  summary_df <- do.call(rbind, summary_rows)
  cob_out <- file.path(OUT_DIR, "_cobertura_temporal_chicago.csv")
  write_csv(summary_df, cob_out)
  log_msg("  _cobertura_temporal_chicago.csv saved")
  print(summary_df)
}

log_msg("Consolidation complete.")
