# ============================================================================
# 00_setup.R — install the R packages the replication needs.
#
# Run once before the analysis scripts:
#   Rscript scripts/00_setup.R
#
# Installs into your default library, so the analysis scripts find everything
# with a plain library() call. R >= 4.3 is assumed.
# ============================================================================

pkgs <- c(
  "dplyr", "tidyr", "readr", "stringr",   # data manipulation
  "fixest",                                # fixed-effects / DiD estimation
  "sandwich", "lmtest", "broom",           # robust SEs, tidy output
  "sf", "spdep", "spatialreg",             # spatial: Moran's I, SAR
  "WeightIt", "cobalt",                    # entropy balancing (script 50)
  "ggplot2", "scales",                     # event-study figures
  "purrr", "httr", "jsonlite"              # build/ scripts (raw -> intermediate)
)

missing <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(missing)) {
  message("Installing: ", paste(missing, collapse = ", "))
  install.packages(missing, repos = "https://cloud.r-project.org")
} else {
  message("All packages already installed.")
}

# Confirm everything loads
invisible(lapply(pkgs, function(p)
  suppressPackageStartupMessages(require(p, character.only = TRUE))))
still <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(still)) stop("Failed to install: ", paste(still, collapse = ", "))
message("Setup OK — ", length(pkgs), " packages ready.")
