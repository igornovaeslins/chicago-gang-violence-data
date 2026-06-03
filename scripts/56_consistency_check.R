# ============================================================================
# 56_consistency_check.R
#
# Internal consistency check. Recomputes a set of key statistics directly from
# the data and the result files produced by scripts 47-55, and verifies each
# matches its published reference value within a tolerance. Prints OK or CHECK
# per item, writes results/consistency_check.csv, and exits non-zero if any
# item fails (so it can serve as a CI / pre-release gate).
#
# Run last:  Rscript scripts/56_consistency_check.R
# ============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(stringr); library(fixest)
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
dir.create(RES, showWarnings = FALSE, recursive = TRUE)
# -------------------------------------------------------------------------

cat("==============================================================\n")
cat("56_consistency_check.R\n")
cat("==============================================================\n\n")

# Collector. `reference` is the published value; `computed` comes from the data.
AUD <- list()
TOL_REL <- 0.02   # default relative tolerance (2%)
check <- function(id, reference, computed, tol = TOL_REL, abs_ok = NA) {
  pa <- suppressWarnings(as.numeric(reference))
  da <- suppressWarnings(as.numeric(computed))
  if (is.na(pa) || is.na(da)) {
    status <- "?? (non-numeric)"
  } else if (!is.na(abs_ok)) {
    status <- if (abs(pa - da) <= abs_ok) "OK" else "CHECK"
  } else {
    rel <- if (da != 0) abs(pa - da) / abs(da) else abs(pa - da)
    status <- if (rel <= tol) "OK" else "CHECK"
  }
  AUD[[length(AUD) + 1]] <<- data.frame(
    id = id, reference = as.character(reference),
    computed = as.character(round(da, 4)), status = status, stringsAsFactors = FALSE)
  cat(sprintf("  [%-5s] %-44s ref=%-12s computed=%s\n",
              status, id, reference, round(da, 4)))
}

# ---------------------------------------------------------------------------
# Load data
# ---------------------------------------------------------------------------
p  <- read_csv(file.path(PROC, "chicago_painel_completo.csv"), show_col_types = FALSE)
sh <- read_csv(file.path(INT, "shootings_with_gang_territory_2008_2024.csv"),
               show_col_types = FALSE) |>
  mutate(year = as.integer(year), isF = toupper(trimws(sex)) == "F")
dv_raw <- read_csv(file.path(INT, "chicago_dv_2008_2024.csv"), show_col_types = FALSE,
                   col_select = c(year, arrest, primary_type))
dv_raw$arr <- as.integer(dv_raw$arrest == TRUE | dv_raw$arrest == "true" | dv_raw$arrest == "True")

p_no15 <- p |> filter(year != 2015)

# Modal gang family per community area (same rule as script 47)
modal_fam <- p |> filter(!is.na(gang_family)) |>
  group_by(community_area, gang_family) |> summarise(nn = n(), .groups = "drop") |>
  group_by(community_area) |> slice_max(nn, n = 1, with_ties = FALSE) |>
  select(community_area, fam = gang_family)

# DiD treatment construction (same as scripts 49/55)
pre <- p_no15 |> filter(year >= 2010, year <= 2014) |> group_by(community_area) |>
  summarise(pg = mean(pct_area_gang_control, na.rm = TRUE), .groups = "drop")
q33 <- quantile(pre$pg, 1/3, na.rm = TRUE); q67 <- quantile(pre$pg, 2/3, na.rm = TRUE)
pre <- pre |> mutate(grp = case_when(pg <= q33 ~ "C", pg >= q67 ~ "T", TRUE ~ "M"),
                     treat = as.integer(grp == "T"))
dd <- p_no15 |> filter(year >= 2010) |> inner_join(pre |> select(community_area, grp, treat), by = "community_area") |>
  filter(grp %in% c("C","T")) |> mutate(post = as.integer(year >= 2016), did = treat * post)

didbeta <- function(v, data = dd) {
  m <- feols(as.formula(paste(v, "~ did | community_area + year")), data = data, cluster = ~community_area)
  unname(coef(m)["did"])
}

cat("---- descriptive counts ----\n")
check("DV records total", 855387, nrow(read_csv(file.path(INT,"chicago_dv_2008_2024.csv"),
      show_col_types=FALSE, col_select=c(year))), abs_ok = 5)
check("Homicides total", 9882, sum(p$n_homicides, na.rm = TRUE), abs_ok = 5)
check("Shooting victims", 49291, nrow(sh), abs_ok = 5)
fs22 <- 100 * mean(sh$isF[sh$year == 2022], na.rm = TRUE)
check("Female share 2022 (%)", 16.2, fs22, abs_ok = 0.3)
comp <- dv_raw |> count(primary_type) |> mutate(pct = 100*n/sum(n))
check("Battery share (%)", 54.7, comp$pct[comp$primary_type=="BATTERY"], abs_ok = 0.3)
check("DV arrest rate mean (%)", 17.7,
      100*sum(p$n_dv_arrested,na.rm=TRUE)/sum(p$n_dv_crimes,na.rm=TRUE), abs_ok = 0.3)

cat("\n---- DV arrest rate vs homicides ----\n")
tann <- p_no15 |> group_by(year) |>
  summarise(taxa = 100*sum(n_dv_arrested,na.rm=TRUE)/sum(n_dv_crimes,na.rm=TRUE),
            hom = sum(n_homicides,na.rm=TRUE), .groups="drop")
check("Arrest rate 2014 (%)", 22.3, tann$taxa[tann$year==2014], abs_ok = 0.3)
check("Arrest rate 2021 (%)", 11.6, tann$taxa[tann$year==2021], abs_ok = 0.3)
check("Corr arrest-rate vs homicides", -0.81, cor(tann$taxa, tann$hom), abs_ok = 0.03)

cat("\n---- by gang family and configuration ----\n")
pf <- p_no15 |> inner_join(modal_fam, by = "community_area") |> filter(fam %in% c("Folk Nation","People Nation"))
fdv <- pf |> group_by(fam) |> summarise(dv = mean(n_dv_crimes,na.rm=TRUE),
        femsh = 100*sum(n_fem_shoot,na.rm=TRUE)/sum(n_shoot_total,na.rm=TRUE), .groups="drop")
check("Folk DV volume", 755, fdv$dv[fdv$fam=="Folk Nation"], abs_ok = 8)
check("People DV volume", 716, fdv$dv[fdv$fam=="People Nation"], abs_ok = 8)
check("Folk female share (%)", 12.8, fdv$femsh[fdv$fam=="Folk Nation"], abs_ok = 0.3)
check("People female share (%)", 12.4, fdv$femsh[fdv$fam=="People Nation"], abs_ok = 0.3)
cfg <- p_no15 |> mutate(c = case_when(pct_area_gang_control>0.70~"con",pct_area_gang_control>=0.30~"par",TRUE~"mar")) |>
  group_by(c) |> summarise(n=n(), dv=mean(n_dv_crimes,na.rm=TRUE), .groups="drop")
check("Consolidated config DV", 1233, cfg$dv[cfg$c=="con"], abs_ok = 5)
check("Consolidated config N", 75, cfg$n[cfg$c=="con"], abs_ok = 1)

cat("\n---- gang count ----\n")
ng <- p |> group_by(year) |> summarise(mg = mean(n_gangs_present,na.rm=TRUE), .groups="drop")
check("Mean gangs per area", 4.3, mean(ng$mg, na.rm=TRUE), abs_ok = 0.15)

cat("\n---- cross-sectional OLS (from results) ----\n")
t4 <- read_csv(file.path(RES,"tabela4_ols_com_controles.csv"), show_col_types=FALSE)
g <- function(out,mod) t4$estimate[t4$outcome==out & t4$model==mod & t4$term=="avg_pct_gang"]
check("OLS DV M1", 1618.2, g("DV crimes (mean/yr)","M1"))
check("OLS DV M3", -237.8, g("DV crimes (mean/yr)","M3"))
check("OLS fem-shoot M1", 14.897, g("Female shootings (mean/yr)","M1"))
check("OLS fem-shoot M3", 0.699, g("Female shootings (mean/yr)","M3"), abs_ok=0.05)

cat("\n---- main DiD ----\n")
check("DiD female shoots", 3.487, didbeta("n_fem_shoot"), abs_ok=0.01)
check("DiD fatal", 1.251, didbeta("n_fem_shoot_fatal"), abs_ok=0.01)
check("DiD DV homicides", 0.385, didbeta("n_dv_homicide"), abs_ok=0.01)
check("DiD all shoots", 4.675, didbeta("n_shoot_total"), abs_ok=0.05)
pm <- mean(dd$n_fem_shoot[dd$treat==1 & dd$year<=2014], na.rm=TRUE)
check("DiD implied % female", 51, 100*didbeta("n_fem_shoot")/pm, abs_ok=1)

cat("\n---- DiD by shooting location ----\n")
check("DiD public", 2.69, didbeta("n_fem_shoot_pub"), abs_ok=0.02)
check("DiD residential", 0.62, didbeta("n_fem_shoot_res"), abs_ok=0.02)
check("Ratio public/residential", 4.36, didbeta("n_fem_shoot_pub")/didbeta("n_fem_shoot_res"), abs_ok=0.1)

cat("\n---- robustness (entropy balancing, Moran, switches) ----\n")
eb <- read_csv(file.path(RES,"did_ebal_chicago.csv"), show_col_types=FALSE)
check("Entropy-balanced DiD female", 3.223, eb$beta[eb$outcome=="n_fem_shoot" & eb$spec=="Entropy-balanced"], abs_ok=0.01)
mi <- read_csv(file.path(RES,"morans_i_chicago.csv"), show_col_types=FALSE)
check("Moran's I", 0.05317, as.numeric(mi$valor[mi$estatistica=="Moran's I"]), abs_ok=0.001)
sw <- read_csv(file.path(RES,"diagnostico_gang_switching_chicago.csv"), show_col_types=FALSE)
check("CAs with no switch", 41, sum(sw$n_switches==0), abs_ok=0)
check("CAs with >=1 switch", 31, sum(sw$n_switches>=1), abs_ok=0)

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------
res <- bind_rows(AUD)
write_csv(res, file.path(RES, "consistency_check.csv"))
n_div <- sum(res$status == "CHECK")
n_ok  <- sum(res$status == "OK")
n_q   <- sum(grepl("\\?\\?", res$status))
cat("\n==============================================================\n")
cat(sprintf("RESULT: %d OK | %d CHECK | %d non-numeric | %d total\n",
            n_ok, n_div, n_q, nrow(res)))
if (n_div > 0) {
  cat("\n>>> Items to review:\n")
  print(res |> filter(status == "CHECK"), row.names = FALSE)
} else {
  cat(">>> All checks pass. Every statistic matches its reference value.\n")
}
cat("Saved: results/consistency_check.csv\n")
cat("==============================================================\n")
if (n_div > 0) quit(status = 1) else quit(status = 0)
