# ============================================================================
# 55_mechanism_tests_chicago.R
#
# Post-2015 mechanism tests, computed from chicago_painel_completo.csv:
#
#   (1) Police-arrests change:   DiD on log(arrests) and on the arrest rate
#   (2) Mediation check:         DiD on female shoots controlling for log(arrests)
#   (3) Triple interaction: did x pre-period n_gangs
#   (4) Residential vs. public:  DiD on residential vs. public female shoots (+ Wald)
#   (5) DV arrest rate:          DiD on the domestic-violence arrest rate
#   (6) Post-2016 timing:        Treat x 2016 vs. Treat x post-2016 (fatal)
#
# Treatment definition matches script 49: Treat = top tertile of pre-2015
# pct_area_gang_control; Control = bottom tertile; pre 2010-2014, post 2016-2024,
# 2015 dropped.
#
# NOTE: the arrests-based rows use log(arrests). The CPD arrests dataset
# (dpt3-jri9) begins in 2014, so n_arrests is zero before 2014. Tests that depend
# on arrests are restricted to year >= 2014 (pre = 2014, post = 2016+).
#
# Input : data/processed/chicago_painel_completo.csv
# Output: results/tabela7_mechanism_chicago.csv, results/tabela8_gang_transition_chicago.csv
# ============================================================================


suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(stringr)
  library(fixest); library(sandwich); library(lmtest)
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
cat("55_mechanism_tests_chicago.R\n\n")

# ---------------------------------------------------------------------------
# 1. Painel + definição de tratamento (idêntico ao script 49)
# ---------------------------------------------------------------------------
df <- read_csv(file.path(PROC, "chicago_painel_completo.csv"),
               show_col_types = FALSE) |>
  filter(year != 2015, year >= 2010)

pre <- df |>
  filter(year <= 2014) |>
  group_by(community_area) |>
  summarise(pre_pct_gang = mean(pct_area_gang_control, na.rm = TRUE),
            pre_n_gangs   = mean(n_gangs_present,        na.rm = TRUE),
            .groups = "drop")

q33 <- quantile(pre$pre_pct_gang, 1/3, na.rm = TRUE)
q67 <- quantile(pre$pre_pct_gang, 2/3, na.rm = TRUE)
pre <- pre |>
  mutate(group = case_when(pre_pct_gang <= q33 ~ "Control",
                           pre_pct_gang >= q67 ~ "Treated",
                           TRUE                ~ "Middle"),
         treat = as.integer(group == "Treated"))

df <- df |>
  left_join(pre |> select(community_area, group, treat, pre_n_gangs),
            by = "community_area") |>
  filter(group %in% c("Control", "Treated")) |>
  mutate(post = as.integer(year >= 2016),
         did  = treat * post)

cat("Amostra (full):", nrow(df), "obs |",
    sum(pre$group == "Treated"), "treated +",
    sum(pre$group == "Control"), "control CAs\n")

# Sub-amostra com dados de prisões (CPD arrests começa em 2014)
df_arr <- df |> filter(year >= 2014) |>
  mutate(larrests = log(pmax(n_arrests, 1)),
         arr_rate = ifelse(n_dv_crimes > 0, n_dv_arrested / n_dv_crimes, NA_real_))
cat("Amostra (arrests, year>=2014):", nrow(df_arr), "obs\n\n")

# helper para extrair beta/se/p de um feols sobre 'did'
grab <- function(m, term = "did") {
  ct <- coeftest(m)
  c(beta = unname(coef(m)[term]),
    se   = unname(sqrt(diag(vcov(m)))[term]),
    p    = unname(ct[term, "Pr(>|t|)"]))
}

rows <- list()

# ---------------------------------------------------------------------------
# (1) DiD on arrests (level, log, and rate)
# ---------------------------------------------------------------------------
m_arr_log  <- feols(larrests ~ did | community_area + year,
                    data = df_arr, cluster = ~community_area)
m_arr_lvl  <- feols(n_arrests ~ did | community_area + year,
                    data = df_arr, cluster = ~community_area)
m_arr_rate <- feols(arr_rate ~ did | community_area + year,
                    data = df_arr, cluster = ~community_area)
g_log  <- grab(m_arr_log); g_lvl <- grab(m_arr_lvl); g_rate <- grab(m_arr_rate)
cat(sprintf("(1) First stage: log(arr) %+.3f (p=%.3f) | level %+.1f | rate %+.4f (p=%.3f)\n",
            g_log["beta"], g_log["p"], g_lvl["beta"], g_rate["beta"], g_rate["p"]))
rows[["fs_log"]]  <- c(grp="Arrests", test="DiD on log(arrests)",
                       est=sprintf("%+.3f", g_log["beta"]), p=sprintf("%.3f", g_log["p"]))
rows[["fs_rate"]] <- c(grp="", test="DiD on DV arrest rate",
                       est=sprintf("%+.4f", g_rate["beta"]), p=sprintf("%.3f", g_rate["p"]))

# ---------------------------------------------------------------------------
# (2) Female shoots DiD with and without log(arrests) as control
# ---------------------------------------------------------------------------
m_med0 <- feols(n_fem_shoot ~ did            | community_area + year,
                data = df_arr, cluster = ~community_area)
m_med1 <- feols(n_fem_shoot ~ did + larrests | community_area + year,
                data = df_arr, cluster = ~community_area)
b0 <- unname(coef(m_med0)["did"]); b1 <- unname(coef(m_med1)["did"])
p_larr <- coeftest(m_med1)["larrests", "Pr(>|t|)"]
cat(sprintf("(2) With/without arrests: base %+.3f -> +log(arr) %+.3f | larr coef p=%.3f\n",
            b0, b1, p_larr))
rows[["med"]] <- c(grp="Control for arrests", test="DiD on female shoots, ctrl log(arrests)",
                   est=sprintf("%+.2f (was %+.2f)", b1, b0), p="—")

# ---------------------------------------------------------------------------
# (3) Triple interaction: did x pre-period n_gangs
# ---------------------------------------------------------------------------
m_tri_fem <- feols(n_fem_shoot       ~ did * pre_n_gangs | community_area + year,
                   data = df, cluster = ~community_area)
m_tri_fat <- feols(n_fem_shoot_fatal ~ did * pre_n_gangs | community_area + year,
                   data = df, cluster = ~community_area)
gt_fem <- grab(m_tri_fem, "did:pre_n_gangs")
gt_fat <- grab(m_tri_fat, "did:pre_n_gangs")
cat(sprintf("(3) Triple-interaction: fem %+.3f (p=%.2f) | fatal %+.3f (p=%.2f)\n",
            gt_fem["beta"], gt_fem["p"], gt_fat["beta"], gt_fat["p"]))
rows[["tri_fem"]] <- c(grp="Pre-period gang count",
                       test="Triple DiD x pre-period gangs (fem shoots)",
                       est=sprintf("%+.2f", gt_fem["beta"]), p=sprintf("%.2f", gt_fem["p"]))
rows[["tri_fat"]] <- c(grp="", test="Triple DiD x pre-period gangs (fatal)",
                       est=sprintf("%+.2f", gt_fat["beta"]), p=sprintf("%.2f", gt_fat["p"]))

# ---------------------------------------------------------------------------
# (4) Residential vs. public female shoots: DiD on each + Wald test
# ---------------------------------------------------------------------------
m_pub <- feols(n_fem_shoot_pub ~ did | community_area + year,
               data = df, cluster = ~community_area)
m_res <- feols(n_fem_shoot_res ~ did | community_area + year,
               data = df, cluster = ~community_area)
g_pub <- grab(m_pub); g_res <- grab(m_res)
df_stack <- bind_rows(
  df |> transmute(community_area, year, did, Y = n_fem_shoot_pub, type = "public"),
  df |> transmute(community_area, year, did, Y = n_fem_shoot_res, type = "residence")
) |> mutate(type = factor(type, levels = c("residence","public")),
            did_x_pub = did * (type == "public"))
m_stack <- feols(Y ~ did + did_x_pub | community_area^type + year^type,
                 data = df_stack, cluster = ~community_area)
b_diff  <- unname(coef(m_stack)["did_x_pub"])
se_diff <- unname(sqrt(diag(vcov(m_stack)))["did_x_pub"])
p_diff  <- 2 * (1 - pnorm(abs(b_diff / se_diff)))
ratio   <- g_pub["beta"] / g_res["beta"]
cat(sprintf("(4) Location split: res %+.3f (p=%.3f) | pub %+.3f (p=%.3f) | Wald diff %+.3f (p=%.4f) | ratio %.2fx\n",
            g_res["beta"], g_res["p"], g_pub["beta"], g_pub["p"], b_diff, p_diff, ratio))
rows[["res"]]  <- c(grp="Location split", test="DiD on residential female shoots",
                    est=sprintf("%+.2f", g_res["beta"]), p=sprintf("%.3f", g_res["p"]))
rows[["pub"]]  <- c(grp="", test="DiD on public-space female shoots",
                    est=sprintf("%+.2f", g_pub["beta"]), p=sprintf("%.3f", g_pub["p"]))
rows[["wald"]] <- c(grp="", test="Wald test b_pub = b_res (stacked interaction)",
                    est=sprintf("%+.2f", b_diff),
                    p=sprintf("%.4f", p_diff))

# ---------------------------------------------------------------------------
# (5) DiD on the DV arrest rate
# ---------------------------------------------------------------------------
m_coop <- feols(arr_rate ~ did | community_area + year,
                data = df_arr, cluster = ~community_area)
g_coop <- grab(m_coop)
cat(sprintf("(5) DV arrest-rate DiD %+.4f (p=%.3f)\n",
            g_coop["beta"], g_coop["p"]))
rows[["coop"]] <- c(grp="DV arrest rate", test="DiD on DV arrest rate",
                    est=sprintf("%+.4f", g_coop["beta"]), p=sprintf("%.2f", g_coop["p"]))

# ---------------------------------------------------------------------------
# (6) Post-2016 timing: Treat x 2016 vs. Treat x 2017+ (fatal female)
# ---------------------------------------------------------------------------
df_post <- df |>
  mutate(t2016    = treat * (year == 2016),
         t_post2016 = treat * (year >= 2017))
m_post <- feols(n_fem_shoot_fatal ~ t2016 + t_post2016 | community_area + year,
                data = df_post, cluster = ~community_area)
gf16 <- grab(m_post, "t2016"); gfpost <- grab(m_post, "t_post2016")
cat(sprintf("(6) Timing: treat x 2016 %+.2f (p=%.2f) | treat x 2017+ %+.2f (p=%.3f)\n",
            gf16["beta"], gf16["p"], gfpost["beta"], gfpost["p"]))
rows[["t16"]]   <- c(grp="Post-2016 timing", test="Treat x 2016 only (fatal)",
                        est=sprintf("%+.2f", gf16["beta"]), p=sprintf("%.2f", gf16["p"]))
rows[["tpost"]] <- c(grp="", test="Treat x 2017+ (fatal)",
                        est=sprintf("%+.2f", gfpost["beta"]), p=sprintf("%.3f", gfpost["p"]))

# ---------------------------------------------------------------------------
# Monta tabela e salva
# ---------------------------------------------------------------------------
tab <- do.call(rbind, lapply(rows, function(r)
  data.frame(test_group = r["grp"], test = r["test"],
             estimate = r["est"], p = r["p"], row.names = NULL)))

cat("\n--- mechanism tests ---\n")
print(tab, row.names = FALSE)
write_csv(tab, file.path(RES, "tabela7_mechanism_chicago.csv"))
cat("\nSaved: results/tabela7_mechanism_chicago.csv\n")

# ===========================================================================
# Event study of changes in the modal dominant gang, plus n_gangs.
#   (a) Event time relative to the FIRST change in modal dominant gang per CA.
#       Reference: t <= -2. Coefficients at t-1, t0, t+1, t>=2.
#       Poisson TWFE (log DV), SE clustered by CA. Identification uses only
#       CAs with at least one change (others contribute no variation).
#   (b) Efeito do número de gangues presentes (fragmentação contínua).
# ===========================================================================
cat("\n--- event study: gang change + n_gangs ---\n")

panel <- read_csv(file.path(PROC, "chicago_painel_completo.csv"),
                  show_col_types = FALSE) |>
  filter(year != 2015) |>
  arrange(community_area, year)

# first change in modal dominant gang per CA -> relative event time
trans <- panel |>
  group_by(community_area) |>
  mutate(prev_gang = lag(gang_dominant),
         is_switch = !is.na(prev_gang) & gang_dominant != prev_gang) |>
  summarise(first_switch_year = ifelse(any(is_switch),
                                       min(year[is_switch], na.rm = TRUE), NA_real_),
            .groups = "drop")

es <- panel |>
  left_join(trans, by = "community_area") |>
  filter(!is.na(first_switch_year)) |>            # só CAs que trocaram (identificação)
  mutate(etime = year - first_switch_year,
         # bin nas pontas para estabilidade
         ebin = case_when(etime <= -2 ~ "ref",
                          etime == -1 ~ "tm1",
                          etime ==  0 ~ "t0",
                          etime ==  1 ~ "tp1",
                          etime >=  2 ~ "tp2plus"),
         ebin = factor(ebin, levels = c("ref","tm1","t0","tp1","tp2plus")))

m_es8 <- fepois(n_dv_crimes ~ i(ebin, ref = "ref") | community_area + year,
                data = es, cluster = ~community_area)
cat("Event study (ref = t<=-2), N CAs com troca =",
    length(unique(es$community_area)), "\n")
print(coeftable(m_es8))

# Efeito de n_gangs (fragmentação contínua) — painel completo
m_ng8 <- fepois(n_dv_crimes ~ n_gangs_present | community_area + year,
                data = panel, cluster = ~community_area)
cat("\nn_gangs effect:\n"); print(coeftable(m_ng8))

# Salva coeficientes do event study + n_gangs
es_co <- as.data.frame(coeftable(m_es8))
es_co$term <- rownames(es_co); rownames(es_co) <- NULL
ng_co <- as.data.frame(coeftable(m_ng8))
ng_co$term <- rownames(ng_co); rownames(ng_co) <- NULL
write_csv(bind_rows(es_co, ng_co), file.path(RES, "tabela8_gang_transition_chicago.csv"))
cat("Salvo: results/tabela8_gang_transition_chicago.csv\n")
cat("==============================================================\n")
