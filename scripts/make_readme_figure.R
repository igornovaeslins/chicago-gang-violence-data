# make_readme_figure.R — regenerate the female-share figure used in the README.
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(ggplot2); library(scales)
})
# resolve package root from this script's own location
.tf <- tryCatch(normalizePath(sub("^--file=","",grep("^--file=",commandArgs(FALSE),value=TRUE)[1])),error=function(e)NA_character_)
ROOT <- if(!is.na(.tf)) dirname(dirname(.tf)) else normalizePath(".")
setwd(ROOT)d <- read_csv("data/intermediate/shootings_with_gang_territory_2008_2024.csv", show_col_types = FALSE)
s <- d |>
  mutate(isF = toupper(trimws(sex)) == "F") |>
  group_by(year) |>
  summarise(pct = 100 * mean(isF, na.rm = TRUE), n = n(), .groups = "drop") |>
  mutate(sparse = year %in% c(2008, 2009))

pct_lab <- function(x) paste0(x, "%")

p <- ggplot(s, aes(year, pct)) +
  geom_smooth(data = filter(s, !sparse), method = "lm", se = FALSE,
              color = "#b0b0b0", linewidth = 0.6, linetype = "dashed") +
  geom_line(data = filter(s, !sparse), color = "#7a0177", linewidth = 1.1) +
  geom_point(aes(alpha = sparse), color = "#7a0177", size = 2.6) +
  scale_alpha_manual(values = c(`FALSE` = 1, `TRUE` = 0.30), guide = "none") +
  annotate("text", x = 2022, y = 16.2, label = "2022: nearly 1 in 6",
           hjust = 1.05, vjust = -0.8, size = 3.5, color = "#7a0177", fontface = "bold") +
  annotate("text", x = 2012, y = 9.4, label = "2010-2016: about 1 in 10",
           hjust = 0, vjust = 2.0, size = 3.3, color = "#555555") +
  scale_x_continuous(breaks = seq(2008, 2024, 2)) +
  scale_y_continuous(labels = pct_lab, limits = c(8, 17.5)) +
  labs(title = "Women are a rising share of Chicago's shooting victims",
       subtitle = "Female share of shooting victims, Chicago community areas, 2008-2024",
       x = NULL, y = "Female share of shooting victims",
       caption = "Chicago Police Department shooting-victim records. 2008-2009 shown faint (sparse early coverage).") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(color = "#555555", size = 10.5),
        plot.caption = element_text(color = "#888888", size = 8, hjust = 0),
        panel.grid.minor = element_blank())

ggsave("figures/female_share_shootings.png", p, width = 8, height = 4.8, dpi = 150, bg = "white")
cat("OK 2022:", round(s$pct[s$year == 2022], 1), "| 2010-16 mean:",
    round(mean(s$pct[s$year %in% 2010:2016]), 1), "\n")
