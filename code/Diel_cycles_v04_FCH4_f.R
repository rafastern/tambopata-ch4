# ───────────────────────────────────────────────────────────────────────────────
# libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(viridis)

# ───────────────────────────────────────────────────────────────────────────────
# paths
setwd("C:/Users/rafas/OneDrive/Documentos/Github/tambopata-ch4/code")

input_folder  <- "C:/Users/rafas/OneDrive/Documentos/Github/tambopata-ch4/data/"
graphs_path   <- "C:/Users/rafas/OneDrive/Documentos/Github/tambopata-ch4/graphs/"
output_path   <- "C:/Users/rafas/OneDrive/Documentos/Github/tambopata-ch4/output/"

# ───────────────────────────────────────────────────────────────────────────────
# data load
df <- read.csv(
  paste0(input_folder, "dataset_from_matlab.csv"),
  header = TRUE,
  sep = ",",
  stringsAsFactors = FALSE
)

# rename columns to simpler names (match your file)
df <- df %>%
  rename(
    DateTime = tv_dt
    # NEE, FCH4, H, LE, FCH4_f are assumed to be present in the CSV
  ) %>%
  mutate(
    DateTime = dmy_hms(DateTime)
  )

# convert NaN to NA (in case Matlab wrote NaNs)
df$FCH4[is.nan(df$FCH4)]     <- NA
df$H[is.nan(df$H)]           <- NA
df$LE[is.nan(df$LE)]         <- NA
df$NEE[is.nan(df$NEE)]       <- NA
if ("FCH4_f" %in% names(df)) {
  df$FCH4_f[is.nan(df$FCH4_f)] <- NA
} else {
  df$FCH4_f <- NA_real_
}

# ───────────────────────────────────────────────────────────────────────────────
# time fields: year, month, and half-hour-of-day (48 bins)
df <- df %>%
  mutate(
    year     = year(DateTime),
    month    = factor(month(DateTime, label = TRUE, abbr = TRUE),
                      levels = month.abb),
    hour_num = hour(DateTime) + minute(DateTime) / 60  # 0, 0.5, ..., 23.5
  )

# ───────────────────────────────────────────────────────────────────────────────
# choose FCH4_f when it has all 48 half-hour bins for that year–month,
# otherwise fall back to FCH4
availability <- df %>%
  group_by(year, month) %>%
  summarise(
    n_bins_fch4_f = n_distinct(hour_num[!is.na(FCH4_f)]),
    use_fch4_f    = (n_bins_fch4_f == 48),
    .groups       = "drop"
  )

df <- df %>%
  left_join(availability %>% select(year, month, use_fch4_f),
            by = c("year", "month")) %>%
  mutate(
    FCH4_use = if_else(use_fch4_f, FCH4_f, FCH4)
  ) %>%
  select(-use_fch4_f)

# ───────────────────────────────────────────────────────────────────────────────
# long format + diel statistics (mean ± SD per variable, per year; 48 bins)
# use FCH4_use as the methane flux in the general multi-variable plots
diel_df <- df %>%
  pivot_longer(
    cols = c(FCH4_use, H, LE, NEE),
    names_to = "variable",
    values_to = "value"
  ) %>%
  drop_na(value, hour_num, month, year) %>%
  group_by(year, month, hour_num, variable) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    sd_value   = sd(value,   na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  mutate(
    lower = mean_value - sd_value,
    upper = mean_value + sd_value,
    year  = factor(year)
  )

# aggregate across years for each month / hour_num / variable (for FCH4_use etc.)
diel_df_all <- diel_df %>%
  group_by(month, hour_num, variable) %>%
  summarise(
    mean_value = mean(mean_value, na.rm = TRUE),
    sd_value   = mean(sd_value,   na.rm = TRUE),
    lower      = mean(lower,      na.rm = TRUE),
    upper      = mean(upper,      na.rm = TRUE),
    .groups    = "drop"
  )

# ───────────────────────────────────────────────────────────────────────────────
# separate diel stats for PURE FCH4 (ignores FCH4_f completely)
diel_df_FCH4 <- df %>%
  drop_na(FCH4, hour_num, month, year) %>%
  group_by(year, month, hour_num) %>%
  summarise(
    mean_value = mean(FCH4, na.rm = TRUE),
    sd_value   = sd(FCH4,   na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  mutate(
    lower = mean_value - sd_value,
    upper = mean_value + sd_value,
    year  = factor(year)
  )

# aggregate PURE FCH4 across years
diel_df_all_FCH4 <- diel_df_FCH4 %>%
  group_by(month, hour_num) %>%
  summarise(
    mean_value = mean(mean_value, na.rm = TRUE),
    sd_value   = mean(sd_value,   na.rm = TRUE),
    lower      = mean(lower,      na.rm = TRUE),
    upper      = mean(upper,      na.rm = TRUE),
    .groups    = "drop"
  )

# ───────────────────────────────────────────────────────────────────────────────
# colors and labels for variables (for FCH4_use multi-variable plots)
var_cols <- c(
  FCH4_use = "#D73027",   # red
  H        = "#1A9850",   # green
  LE       = "#4575B4",   # blue
  NEE      = "#984EA3"    # purple
)

var_labs <- c(
  FCH4_use = "FCH4 (pref. FCH4_f; fallback FCH4)",
  H        = "H (W m\u207b\u00b2)",
  LE       = "LE (W m\u207b\u00b2)",
  NEE      = "NEE (\u03bc mol m\u207b\u00b2 s\u207b\u00b9)"
)

# ───────────────────────────────────────────────────────────────────────────────
# diel plot with SD ribbons and correct units in legend (FCH4_use, H, LE, NEE)
p <- ggplot(diel_df, aes(x = hour_num)) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper, fill = variable, group = variable),
    alpha = 0.18,
    colour = NA
  ) +
  geom_line(
    aes(y = mean_value, colour = variable, group = variable),
    linewidth = 0.9
  ) +
  geom_point(
    aes(y = mean_value, colour = variable),
    size = 1.7
  ) +
  facet_wrap(~ month, ncol = 3) +
  theme_bw() +
  labs(
    title = "Diel Cycles at Tambopata: FCH4_use, H, LE and NEE",
    x = "Hour of Day",
    y = "Value (FCH4: nmol m\u207b\u00b2 s\u207b\u00b9; H, LE: W m\u207b\u00b2; NEE: \u03bc mol m\u207b\u00b2 s\u207b\u00b9)",
    colour = "Variable",
    fill   = "Variable"
  ) +
  scale_x_continuous(
    breaks = c(2, 8, 14, 20),
    labels = c("02:00", "08:00", "14:00", "20:00")
  ) +
  scale_colour_manual(values = var_cols, labels = var_labs, name = "Variable") +
  scale_fill_manual(values = var_cols,   labels = var_labs, name = "Variable") +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.4) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text  = element_text(size = 12)
  )

print(p)

ggsave(
  filename = paste0(graphs_path, "diel_FCH4use_H_LE_NEE_monthly_with_SD.png"),
  plot     = p,
  width    = 12,
  height   = 6,
  dpi      = 300
)

# ───────────────────────────────────────────────────────────────────────────────
# PLOT 1: H + LE (using FCH4_use dataset, but only H & LE)
diel_H_LE <- diel_df %>% 
  filter(variable %in% c("H", "LE"))

p_H_LE <- ggplot(diel_H_LE, aes(x = hour_num)) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper, fill = variable, group = variable),
    alpha = 0.20, colour = NA
  ) +
  geom_line(aes(y = mean_value, colour = variable), linewidth = 1.0) +
  geom_point(aes(y = mean_value, colour = variable), size = 1.7) +
  facet_wrap(~ month, ncol = 3) +
  theme_bw() +
  labs(
    title = "Diel Cycles at Tambopata: H and LE",
    x = "Hour of Day",
    y = "Flux (W m\u207b\u00b2)",
    colour = "Variable",
    fill   = "Variable"
  ) +
  scale_x_continuous(
    breaks = c(2,8,14,20),
    labels = c("02:00","08:00","14:00","20:00")
  ) +
  scale_colour_manual(values = var_cols[c("H","LE")],
                      labels = var_labs[c("H","LE")]) +
  scale_fill_manual(values = var_cols[c("H","LE")],
                    labels = var_labs[c("H","LE")]) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.4) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text  = element_text(size = 12))

ggsave(
  filename = paste0(graphs_path, "diel_H_LE_monthly_with_SD.png"),
  plot = p_H_LE,
  width = 12,
  height = 6,
  dpi = 300
)

# ───────────────────────────────────────────────────────────────────────────────
# PLOT: FCH4_use only, by year WITH SD ribbons
diel_FCH4use <- diel_df %>% 
  filter(variable == "FCH4_use")

p_FCH4_use_year <- ggplot(diel_FCH4use, aes(x = hour_num)) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper, group = year, fill = year),
    alpha = 0.20,
    colour = NA
  ) +
  geom_line(
    aes(y = mean_value, colour = year, group = year),
    linewidth = 1.0
  ) +
  geom_point(
    aes(y = mean_value, colour = year),
    size = 1.6
  ) +
  facet_wrap(~ month, ncol = 3) +
  theme_bw() +
  labs(
    title = "Diel Cycle at Tambopata: FCH4_use by Year (Mean ± SD)",
    x = "Hour of Day",
    y = "FCH4_use (nmol m\u207b\u00b2 s\u207b\u00b9)",
    colour = "Year",
    fill   = "Year"
  ) +
  scale_x_continuous(
    breaks = c(2, 8, 14, 20),
    labels = c("02:00","08:00","14:00","20:00")
  ) +
  scale_colour_viridis_d() +
  scale_fill_viridis_d() +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.4) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text  = element_text(size = 12)
  )

ggsave(
  filename = paste0(graphs_path, "diel_FCH4use_by_year_with_SD.png"),
  plot = p_FCH4_use_year,
  width = 12,
  height = 6,
  dpi = 300
)

# PLOT: FCH4_use only, by year WITH SD ribbons, Y-axis limited
p_FCH4_use_limited <- ggplot(diel_FCH4use, aes(x = hour_num)) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper, group = year, fill = year),
    alpha = 0.20,
    colour = NA
  ) +
  geom_line(
    aes(y = mean_value, colour = year, group = year),
    linewidth = 1.0
  ) +
  geom_point(
    aes(y = mean_value, colour = year),
    size = 1.6
  ) +
  facet_wrap(~ month, ncol = 3) +
  theme_bw() +
  labs(
    title = "Diel Cycle at Tambopata: FCH4_use by Year (Mean ± SD), Y-Limited",
    x = "Hour of Day",
    y = "FCH4_use (nmol m\u207b\u00b2 s\u207b\u00b9)",
    colour = "Year",
    fill   = "Year"
  ) +
  scale_x_continuous(
    breaks = c(2, 8, 14, 20),
    labels = c("02:00","08:00","14:00","20:00")
  ) +
  scale_colour_viridis_d() +
  scale_fill_viridis_d() +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.4) +
  coord_cartesian(ylim = c(-20, 20)) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text  = element_text(size = 12)
  )

ggsave(
  filename = paste0(graphs_path, "diel_FCH4use_by_year_with_SD_ylim_-20_20.png"),
  plot = p_FCH4_use_limited,
  width = 12,
  height = 6,
  dpi = 300
)

# ───────────────────────────────────────────────────────────────────────────────
# NEW PLOT A: PURE FCH4, single panel, lines coloured by month (all years combined)
diel_FCH4_month <- diel_df_all_FCH4

p_FCH4_month_lines <- ggplot(diel_FCH4_month,
                             aes(x = hour_num,
                                 y = mean_value,
                                 colour = month,
                                 group  = month)) +
  geom_line(linewidth = 1.0) +
  geom_point(size = 1.6) +
  theme_bw() +
  labs(
    title  = "Diel Cycle at Tambopata (terra firme)",
    x      = "Hour of Day",
    y      = expression(FCH[4]~"(nmol m"^{-2}*" s"^{-1}*")"),
    colour = "Month"
  ) +
  scale_x_continuous(
    breaks = c(2, 8, 14, 20),
    labels = c("02:00", "08:00", "14:00", "20:00")
  ) +
  scale_colour_viridis_d(option = "turbo") +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.4) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_FCH4_month_lines)

ggsave(
  filename = paste0(graphs_path, "diel_FCH4_by_month_single_panel.png"),
  plot     = p_FCH4_month_lines,
  width    = 8,
  height   = 5,
  dpi      = 300
)

# ───────────────────────────────────────────────────────────────────────────────
# NEW PLOT B: FCH4_use, single panel, lines coloured by month (all years combined)
diel_FCH4use_month <- diel_df_all %>%
  filter(variable == "FCH4_use")

p_FCH4use_month_lines <- ggplot(diel_FCH4use_month,
                                aes(x = hour_num,
                                    y = mean_value,
                                    colour = month,
                                    group  = month)) +
  geom_line(linewidth = 1.0) +
  geom_point(size = 1.6) +
  theme_bw() +
  labs(
    title  = "Diel Cycle at Tambopata (terra firme): FCH4_use by Month (All Years Combined)",
    x      = "Hour of Day",
    y      = "FCH4_use (nmol m\u207b\u00b2 s\u207b\u00b9)",
    colour = "Month"
  ) +
  scale_x_continuous(
    breaks = c(2, 8, 14, 20),
    labels = c("02:00", "08:00", "14:00", "20:00")
  ) +
  scale_colour_viridis_d(option = "turbo") +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.4) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_FCH4use_month_lines)

ggsave(
  filename = paste0(graphs_path, "diel_FCH4use_by_month_single_panel.png"),
  plot     = p_FCH4use_month_lines,
  width    = 8,
  height   = 5,
  dpi      = 300
)

# ───────────────────────────────────────────────────────────────────────────────
# PLOT 3: NEE only
diel_NEE <- diel_df %>% 
  filter(variable == "NEE")

p_NEE <- ggplot(diel_NEE, aes(x = hour_num)) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    fill = var_cols["NEE"], alpha = 0.25
  ) +
  geom_line(aes(y = mean_value), colour = var_cols["NEE"], linewidth = 1.0) +
  geom_point(aes(y = mean_value), colour = var_cols["NEE"], size = 1.7) +
  facet_wrap(~ month, ncol = 3) +
  theme_bw() +
  labs(
    title = "Diel Cycle at Tambopata: NEE",
    x = "Hour of Day",
    y = "NEE (\u03bc mol m\u207b\u00b2 s\u207b\u00b9)"
  ) +
  scale_x_continuous(
    breaks = c(2,8,14,20),
    labels = c("02:00","08:00","14:00","20:00")
  ) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.4) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text  = element_text(size = 12))

ggsave(
  filename = paste0(graphs_path, "diel_NEE_only_monthly_with_SD.png"),
  plot = p_NEE,
  width = 12,
  height = 6,
  dpi = 300
)
