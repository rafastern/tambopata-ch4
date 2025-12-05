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
    # NEE, FCH4, H, LE are already named like this in the CSV
  ) %>%
  mutate(
    DateTime = dmy_hms(DateTime)
  )

# convert NaN to NA (in case Matlab wrote NaNs)
df$FCH4[is.nan(df$FCH4)] <- NA
df$H[is.nan(df$H)]       <- NA
df$LE[is.nan(df$LE)]     <- NA
df$NEE[is.nan(df$NEE)]   <- NA

# ───────────────────────────────────────────────────────────────────────────────
# time fields
df <- df %>%
  mutate(
    hour  = format(DateTime, "%H:00:00"),
    hour  = factor(hour, levels = sprintf("%02d:00:00", 0:23)),
    month = factor(month(DateTime, label = TRUE, abbr = TRUE),
                   levels = month.abb)
  )

df <- df %>% filter(month != "Aug")   # exclude August (keep or remove as you wish)

# ───────────────────────────────────────────────────────────────────────────────
# long format + diel statistics (mean ± SD per variable)
diel_df <- df %>%
  pivot_longer(
    cols = c(FCH4, H, LE, NEE),
    names_to = "variable",
    values_to = "value"
  ) %>%
  drop_na(value, hour, month) %>%
  group_by(month, hour, variable) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    sd_value   = sd(value,   na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  mutate(
    lower    = mean_value - sd_value,
    upper    = mean_value + sd_value,
    hour_num = as.numeric(hour) - 1
  )

# colors and labels for variables
var_cols <- c(
  FCH4 = "#D73027",   # red
  H    = "#1A9850",   # green
  LE   = "#4575B4",   # blue
  NEE  = "#984EA3"    # purple
)

var_labs <- c(
  FCH4 = "FCH4 (nmol m\u207b\u00b2 s\u207b\u00b9)",
  H    = "H (W m\u207b\u00b2)",
  LE   = "LE (W m\u207b\u00b2)",
  NEE  = "NEE (\u03bc mol m\u207b\u00b2 s\u207b\u00b9)"
)

# ───────────────────────────────────────────────────────────────────────────────
# diel plot with SD ribbons and correct units in legend
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
    title = "Diel Cycles at Mamirauá: FCH4, H, LE and NEE",
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
  filename = paste0(graphs_path, "diel_FCH4_H_LE_NEE_monthly_with_SD.png"),
  plot     = p,
  width    = 12,
  height   = 6,
  dpi      = 300
)

# ───────────────────────────────────────────────────────────────────────────────
# PLOT 1: H + LE
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
    title = "Diel Cycles at Mamirauá: H and LE",
    x = "Hour of Day",
    y = "Flux (W m\u207b\u00b2)",
    colour = "Variable",
    fill   = "Variable"
  ) +
  scale_x_continuous(
    breaks = c(2,8,14,20),
    labels = c("02:00","08:00","14:00","20:00")
  ) +
  scale_colour_manual(values = var_cols[c("H","LE")], labels = var_labs[c("H","LE")]) +
  scale_fill_manual(values = var_cols[c("H","LE")],   labels = var_labs[c("H","LE")]) +
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
# PLOT 2: FCH4 only
diel_FCH4 <- diel_df %>% 
  filter(variable == "FCH4")

p_FCH4 <- ggplot(diel_FCH4, aes(x = hour_num)) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    fill = var_cols["FCH4"], alpha = 0.25
  ) +
  geom_line(aes(y = mean_value), colour = var_cols["FCH4"], linewidth = 1.0) +
  geom_point(aes(y = mean_value), colour = var_cols["FCH4"], size = 1.7) +
  facet_wrap(~ month, ncol = 3) +
  theme_bw() +
  labs(
    title = "Diel Cycle at Mamirauá: FCH4",
    x = "Hour of Day",
    y = "FCH4 (nmol m\u207b\u00b2 s\u207b\u00b9)"
  ) +
  scale_x_continuous(
    breaks = c(2,8,14,20),
    labels = c("02:00","08:00","14:00","20:00")
  ) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.4) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text  = element_text(size = 12))

ggsave(
  filename = paste0(graphs_path, "diel_FCH4_only_monthly_with_SD.png"),
  plot = p_FCH4,
  width = 12,
  height = 6,
  dpi = 300
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
    title = "Diel Cycle at Mamirauá: NEE",
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
