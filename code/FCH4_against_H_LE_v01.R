# ───────────────────────────────────────────────────────────────────────────────
# libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(viridis)

# ───────────────────────────────────────────────────────────────────────────────
# paths
setwd("C:/Users/rafas/OneDrive/Documentos/Github/mamiraua-ch4/code")

input_folder  <- "C:/Users/rafas/OneDrive/Documentos/Github/mamiraua-ch4/data/"
graphs_path   <- "C:/Users/rafas/OneDrive/Documentos/Github/mamiraua-ch4/graphs/"
output_path   <- "C:/Users/rafas/OneDrive/Documentos/Github/mamiraua-ch4/output/"

# ───────────────────────────────────────────────────────────────────────────────
# data load
df <- read.csv(
  paste0(input_folder, "dataset_from_matlab.txt"),
  header = TRUE,
  sep = ",",
  stringsAsFactors = FALSE
)

# rename columns to simpler names
df <- df %>%
  rename(
    DateTime = tv_dt,
    FCH4     = FCH4_PI_JSZ_MAD_RP_uStar_orig,
    H        = H_PI_JSZ_MAD_RP_uStar_orig,
    LE       = LE_PI_JSZ_MAD_RP_uStar_orig
  ) %>%
  mutate(DateTime = dmy_hms(DateTime))

# fix NaNs
df$FCH4[is.nan(df$FCH4)] <- NA
df$H[is.nan(df$H)]       <- NA
df$LE[is.nan(df$LE)]     <- NA

# ───────────────────────────────────────────────────────────────────────────────
# time fields
df <- df %>%
  mutate(
    month = factor(month(DateTime, label = TRUE, abbr = TRUE),
                   levels = month.abb)
  ) %>%
  filter(month != "Aug")          # remove August

# keep only data after September 20 (adjust year if needed)
df <- df %>% filter(DateTime > ymd("2024-09-20"))

# keep only when H > 10 AND LE > 10
df <- df %>% filter(H > 10, LE > 10)

# ───────────────────────────────────────────────────────────────────────────────
# 1) FCH4 vs H  (faceted by month, with lm, equation and R2)

df_H <- df %>%
  drop_na(FCH4, H, month)

# regression stats per month
reg_H <- df_H %>%
  group_by(month) %>%
  reframe({
    m <- lm(FCH4 ~ H, data = cur_data())
    tibble(
      slope     = coef(m)[2],
      intercept = coef(m)[1],
      r2        = summary(m)$r.squared,
      x         = -Inf,   # left edge of panel
      y         = Inf     # top of panel
    )
  })

p_H <- ggplot(df_H, aes(x = H, y = FCH4)) +
  geom_point(alpha = 0.35, size = 1.2, color = "firebrick") +
  geom_smooth(method = "lm", color = "black", linewidth = 0.8) +
  facet_wrap(~ month, ncol = 3, scales = "free") +
  geom_text(
    data = reg_H,
    aes(
      x = x, y = y,
      label = sprintf("y = %.3f x %+ .2f\nR\u00b2 = %.2f", slope, intercept, r2)
    ),
    hjust = -0.1, vjust = 1.1,
    size = 3,
    inherit.aes = FALSE
  ) +
  theme_bw() +
  labs(
    title = "FCH4 vs H (Filtered: Date > Sept 20, H>10, LE>10)",
    x = expression(H ~ "(W" ~ m^{-2} * ")"),
    y = expression(F[CH4] ~ "(nmol" ~ m^{-2} ~ s^{-1} * ")")
  ) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.4)

print(p_H)

ggsave(
  filename = paste0(graphs_path, "scatter_FCH4_vs_H_with_lm_R2.png"),
  plot     = p_H,
  width    = 10,
  height   = 6,
  dpi      = 300
)

# ───────────────────────────────────────────────────────────────────────────────
# 2) FCH4 vs LE  (faceted by month, with lm, equation and R2)

df_LE <- df %>%
  drop_na(FCH4, LE, month)

reg_LE <- df_LE %>%
  group_by(month) %>%
  reframe({
    m <- lm(FCH4 ~ LE, data = cur_data())
    tibble(
      slope     = coef(m)[2],
      intercept = coef(m)[1],
      r2        = summary(m)$r.squared,
      x         = -Inf,
      y         = Inf
    )
  })

p_LE <- ggplot(df_LE, aes(x = LE, y = FCH4)) +
  geom_point(alpha = 0.35, size = 1.2, color = "darkblue") +
  geom_smooth(method = "lm", color = "black", linewidth = 0.8) +
  facet_wrap(~ month, ncol = 3, scales = "free") +
  geom_text(
    data = reg_LE,
    aes(
      x = x, y = y,
      label = sprintf("y = %.3f x %+ .2f\nR\u00b2 = %.2f", slope, intercept, r2)
    ),
    hjust = -0.1, vjust = 1.1,
    size = 3,
    inherit.aes = FALSE
  ) +
  theme_bw() +
  labs(
    title = "FCH4 vs LE (Filtered: Date > Sept 20, H>10, LE>10)",
    x = expression(LE ~ "(W" ~ m^{-2} * ")"),
    y = expression(F[CH4] ~ "(nmol" ~ m^{-2} ~ s^{-1} * ")")
  ) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.4)

print(p_LE)

ggsave(
  filename = paste0(graphs_path, "scatter_FCH4_vs_LE_with_lm_R2.png"),
  plot     = p_LE,
  width    = 10,
  height   = 6,
  dpi      = 300
)