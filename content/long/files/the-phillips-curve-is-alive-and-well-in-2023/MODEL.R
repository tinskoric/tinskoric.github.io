# Tin Skoric
# ECON 3500A Term Paper: Gordon Plot

library(broom)
library(car)
library(dplyr)
library(forecast)
library(gtsummary)
library(ggplot2)
library(psych)
library(ggdark)
library(haven)
library(knitr)
library(hpfilter)
library(lubridate)
library(RColorBrewer)
library(readxl)
library(roll)
library(summarytools)
library(stargazer)
library(systemfit)
library(tidyverse)
library(tseries)
library(TTR)
library(zoo)

data <- read.csv("DataComplete.csv")

data <- data %>% 
  mutate(n = row_number())

# TV-NAIRU (Core PCE: "when supply-shock variables are omitted, the TV-NAIRU
# soars to 8 percent and above in the mid-1970s, since this is the only way the
# inflation equation can “explain” why inflation was so high in the 1970s."
# https://www.nber.org/system/files/working_papers/w19390/w19390.pdf)
# https://www.princeton.edu/~mwatson/papers/Staiger_Stock_Watson_JEP_1997.pdf

# Instead of the fancy cubic polynomial spline regressions that Staiger, Stock,
# and Watson (1997) and Gordon (2013) do, I do a rolling regression, under the
# advice of Prof. Sicotte, using a window width of 52.

# d.inf = [-(beta1 + beta2)nairu] + beta1unemployment(t-1) + beta2unemployment(t-2)
# the first term ^ is the intercept!!!
# ex: nairu_estimate <- intercept/(-1 * (beta1 + beta2) = 0.82330/(-1*(-0.30313 + 0.14596))
# I then run a hodrick-prescott filter on the raw NAIRU estimates derived from the
# coefficients/intercept to smooth things out.

x_vars_TVNAIRU <- data %>% 
  mutate(Lag1_UNRATE = lag(UNRATE, 1), Lag2_UNRATE = lag(UNRATE, 2), Lag3_UNRATE = lag(UNRATE, 3), Lag4_UNRATE = lag(UNRATE, 4)) %>%
  select(Lag1_UNRATE, Lag2_UNRATE, Lag3_UNRATE, Lag4_UNRATE) 

x_vars_TVNAIRU <- as.matrix(x_vars_TVNAIRU)

# Matrix of dependent vars on CHANGE in inflation!
TV_NAIRU_regression <- roll_lm(x_vars_TVNAIRU, (data$core_inflation - lag(data$core_inflation, 1)), 52)
TV_NAIRU_dataframe <- as.data.frame(TV_NAIRU_regression) %>% 
  mutate(NAIRU = (coefficients..Intercept.) / (-1 * (coefficients.Lag1_UNRATE + coefficients.Lag2_UNRATE + coefficients.Lag3_UNRATE + coefficients.Lag4_UNRATE))) %>%
  filter(is.na(NAIRU) != TRUE) %>% 
  mutate(n = row_number())

# Smoothing the NAIRU

TV_NAIRU_data_smoothing <- TV_NAIRU_dataframe %>% 
  select(NAIRU)
TV_NAIRU_trend <- hp2(TV_NAIRU_data_smoothing, lambda = 1600) # 1600 is standard for quarterly data
TV_NAIRU_trend_data <- TV_NAIRU_trend %>% 
  rename(NAIRU_smoothed = NAIRU) %>% 
  mutate(n = row_number())
TV_NAIRU_dataframe <- TV_NAIRU_dataframe %>%
  left_join(TV_NAIRU_trend_data) %>% 
  select(NAIRU_smoothed, NAIRU) %>% 
  mutate(n = row_number())

super <- data %>%
  filter(n >= 56) %>% # 56 when 52 period window, 87 when 83 period
  mutate(n = row_number()) %>% # to align the TV NAIRU with other data after NAs were removed
  left_join(TV_NAIRU_dataframe) %>% 
  mutate(UNGAP_raw = (UNRATE - NAIRU)) %>% # unsmoothed NAIRU
  mutate(UNGAP = (UNRATE - NAIRU_smoothed))

# Smoothed vs. unsmoothed NAIRU

ggplot(super, aes(factor(Date))) + geom_line(aes(x = factor(Date), y = (NAIRU), group=1, color = "unsmoothed"), linewidth = 1) +
  geom_line(aes(x = factor(Date), y = (NAIRU_smoothed), group=1, color = "smoothed"), linewidth = 1)

# Smoothed vs. unsmoothed NAIRU UNGAP

ungap_plot <- ggplot(super, aes(factor(Date))) + geom_line(aes(x = factor(Date), y = (UNGAP_raw), group=1, color = "Unsmoothed"), linewidth = 1) +
  geom_line(aes(x = factor(Date), y = (UNGAP), group=1, color = "Smoothed"), linewidth = 1) +
  theme_classic() + 
  theme(axis.line = element_line(color = "#003a5d"),
        axis.text = element_text(color = "#003a5d"),
        text =  element_text(face = "bold", color = "#003a5d"),
        panel.grid.major.y = element_line(color = "#003a5dff", size = 0.25, linetype = 2),
        legend.position = "bottom") +
  scale_x_discrete(breaks = c("1975-Q1", "1990-Q1", "2005-Q1", "2020-Q1")) +
  scale_color_brewer(palette = "Set1") +
  labs(
    caption = "Source: U.S. Bureau of Economic Analysis via St. Louis Federal Reserve",
    color = "Indicies",
    subtitle = "Data from Q4 1974 to Q2 2023",
    title = "Unemployment Gap with Smoothed/Unsmoothed Estimated TV-NAIRU",
    x = "Quarter",
    y = "Percent\n(%)"
  )
ggsave("ungap_plot.png", plot = ungap_plot, device = "png", width = 11, height = 8.5, path = "plots")

write.csv(super, "GordonModelDataset.csv", row.names = FALSE)

write.csv(print(describe(super, skew = FALSE, IQR = FALSE), digits = 3), "summary_final.csv", row.names = FALSE)

# ------------------------------------------------------------------------------

# PART 2

super <- read.csv("GordonModelDataset.csv")

super <- super %>% 
  mutate(n = row_number()) %>%
  mutate(data_decade = case_when(Date < "1980-Q1" ~ "1970s",
                                 Date >= "1980-Q1" & Date < "1990-Q1" ~ "1980s",
                                 Date >= "1990-Q1" & Date < "2000-Q1" ~ "1990s",
                                 Date >= "2000-Q1" & Date < "2010-Q1" ~ "2000s",
                                 Date >= "2010-Q1" & Date < "2020-Q1" ~ "2010s",
                                 Date >= "2020-Q1" & Date < "2030-Q1" ~ "2020s"))
trad_curve_data <- data %>% 
  mutate(data_decade = case_when(Date < "1970-Q1" ~ "1960s",
                                 Date >= "1970-Q1" & Date < "1980-Q1" ~ "1970s",
                                 Date >= "1980-Q1" & Date < "1990-Q1" ~ "1980s",
                                 Date >= "1990-Q1" & Date < "2000-Q1" ~ "1990s",
                                 Date >= "2000-Q1" & Date < "2010-Q1" ~ "2000s",
                                 Date >= "2010-Q1" & Date < "2020-Q1" ~ "2010s",
                                 Date >= "2020-Q1" & Date < "2030-Q1" ~ "2020s"))

super_lite <- super %>% 
  filter(Date <= "2013-Q2")

# --- # Phillips Curves

trad_curve <- ggplot(trad_curve_data, aes(UNRATE, headline_inflation)) + geom_point(aes(color = data_decade)) +
  geom_smooth(color = "#003a5d", method = "lm", se = FALSE) +
  theme_classic() + 
  theme(axis.line = element_line(color = "#003a5d"),
        axis.text = element_text(color = "#003a5d"),
        text =  element_text(face = "bold", color = "#003a5d"),
        panel.grid.major.y = element_line(color = "#003a5dff", size = 0.25, linetype = 2),
        legend.position = "bottom") +
    scale_x_continuous(breaks = c(4, 6, 8, 10, 12, 14)) +
    scale_color_brewer(palette = "Set1") +
    labs(
      caption = "Sources: [1] U.S. Bureau of Labor Statistics via St. Louis Federal Reserve,
[2] U.S. Bureau of Economic Analysis via St. Louis Federal Reserve",
      color = "Decade",
      subtitle = "Data from Q1 1960 to Q2 2023",
      title = "Traditional Phillips Curve",
      x = "Unemployment Rate (%)",
      y = "Headline PCE Inflation (%)"
  )
ggsave("trad_curve.png", plot = trad_curve, device = "png", width = 11, height = 8.5, path = "plots")

accel_curve <- ggplot(super, aes(UNGAP, headline_inflation_change)) + geom_point(aes(color = data_decade)) +
  geom_smooth(color = "#003a5d", method = "lm", se = FALSE) +
  theme_classic() + 
  theme(axis.line = element_line(color = "#003a5d"),
        axis.text = element_text(color = "#003a5d"),
        text =  element_text(face = "bold", color = "#003a5d"),
        panel.grid.major.y = element_line(color = "#003a5dff", size = 0.25, linetype = 2),
        legend.position = "bottom") +
  scale_x_continuous(breaks = c(-4,-2,0,2,4, 6, 8, 10, 12, 14)) +
  scale_color_brewer(palette = "Set1") +
  labs(
    caption = "Sources: [1] U.S. Bureau of Labor Statistics via St. Louis Federal Reserve,
[2] U.S. Bureau of Economic Analysis via St. Louis Federal Reserve",
    color = "Decade",
    subtitle = "Data from Q4 1974 to Q2 2023",
    title = "Accelerationist Phillips Curve",
    x = "Unemployment Gap (%)",
    y = "ΔHeadline PCE Inflation (%)"
  )
ggsave("accel_curve.png", plot = accel_curve, device = "png", width = 11, height = 8.5, path = "plots")

accel_curve_2 <- ggplot(super, aes(UNGAP, headline_inflation_change)) + geom_point(aes(color = data_decade)) +
  geom_smooth(aes(color = data_decade), method = "lm", se = FALSE) +
  theme_classic() + 
  theme(axis.line = element_line(color = "#003a5d"),
        axis.text = element_text(color = "#003a5d"),
        text =  element_text(face = "bold", color = "#003a5d"),
        panel.grid.major.y = element_line(color = "#003a5dff", size = 0.25, linetype = 2),
        legend.position = "bottom") +
  scale_x_continuous(breaks = c(-4,-2,0,2,4, 6, 8, 10, 12, 14)) +
  scale_color_brewer(palette = "Set1") +
  labs(
    caption = "Sources: [1] U.S. Bureau of Labor Statistics via St. Louis Federal Reserve,
[2] U.S. Bureau of Economic Analysis via St. Louis Federal Reserve",
    color = "Decade",
    subtitle = "Data from Q4 1974 to Q2 2023",
    title = "Accelerationist Phillips Curve",
    x = "Unemployment Gap (%)",
    y = "ΔHeadline PCE Inflation (%)"
  )
ggsave("accel_curve_2.png", plot = accel_curve_2, device = "png", width = 11, height = 8.5, path = "plots")

accel_model <- lm(headline_inflation_change ~ UNGAP, super_lite)

stargazer(accel_model, type="text",
          dep.var.labels=c("ΔInflation"),
          covariate.labels=c("UNGAP"), out="accel.csv")

super_accel <- super %>% 
  mutate(accel_model_pred_change = predict(accel_model, super)) %>% 
  mutate(accel_model_pred_inf = lag(headline_inflation, 1) + accel_model_pred_change)

accel_curve_model <- ggplot(super_accel, aes(factor(Date))) + 
  geom_line(aes(x = factor(Date), y = (headline_inflation_change), group=1, color = "Actual ΔInflation (%)"), linewidth = 1) +
  geom_line(aes(x = factor(Date), y = (accel_model_pred_change), group=1, color = "Predicted ΔInflation (%)"), alpha = 0.7, linewidth = 1) +
  geom_point(aes(x = factor(Date), y = (accel_model_pred_change), group=1, color = "Predicted ΔInflation (%)"), size = 1) +
  geom_vline(xintercept = "2013-Q2", color = "#003a5d") + 
  annotate("text", x = "2012-Q2", y = 2.5, label = "Fitted", color = "#003a5d") + 
  annotate("text", x = "2014-Q4", y = 2.5, label = "Simulated", color = "#003a5d") +
  annotate("text", x = "2012-Q2", y = 2.25, label = "⬅️", color = "#003a5d") + 
  annotate("text", x = "2014-Q4", y = 2.25, label = "➡️", color = "#003a5d") +
  theme_classic() + 
  theme(axis.line = element_line(color = "#003a5d"),
        axis.text = element_text(color = "#003a5d"),
        text =  element_text(face = "bold", color = "#003a5d"),
        panel.grid.major.y = element_line(color = "#003a5dff", size = 0.25, linetype = 2),
        legend.position = "bottom") +
  scale_x_discrete(breaks = c("1983-Q2", "1993-Q2", "2003-Q2", "2013-Q2", "2023-Q2")) +
  scale_color_brewer(palette = "Set1") +
  labs(
    caption = "Sources: [1] U.S. Bureau of Labor Statistics via St. Louis Federal Reserve,
[2] U.S. Bureau of Economic Analysis via St. Louis Federal Reserve",
    color = "Indicies",
    subtitle = "Data from Q4 1974 to Q2 2023 (Sample period Q4 1974 to Q2 2013)",
    title = "Simple Accelerationist Phillips Curve Model",
    x = "Quarter",
    y = "ΔInflation"
  )
ggsave("accel_curve_model.png", plot = accel_curve_model, device = "png", width = 11, height = 8.5, path = "plots")

accel_inflation <- ggplot(super_accel, aes(factor(Date))) + 
  geom_line(aes(x = factor(Date), y = (headline_inflation), group=1, color = "Actual Inflation (%)"), linewidth = 1) +
  geom_line(aes(x = factor(Date), y = (accel_model_pred_inf), group=1, color = "Predicted Inflation (%)"), alpha = 0.7, linewidth = 1) +
  geom_point(aes(x = factor(Date), y = (accel_model_pred_inf), group=1, color = "Predicted Inflation (%)"), size = 1) +
  geom_vline(xintercept = "2013-Q2", color = "#003a5d") + 
  annotate("text", x = "2012-Q2", y = 2.5, label = "Fitted", color = "#003a5d") + 
  annotate("text", x = "2014-Q4", y = 2.5, label = "Simulated", color = "#003a5d") +
  annotate("text", x = "2012-Q2", y = 2.25, label = "⬅️", color = "#003a5d") + 
  annotate("text", x = "2014-Q4", y = 2.25, label = "➡️", color = "#003a5d") +
  theme_classic() + 
  theme(axis.line = element_line(color = "#003a5d"),
        axis.text = element_text(color = "#003a5d"),
        text =  element_text(face = "bold", color = "#003a5d"),
        panel.grid.major.y = element_line(color = "#003a5dff", size = 0.25, linetype = 2),
        legend.position = "bottom") +
  scale_x_discrete(breaks = c("1983-Q2", "1993-Q2", "2003-Q2", "2013-Q2", "2023-Q2")) +
  scale_color_brewer(palette = "Set1") +
  labs(
    caption = "Sources: [1] U.S. Bureau of Labor Statistics via St. Louis Federal Reserve,
[2] U.S. Bureau of Economic Analysis via St. Louis Federal Reserve",
    color = "Indicies",
    subtitle = "Data from Q4 1974 to Q2 2023 (Sample period Q4 1974 to Q2 2013)",
    title = "Simple Accelerationist Phillips Curve Model (Inflation)",
    x = "Quarter",
    y = "Inflation"
  )
ggsave("accel_inflation.png", plot = accel_inflation, device = "png", width = 11, height = 8.5, path = "plots")

# ------------------------------------------------------------------------------

# Gordon Triangle (following implementation in Gordon's 2013 paper,
# however I don't include the relative price of imports)
# https://www.nber.org/system/files/working_papers/w19390/w19390.pdf
# How he beats the data in this regression is insane.

# THE MODEL

# ------------------------------------------------------------------------------

# Sample period: 1983-Q2:2013-Q2
# Data period: 1983:Q2-2023-Q2

# (UNRATE - NAIRU_smoothed), removed to avoid simultaneous regression w/ a rolling model

PCE_gordon_lite <- lm(headline_inflation ~ lag(headline_inflation_ma, 1) + lag(headline_inflation, 2) + lag(headline_inflation, 3) + lag(headline_inflation, 4) +
                        lag(headline_inflation_ma, 5) + lag(headline_inflation, 6) + lag(headline_inflation, 7) + lag(headline_inflation, 8) +
                        lag(headline_inflation_ma, 9) + lag(headline_inflation, 10) + lag(headline_inflation, 11) + lag(headline_inflation, 12) +
                        lag(headline_inflation_ma, 13) + lag(headline_inflation, 14) + lag(headline_inflation, 15) + lag(headline_inflation, 16) +
                        lag(headline_inflation_ma, 17) + lag(headline_inflation, 18) + lag(headline_inflation, 19) + lag(headline_inflation, 20) +
                        lag(headline_inflation_ma, 21) + lag(headline_inflation, 22) + lag(headline_inflation, 23) + lag(headline_inflation, 24) +
                        UNGAP + lag(UNGAP, 1) + lag(UNGAP, 2) + lag(UNGAP, 3) + lag(UNGAP, 4) +
                        food_energy + lag(food_energy, 1) + lag(food_energy, 2) + lag(food_energy, 3) + lag(food_energy, 4) +
                        food_energy_late_period + lag(food_energy_late_period, 1) + lag(food_energy_late_period, 2) + lag(food_energy_late_period, 3) + lag(food_energy_late_period, 4) +
                        lag(trend_8_quarter, 1) + lag(trend_8_quarter, 2) + lag(trend_8_quarter, 3) + lag(trend_8_quarter, 4) + lag(trend_8_quarter, 5), super_lite)


stargazer(PCE_gordon_lite, type="html",
          dep.var.labels=c("Headline PCE Inflation"), out="gordon_results_initial.html")

PCE_gordon_lite <- lm(headline_inflation ~ lag(headline_inflation_ma, 1) + lag(headline_inflation, 4) +
                        lag(headline_inflation_ma, 5) + 
                        UNGAP + 
                        food_energy + lag(food_energy, 1) +
                        food_energy_late_period + lag(food_energy_late_period, 1), super_lite)

summary(PCE_gordon_lite) # Ignore the fact that most of this stuff isn't statistically significant.

super_2013 <- super %>% 
  mutate(PCE_gordon_prediction = predict(PCE_gordon_lite, super)) %>% 
  filter(Date >= "1983-Q2")

PCE_delta_gordon_plot_20yearSample <- ggplot(super_2013, aes(factor(Date))) + 
  geom_line(aes(x = factor(Date), y = (headline_inflation), group=1, color = "Actual Inflation (%)"), linewidth = 1) +
  geom_line(aes(x = factor(Date), y = (PCE_gordon_prediction), group=1, color = "Predicted Inflation (%)"), alpha = 0.7, linewidth = 1) +
  geom_point(aes(x = factor(Date), y = (PCE_gordon_prediction), group=1, color = "Predicted Inflation (%)"), size = 1) +
  geom_vline(xintercept = "2013-Q2", color = "#003a5d") + 
  annotate("text", x = "2012-Q2", y = 7, label = "Fitted", color = "#003a5d") + 
  annotate("text", x = "2014-Q4", y = 7, label = "Simulated", color = "#003a5d") +
  annotate("text", x = "2012-Q2", y = 6.75, label = "⬅️", color = "#003a5d") + 
  annotate("text", x = "2014-Q4", y = 6.75, label = "➡️", color = "#003a5d") +
  theme_classic() + 
  theme(axis.line = element_line(color = "#003a5d"),
        axis.text = element_text(color = "#003a5d"),
        text =  element_text(face = "bold", color = "#003a5d"),
        panel.grid.major.y = element_line(color = "#003a5dff", size = 0.25, linetype = 2),
        legend.position = "bottom") +
  scale_x_discrete(breaks = c("1983-Q2", "1993-Q2", "2003-Q2", "2013-Q2", "2023-Q2")) +
  scale_color_brewer(palette = "Set1") +
  labs(
    caption = "Sources: [1] U.S. Bureau of Labor Statistics via St. Louis Federal Reserve,
[2] U.S. Bureau of Economic Analysis via St. Louis Federal Reserve",
    color = "Indicies",
    subtitle = "Gordon's Triangle Model, Data from Q2 1983 to Q2 2023 (Sample period Q4 1974 to Q2 2003)",
    title = "Inflation Comparison Overall PCE Real/Predicted",
    x = "Quarter",
    y = "Inflation\n(%)"
  )
ggsave("PCE_delta_gordon_plot_20yearSample.png", plot = PCE_delta_gordon_plot_20yearSample, device = "png", width = 11, height = 8.5, path = "plots")

stargazer(PCE_gordon_lite, type="html",
          dep.var.labels=c("Headline PCE Inflation"), out="gordon_results.html")

PCE_gordon_lite <- lm(headline_inflation ~ lag(headline_inflation_ma, 1) + lag(headline_inflation, 4) +
                        lag(headline_inflation_ma, 5) + 
                        food_energy + lag(food_energy, 1) +
                        food_energy_late_period + lag(food_energy_late_period, 1), super_lite)

summary(PCE_gordon_lite) # Ignore the fact that most of this stuff isn't statistically significant.

super_2013 <- super %>% 
  mutate(PCE_gordon_prediction = predict(PCE_gordon_lite, super)) %>% 
  filter(Date >= "1983-Q2")

PCE_delta_gordon_plot_20yearSample_no_ungap <- ggplot(super_2013, aes(factor(Date))) + 
  geom_line(aes(x = factor(Date), y = (headline_inflation), group=1, color = "Actual Inflation (%)"), linewidth = 1) +
  geom_line(aes(x = factor(Date), y = (PCE_gordon_prediction), group=1, color = "Predicted Inflation (%)"), alpha = 0.7, linewidth = 1) +
  geom_point(aes(x = factor(Date), y = (PCE_gordon_prediction), group=1, color = "Predicted Inflation (%)"), size = 1) +
  geom_vline(xintercept = "2013-Q2", color = "#003a5d") + 
  annotate("text", x = "2012-Q2", y = 7, label = "Fitted", color = "#003a5d") + 
  annotate("text", x = "2014-Q4", y = 7, label = "Simulated", color = "#003a5d") +
  annotate("text", x = "2012-Q2", y = 6.75, label = "⬅️", color = "#003a5d") + 
  annotate("text", x = "2014-Q4", y = 6.75, label = "➡️", color = "#003a5d") +
  theme_classic() + 
  theme(axis.line = element_line(color = "#003a5d"),
        axis.text = element_text(color = "#003a5d"),
        text =  element_text(face = "bold", color = "#003a5d"),
        panel.grid.major.y = element_line(color = "#003a5dff", size = 0.25, linetype = 2),
        legend.position = "bottom") +
  scale_x_discrete(breaks = c("1983-Q2", "1993-Q2", "2003-Q2", "2013-Q2", "2023-Q2")) +
  scale_color_brewer(palette = "Set1") +
  labs(
    caption = "Sources: [1] U.S. Bureau of Labor Statistics via St. Louis Federal Reserve,
[2] U.S. Bureau of Economic Analysis via St. Louis Federal Reserve",
    color = "Indicies",
    subtitle = "Gordon's Triangle Model, Data from Q2 1983 to Q2 2023 (Sample period Q4 1974 to Q2 2003)",
    title = "Inflation Comparison Overall PCE Real/Predicted",
    x = "Quarter",
    y = "Inflation\n(%)"
  )
ggsave("PCE_delta_gordon_plot_20yearSample_no_ungap.png", plot = PCE_delta_gordon_plot_20yearSample_no_ungap, device = "png", width = 11, height = 8.5, path = "plots")

# ------------------------------------------------------------------------------

# Sample period: 1983-Q2:2023-Q2
# Data period: 1983:Q2-2023-Q2

ungap <- super %>% 
  mutate(u = UNGAP, u1 = lag(UNGAP, 1), u2 = lag(UNGAP, 2), u3 = lag(UNGAP, 3), u4 = lag(UNGAP, 4)) %>%
  select(u,u1,u2,u3,u4) 

ungap <- as.matrix(ungap)

PCE_gordon <- lm(headline_inflation ~ lag(headline_inflation_ma, 1) + lag(headline_inflation, 2) + lag(headline_inflation, 3) + lag(headline_inflation, 4) +
                   lag(headline_inflation_ma, 5) + lag(headline_inflation, 8) + lag(headline_inflation_ma, 9) + lag(headline_inflation, 10) + lag(headline_inflation, 11) + lag(headline_inflation, 12) +
                   lag(headline_inflation_ma, 13) + lag(headline_inflation, 14) + lag(headline_inflation, 15) + lag(headline_inflation, 16) +
                   lag(headline_inflation_ma, 17) + lag(headline_inflation, 18) + lag(headline_inflation, 19) + lag(headline_inflation, 20) +
                   lag(headline_inflation_ma, 21) + lag(headline_inflation, 22) + lag(headline_inflation, 23) + lag(headline_inflation, 24) +
                   UNGAP + lag(UNGAP, 1) + lag(UNGAP, 2) + lag(UNGAP, 3) + lag(UNGAP, 4) +
                   food_energy + lag(food_energy, 1) + lag(food_energy, 2) + lag(food_energy, 3) + lag(food_energy, 4) +
                   food_energy_late_period + lag(food_energy_late_period, 1) + lag(food_energy_late_period, 2) + lag(food_energy_late_period, 3) + lag(food_energy_late_period, 4) +
                   lag(trend_8_quarter, 1) + lag(trend_8_quarter, 2) + lag(trend_8_quarter, 3) + lag(trend_8_quarter, 4) + lag(trend_8_quarter, 5), super)

summary(PCE_gordon) # Ignore the fact that most of this stuff isn't statistically significant again!

super <- super %>% 
  mutate(PCE_gordon_prediction = predict(PCE_gordon, super)) %>% 
  filter(Date >= "1983-Q2")

PCE_delta_gordon_plot <- ggplot(super, aes(factor(Date))) + 
  geom_line(aes(x = factor(Date), y = (headline_inflation), group=1, color = "Actual Inflation (%)"), linewidth = 1) +
  geom_line(aes(x = factor(Date), y = (PCE_gordon_prediction), group=1, color = "Predicted Inflation (%)"), alpha = 0.7, linewidth = 1) +
  geom_point(aes(x = factor(Date), y = (PCE_gordon_prediction), group=1, color = "Predicted Inflation (%)"), size = 1) +
  theme_classic() + 
  theme(axis.line = element_line(color = "#003a5d"),
        axis.text = element_text(color = "#003a5d"),
        text =  element_text(face = "bold", color = "#003a5d"),
        panel.grid.major.y = element_line(color = "#003a5dff", size = 0.25, linetype = 2),
        legend.position = "bottom") +
  scale_x_discrete(breaks = c("1983-Q2", "1993-Q2", "2003-Q2", "2013-Q2", "2023-Q2")) +
  scale_color_brewer(palette = "Set1") +
  labs(
    caption = "Sources: [1] U.S. Bureau of Labor Statistics via St. Louis Federal Reserve,
[2] U.S. Bureau of Economic Analysis via St. Louis Federal Reserve",
    color = "Indicies",
    subtitle = "Gordon's Triangle Model, Data from Q2 1983 to Q2 2023 (Sample period Q2 1983 to Q2 2023)",
    title = "Inflation Comparison Overall PCE Real/Predicted",
    x = "Quarter",
    y = "Inflation\n(%)"
  )
ggsave("PCE_delta_gordon_plot.png", plot = PCE_delta_gordon_plot, device = "png", width = 11, height = 8.5, path = "plots")

# ------------------------------------------------------------------------------

# Graphs look good but do we trust this? ADF tests!

adf.test(super$headline_inflation)
adf.test(super$headline_inflation_ma)
adf.test(super$UNRATE)
adf.test(super$NAIRU_smoothed)
adf.test(super$UNGAP)
adf.test(super$food_energy)
adf.test(super$food_energy_late_period)
adf.test(super$trend_8_quarter)

write.csv(tidy(adf.test(super$headline_inflation), digits = 3), "adf_pce_headline.csv", row.names = FALSE)


# ------------------------------------------------------------------------------

# Testing Validity:

confint(PCE_gordon, level = 0.95)