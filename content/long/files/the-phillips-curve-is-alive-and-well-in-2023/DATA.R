# Tin Skoric
# ECON 3500A Term Paper: Gordon Plot

# DATA CLEANING

library(papaja)
library(broom)
library(car)
library(dplyr)
library(ggplot2)
library(ggdark)
library(gtsummary)
library(psych)
library(knitr)
library(hpfilter)
library(lubridate)
library(RColorBrewer)
library(readxl)
library(summarytools)
library(tidyverse)
library(tseries)
library(zoo)

# Quarterly FRED Data (aggregated when needed)

# PCE Data [Gordon uses the logs of lagged quarterly moving averages]
# (Headline)
pce_overall <- read.csv("data/PCECTPI.csv")
pce_overall$PCECTPI <- as.numeric(pce_overall$PCECTPI)
pce_overall <- pce_overall%>% 
  mutate(Date = format(as.yearqtr(DATE, "%Y-%m-%d"), "%Y-Q%q")) %>%
  mutate(headline_inflation = 100 * (PCECTPI - lag(PCECTPI, 4))/lag(PCECTPI, 4)) %>%
  mutate(headline_inflation_change = headline_inflation - lag(headline_inflation, 1)) %>%
  mutate(headline_inflation_ma = 100 * (log(PCECTPI) - log(lag(PCECTPI, 4)))) %>% 
  select(Date, PCECTPI, headline_inflation, headline_inflation_change, headline_inflation_ma)
# (Core)
pce_core <- read.csv("data/JCXFE.csv")
pce_core$JCXFE <- as.numeric(pce_core$JCXFE)
pce_core <- pce_core%>% 
  mutate(Date = format(as.yearqtr(DATE, "%Y-%m-%d"), "%Y-Q%q")) %>%
  mutate(core_inflation = 100 * (JCXFE - lag(JCXFE, 4))/lag(JCXFE, 4)) %>%
  mutate(core_inflation_change = core_inflation - lag(core_inflation, 1)) %>%
  mutate(core_inflation_ma = 100 * (log(JCXFE) - log(lag(JCXFE, 4)))) %>% 
  select(Date, JCXFE, core_inflation, core_inflation_change, core_inflation_ma)
# (Combined)
inflation <- pce_overall %>% 
  left_join(pce_core)

# Food-Energy Effect (diff. between headline and core PCE inflation, I do not care about testing the "late period" thing)
inflation <- inflation %>% 
  mutate(food_energy = headline_inflation - core_inflation) %>%
  filter(Date >= "1960-Q1") %>% 
  mutate(food_energy_late_period = case_when(Date >= "1987-Q1" ~ (food_energy * 1),
                                             TRUE ~ 0))
# check:
ggplot(inflation, aes(factor(Date))) + geom_line(aes(x = factor(Date), y = (core_inflation_ma), group=1, color = "PCE"), size = 1) + 
  geom_line(aes(x = factor(Date), y = (headline_inflation_ma), group=1, color = "PCE 2"), size = 1)


# Unemployment (Total unemployment rate = long term + short term [see figure 6 in https://www.nber.org/system/files/working_papers/w19390/w19390.pdf])
# (Total)
total_unemployment <- read.csv("data/UNRATE.csv")
total_unemployment$UNRATE <- as.numeric(total_unemployment$UNRATE)
total_unemployment <- total_unemployment %>%
  mutate(Date = format(as.yearqtr(DATE, "%Y-%m-%d"), "%Y-Q%q")) %>% 
  filter(Date >= "1960-Q1") %>% 
  select(Date, UNRATE)
# (Short-term: [On FRED, create graph with this formula and these series: 100*(Unemployment Level-Number Unemployed for 27 Weeks & over)/Civilian Labor Force Level])
st_unemployment <- read.csv("data/ST_UNRATE.csv")
st_unemployment$UEMP27OV_UNEMPLOY_CLF16OV <- as.numeric(st_unemployment$UEMP27OV_UNEMPLOY_CLF16OV)
st_unemployment <- st_unemployment %>%
  rename(ST_UNRATE = UEMP27OV_UNEMPLOY_CLF16OV) %>%
  mutate(Date = format(as.yearqtr(DATE, "%Y-%m-%d"), "%Y-Q%q")) %>% 
  filter(Date >= "1960-Q1") %>% 
  select(Date, ST_UNRATE)
# Combined (w/ long term unemployment = total - short term)
unemployment <- total_unemployment %>% 
  left_join(st_unemployment) %>%
  mutate(LT_UNRATE = UNRATE - ST_UNRATE) %>% 
  select(Date, UNRATE, ST_UNRATE, LT_UNRATE)

# Productivity Trend ["In papers between 1977 and 2005, the productivity variable
# was the difference in the growth rate of actual productivity growth from trend
# productivity growth. Starting in Dew-Becker and Gordon (2005), the treatment
# changed to the eight-quarter change in the productivity trend. The idea was
# that a declining productivity growth trend in a world of rigid wages would put
# upward pressure on the inflation rate", footnote 12, https://www.nber.org/system/files/working_papers/w19390/w19390.pdf]

# "Dew-Becker and Gordon created a productivity trend growth acceleration
# variable equal to a Hodrick- Prescott filter version of the productivity
# growth trend minus that trend eight quarters earlier. The same productivity
# trend acceleration variable is plotted in the bottom frame of Figure 4"

# Data: FRED Non-Farm Labor Productivity Index 2012 = 100; OPHNFB
productivity <- read.csv("data/OPHNFB.csv")
productivity$OPHNFB <- as.numeric(productivity$OPHNFB)
productivity <- productivity%>% 
  mutate(Date = format(as.yearqtr(DATE, "%Y-%m-%d"), "%Y-Q%q")) %>% 
  mutate(actual_8_quarter_change = 100 * (OPHNFB - lag(OPHNFB, 8))/lag(OPHNFB, 8)) %>% 
  filter(Date >= "1960-Q1") %>% 
  mutate(n = row_number())

productivity_data <- productivity %>% 
  select(actual_8_quarter_change)
# Hodrick-Prescott Filter, https://www.nber.org/system/files/working_papers/w11842/w11842.pdf
productivity_trend <- hp2(productivity_data, lambda = 6400)
productivity_trend_data <- productivity_trend %>% 
  rename(trend_8_quarter = actual_8_quarter_change) %>% 
  mutate(n = row_number())
productivity <- productivity %>%
  left_join(productivity_trend_data) %>% 
  select(Date, OPHNFB, trend_8_quarter, actual_8_quarter_change)

# COMBINED (ALL)

GordonModelDataset <- inflation %>% 
  left_join(unemployment) %>% 
  left_join(productivity)

# Nixon Controls dummy variable ["The Nixon controls "on" dummy variable is
# entered as a variable equal to 0.8 for the five quarters 1971:Q3—1972:Q3. The
# "off" variable is equal to 0.4 in 1974:Q2 and 1975:Q1, and equal to 1.6 in l974:Q3 and
# 1974:Q4. The respective dummy variables sum to 4.0 rather than 1.0 because the
# dependent variable in each equation is a quarterly change expressed as an annual
# rate, i.e., multiplied by 4.0."]

GordonModelDataset <- GordonModelDataset %>% 
  mutate(nixon_price_on = case_when(Date >= "1971-Q3" & Date <= "1972-Q3" ~ 0.8,
                                    TRUE ~ 0),
         nixon_price_off = case_when(Date == "1974-Q2" & Date == "1975-Q1" ~ 0.4,
                                     Date >= "1974-Q3" & Date <= "1974-Q4" ~ 1.6,
                                     TRUE ~ 0)) %>% 
  mutate(NIXON = nixon_price_on + nixon_price_off)

# check: 
ggplot(GordonModelDataset, aes(factor(Date))) + geom_line(aes(x = factor(Date), y = (headline_inflation_ma), group=1, color = "PCE"), size = 1) +
  geom_line(aes(x = factor(Date), y = (UNRATE), group=1, color = "UNRATE"), size = 1)

GordonModelDataset_raw <- read.csv("data/PCECTPI.csv") %>% 
  left_join(read.csv("data/JCXFE.csv")) %>% 
  left_join(read.csv("data/UNRATE.csv")) %>% 
  left_join(read.csv("data/ST_UNRATE.csv")) %>% 
  left_join(read.csv("data/OPHNFB.csv"))

write.csv(print(describe(GordonModelDataset_raw, skew = FALSE, IQR = FALSE), digits = 3), "summary_raw.csv", row.names = FALSE)

GordonModelDataset_plain <- GordonModelDataset %>% 
  select(Date, PCECTPI, JCXFE, UNRATE, ST_UNRATE, OPHNFB)

summarystat <- GordonModelDataset_plain %>% 
  tbl_summary(
    include = c(PCECTPI, JCXFE, UNRATE, ST_UNRATE, OPHNFB),
    label = c(PCECTPI ~ "PCECTPI (PCE Headline Index)", JCXFE ~ "JCXFE (PCE Core Index)", UNRATE ~ "Unemployment Rate", ST_UNRATE ~ "Short Term Unemployment Rate", OPHNFB ~ "Nonfarm Business Labor Productivity"),
    statistic = ~ "{mean} ({sd}), {min}:{max}",
  ) %>% 
  modify_header(label ~ "**Variables**") %>% 
  modify_caption("Table 1. Summary Statistics (Q1 1960 to Q2 2023)") %>% 
  as_flex_table()

write.csv(print(describe(GordonModelDataset_plain, skew = FALSE, IQR = FALSE), digits = 3), "summary.csv", row.names = FALSE)

write.csv(print(describe(GordonModelDataset, skew = FALSE, IQR = FALSE), digits = 3), "summary_2.csv", row.names = FALSE)


write.csv(GordonModelDataset_plain, "DataCleaned.csv", row.names = FALSE)

write.csv(GordonModelDataset, "DataComplete.csv", row.names = FALSE)

inflation_plot <- ggplot(GordonModelDataset, aes(factor(Date))) + 
  geom_line(aes(x = factor(Date), y = (core_inflation), group=1, color = "Core Inflation (%)"), linewidth = 1) +
  geom_line(aes(x = factor(Date), y = (headline_inflation), group=1, color = "Headline Inflation (%)"), size = 1) +
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
    subtitle = "Data from Q1 1960 to Q2 2023",
    title = "Inflation Comparison Core/Headline PCE",
    x = "Quarter",
    y = "Inflation\n(%)"
  )
ggsave("inflation_plot.png", plot = inflation_plot, device = "png", width = 11, height = 8.5, path = "plots")
