
# Week 2 - Data Visualization and Insight Communication using R
# Dataset: ggplot2::economics
# The dataset is included with ggplot2, so no external CSV is required.

install.packages(c("ggplot2", "dplyr", "scales"))  # Run once if needed

library(ggplot2)
library(dplyr)
library(scales)

data("economics")

economics <- economics %>%
  mutate(
    year = as.integer(format(date, "%Y")),
    decade = floor(year / 10) * 10
  )

# Quick data checks
str(economics)
summary(economics)
colSums(is.na(economics))


# 1. Unemployment trend
ggplot(economics, aes(x = date, y = unemploy)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Unemployment Trend Over Time",
    x = "Date",
    y = "Unemployed people (thousands)"
  ) +
  theme_minimal()

ggsave("01_unemployment_trend.png", width = 10, height = 5.6, dpi = 300)


# 2. Personal consumption expenditure trend
ggplot(economics, aes(x = date, y = pce)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Personal Consumption Expenditures (PCE) Trend",
    x = "Date",
    y = "PCE (billions of dollars)"
  ) +
  theme_minimal()

ggsave("02_pce_trend.png", width = 10, height = 5.6, dpi = 300)


# 3. Distribution of unemployment
ggplot(economics, aes(x = unemploy)) +
  geom_histogram(bins = 28, color = "white") +
  labs(
    title = "Distribution of Monthly Unemployment",
    x = "Unemployed people (thousands)",
    y = "Number of months"
  ) +
  theme_minimal()

ggsave("03_unemployment_histogram.png", width = 10, height = 5.6, dpi = 300)


# 4. Relationship between PCE and unemployment
ggplot(economics, aes(x = pce, y = unemploy)) +
  geom_point(alpha = 0.45) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "PCE vs. Unemployment",
    x = "Personal consumption expenditures (billions)",
    y = "Unemployed people (thousands)"
  ) +
  theme_minimal()

ggsave("04_pce_vs_unemployment.png", width = 10, height = 5.6, dpi = 300)


# 5. Average unemployment by decade
decade_summary <- economics %>%
  group_by(decade) %>%
  summarise(
    avg_unemploy = mean(unemploy, na.rm = TRUE)
  )

ggplot(decade_summary, aes(x = factor(decade), y = avg_unemploy)) +
  geom_col() +
  geom_text(
    aes(label = comma(round(avg_unemploy))),
    vjust = -0.3,
    size = 3
  ) +
  labs(
    title = "Average Unemployment by Decade",
    x = "Decade",
    y = "Average unemployed people (thousands)"
  ) +
  theme_minimal()

ggsave("05_avg_unemployment_decade.png", width = 10, height = 5.6, dpi = 300)


# Simple statistics used in the interpretation
economics %>%
  summarise(
    observations = n(),
    start_date = min(date),
    end_date = max(date),
    average_unemploy = mean(unemploy),
    minimum_unemploy = min(unemploy),
    maximum_unemploy = max(unemploy),
    correlation_pce_unemploy = cor(pce, unemploy)
  )

economics %>%
  filter(unemploy == max(unemploy)) %>%
  select(date, unemploy)

economics %>%
  group_by(decade) %>%
  summarise(
    avg_unemploy = mean(unemploy),
    avg_pce = mean(pce),
    avg_psavert = mean(psavert)
  )
