library(tidyverse)

sst <- read_csv("/Users/tobiahwatts/Downloads/sst (1).csv")

biomass <- read_csv("/Users/tobiahwatts/Downloads/biomass_data_stacked.csv")

#group SST data by month and average daily values.

monthly_sst <- sst |>
  mutate(
    date = as.Date(date),
    sst_month = as.Date(format(date, "%Y-%m-01"))) |>
  group_by(sst_month) |>
  summarise(
    mean_sst = mean(sst, na.rm = TRUE),
    .groups = "drop")

#turns dates into as.dates. creates column gro lagged sst data
#compounds rows so only 1 row per month, averages plot biomass data

monthly_total_biomass <- biomass |>
  mutate(
    month = as.integer(month),
    year = as.integer(year),
    sample_month = as.Date(sprintf("%04d-%02d-01", year, month)),
    lag_sst_month = as.Date(format(sample_month - 1, "%Y-%m-01"))) |>
  distinct(
    site,
    plot,
    treatment_status,
    survey_group,
    sample_month,
    lag_sst_month,
    plot_biomass) |>
  group_by(sample_month, lag_sst_month) |>
  summarise(
    mean_total_biomass = mean(plot_biomass, na.rm = TRUE),
    .groups = "drop")

#joins monthly_ssy w monthly_total_biomass, drops na values

lagged_data <- monthly_total_biomass |>
  left_join(monthly_sst,
    by = c("lag_sst_month" = "sst_month")) |>
  drop_na(mean_total_biomass, mean_sst)

#linear regression

sst_total_lm <- lm(mean_total_biomass ~ mean_sst, data = lagged_data)
sst_total_summary <- summary(sst_total_lm)

r_squared <- sst_total_summary$r.squared
p_value <- sst_total_summary$coefficients["mean_sst", "Pr(>|t|)"]

#scatter plot with line of best fit and annotated r^2 and p-values.

ggplot(lagged_data, aes(x = mean_sst, y = mean_total_biomass)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "Mean SST during previous month",
    y = "Mean monthly total plot biomass",
    title = "Lagged effect of SST on mean monthly total biomass") +
  annotate("text", x = Inf, y = Inf,
    label = paste(
      "R² =", round(r_squared, 3),
      "\np =", round(p_value, 4)),
    hjust = 1.2,
    vjust = 1.2)