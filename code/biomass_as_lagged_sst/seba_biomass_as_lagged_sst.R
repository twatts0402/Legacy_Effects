library(tidyverse)

sst <- read_csv("/Users/tobiahwatts/Downloads/sst (1).csv")

biomass <- read_csv("/Users/tobiahwatts/Downloads/biomass_data_stacked.csv")

#group SST data by month and average daily values.

sst_monthly <- sst |>
  mutate(
    date = as.Date(date),
    sst_month = as.Date(format(date, "%Y-%m-01"))) |>
  group_by(sst_month) |>
  summarize(
    previous_month_sst = mean(sst, na.rm = TRUE),
    .groups = "drop")

#turns dates into actual as.dates. Filters out non-SEBA data.
# Groups by month and takes avgerage SEBA biomass
#joins seba_lagged w sst_monthyly and drops na values

seba_lagged <- biomass |>
  mutate(year = as.integer(year), month = as.integer(month),
    sample_month = as.Date(sprintf("%04d-%02d-01", year, month)),
    lag_sst_month = as.Date(format(sample_month - 1, "%Y-%m-01"))) |>
  filter(biomass.species == "SEBA") |>
  group_by(sample_month, lag_sst_month) |>
  summarize(
    seba_biomass = mean(biomass, na.rm = TRUE),
    .groups = "drop") |>
  left_join(
    sst_monthly,
    by = c("lag_sst_month" = "sst_month")
  ) |>
  drop_na(previous_month_sst, seba_biomass)

#linear regression

seba_lm <- lm(seba_biomass ~ previous_month_sst, data = seba_lagged)
seba_lm_sumamry <- summary(seba_lm)

r_squared <- seba_lm_sumamry$r.squared
p_value <- seba_lm_sumamry$coefficients["previous_month_sst", "Pr(>|t|)"]

#scatter plot with line of best fit, p-value and r^2 annotations.

ggplot(seba_lagged, aes(x = previous_month_sst, y = seba_biomass)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "Previous Month Mean SST",
    y = "SEBA Biomass",
    title = "Lagged Effect of SST on SEBA Biomass"
  ) +
  annotate("text", x = Inf, y = Inf,
    label = paste(
      "R² =", round(r_squared, 3),
      "\np =", round(p_value, 4)
    ),
    hjust = 1.2,
    vjust = 1.2)
