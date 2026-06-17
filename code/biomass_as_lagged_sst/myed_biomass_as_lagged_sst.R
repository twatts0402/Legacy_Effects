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

#turns dates into actual as.dates. Filters out non-MYED data.
# Groups by month and takes avgerage MYED biomass
#joins myed_lagged w sst_monthyly and drops na values

myed_lagged <- biomass |>
  mutate(year = as.integer(year), month = as.integer(month),
    sample_month = as.Date(sprintf("%04d-%02d-01", year, month)),
    lag_sst_month = as.Date(format(sample_month - 1, "%Y-%m-01"))) |>
  filter(biomass.species == "MYED") |>
  group_by(sample_month, lag_sst_month) |>
  summarize(
    myed_biomass = mean(biomass, na.rm = TRUE),
    .groups = "drop") |>
  left_join(
    sst_monthly,
    by = c("lag_sst_month" = "sst_month")
  ) |>
  drop_na(previous_month_sst, myed_biomass)

#linear regression

myed_lm <- lm(myed_biomass ~ previous_month_sst, data = myed_lagged)
myed_lm_sumamry <- summary(myed_lm)

r_squared <- myed_lm_sumamry$r.squared
p_value <- myed_lm_sumamry$coefficients["previous_month_sst", "Pr(>|t|)"]

#scatter plot with line of best fit, p-value and r^2 annotations.

ggplot(myed_lagged, aes(x = previous_month_sst, y = myed_biomass)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "Previous Month Mean SST",
    y = "MYED Biomass",
    title = "Lagged Effect of SST on MYED Biomass"
  ) +
  annotate("text", x = Inf, y = Inf,
    label = paste(
      "R² =", round(r_squared, 3),
      "\np =", round(p_value, 4)
    ),
    hjust = 1.2,
    vjust = 1.2)
