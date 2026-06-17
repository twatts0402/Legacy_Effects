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

#turns dates into actual as.dates. Filters out non FUDI data.
# Groups by month and takes avgerage fudi biomass
#joins fudi_lagged w sst_monthyly and drops na values

fudi_lagged <- biomass |>
  mutate(year = as.integer(year), month = as.integer(month),
    sample_month = as.Date(sprintf("%04d-%02d-01", year, month)),
    lag_sst_month = as.Date(format(sample_month - 1, "%Y-%m-01"))) |>
  filter(biomass.species == "FUDI") |>
  group_by(sample_month, lag_sst_month) |>
  summarize(
    fudi_biomass = mean(biomass, na.rm = TRUE),
    .groups = "drop") |>
  left_join(
    sst_monthly,
    by = c("lag_sst_month" = "sst_month")
  ) |>
  drop_na(previous_month_sst, fudi_biomass)

#linear regression

fudi_lm <- lm(fudi_biomass ~ previous_month_sst, data = fudi_lagged)
fudi_lm_sumamry <- summary(fudi_lm)

r_squared <- fudi_lm_sumamry$r.squared
p_value <- fudi_lm_sumamry$coefficients["previous_month_sst", "Pr(>|t|)"]

#scatter plot with line of best fit, p-value and r^2 annotations.
#important comment

ggplot(fudi_lagged, aes(x = previous_month_sst, y = fudi_biomass)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "Previous Month Mean SST",
    y = "FUDI Biomass",
    title = "Lagged Effect of SST on FUDI Biomass"
  ) +
  annotate("text", x = Inf, y = Inf,
    label = paste(
      "R² =", round(r_squared, 3),
      "\np =", round(p_value, 4)
    ),
    hjust = 1.2,
    vjust = 1.2)
