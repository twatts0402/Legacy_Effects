library(tidyverse)

biomass <- read_csv("/Users/tobiahwatts/Downloads/biomass_data_stacked.csv")

#filter biomass data to only include species MYED

myed <- biomass |>
  filter(biomass.species == "MYED") |>
  mutate(year = as.integer(year), month = as.integer(month)) |> 
    select(year, month, biomass.species, biomass, everything())

sst <- read_csv("/Users/tobiahwatts/Downloads/sst.csv")

#find monthly SST anomaly averages

monthly_sst <- sst |>
  mutate(
    year = as.integer(year),
    month = as.integer(month)) |>
  group_by(year, month) |>
  summarise(
    mean_sst = mean(sst, na.rm = TRUE),
    .groups = "drop")

# find the average biomass per month of myed

monthly_myed <- myed |>
  group_by(year, month) |>
  summarise(
    mean_myed_biomass = mean(biomass, na.rm = TRUE),
    .groups = "drop")

#link the two datasets by year and mont,

myed_sst <- monthly_myed |>
  left_join(monthly_sst, by = c("year", "month")) |>
  filter(!is.na(mean_myed_biomass), !is.na(mean_sst))

#linear regression of myed biomass vs SST anomaly

myed_lm <- lm(mean_myed_biomass ~ mean_sst, data = myed_sst)
myed_summary <- summary(myed_lm)

r_squared <- myed_summary$r.squared
p_value <- myed_summary$coefficients["mean_sst", "Pr(>|t|)"]

#scatter plot with line of best fit, R^2, and p-value

ggplot(myed_sst, aes(x = mean_sst, y = mean_myed_biomass)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "Monthly mean SST Anomaly",
    y = "Mean MYED biomass",
    title = "MYED biomass vs SST Anomaly") +
  annotate("text", x = Inf, y = Inf,
    label = paste("R² =", round(r_squared, 3),
                "\np =", round(p_value, 4)),
    hjust = 1.2,
    vjust = 1.2)

