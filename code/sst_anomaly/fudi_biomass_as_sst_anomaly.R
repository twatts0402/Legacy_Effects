library(tidyverse)

biomass <- read_csv("/Users/tobiahwatts/Downloads/biomass_data_stacked.csv")

#filter biomass data to only include species fudi

fudi <- biomass |>
  filter(biomass.species == "FUDI") |>
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

# find the average biomass per month of fudi

monthly_fudi <- fudi |>
  group_by(year, month) |>
  summarise(
    mean_fudi_biomass = mean(biomass, na.rm = TRUE),
    .groups = "drop")

#link the two datasets by year and mont,

fudi_sst <- monthly_fudi |>
  left_join(monthly_sst, by = c("year", "month")) |>
  filter(!is.na(mean_fudi_biomass), !is.na(mean_sst))

#linear regression of fudi biomass vs SST anomaly

fudi_lm <- lm(mean_fudi_biomass ~ mean_sst, data = fudi_sst)
fudi_summary <- summary(fudi_lm)

r_squared <- fudi_summary$r.squared
p_value <- fudi_summary$coefficients["mean_sst", "Pr(>|t|)"]

#scatter plot with line of best fit, R^2, and p-value

ggplot(fudi_sst, aes(x = mean_sst, y = mean_fudi_biomass)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "Monthly mean SST Anomaly",
    y = "Mean FUDI biomass",
    title = "FUDI biomass vs SST Anomaly") +
  annotate("text", x = Inf, y = Inf,
    label = paste("R² =", round(r_squared, 3),
                "\np =", round(p_value, 4)),
    hjust = 1.2,
    vjust = 1.2)
