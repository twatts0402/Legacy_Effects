library(tidyverse)

biomass <- read_csv("/Users/tobiahwatts/Downloads/biomass_data_stacked.csv")

#filter biomass data to only include species SEBA

seba <- biomass |>
  filter(biomass.species == "SEBA") |>
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

# find the average biomass per month of seba

monthly_seba <- seba |>
  group_by(year, month) |>
  summarise(
    mean_seba_biomass = mean(biomass, na.rm = TRUE),
    .groups = "drop")

#link the two datasets by year and mont,

seba_sst <- monthly_seba |>
  left_join(monthly_sst, by = c("year", "month")) |>
  filter(!is.na(mean_seba_biomass), !is.na(mean_sst))

#linear regression of seba biomass vs SST anomaly

seba_lm <- lm(mean_seba_biomass ~ mean_sst, data = seba_sst)
seba_summary <- summary(seba_lm)

r_squared <- seba_summary$r.squared
p_value <- seba_summary$coefficients["mean_sst", "Pr(>|t|)"]

#scatter plot with line of best fit, R^2, and p-value

ggplot(seba_sst, aes(x = mean_sst, y = mean_seba_biomass)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "Monthly mean SST Anomaly",
    y = "Mean SEBA biomass",
    title = "SEBA biomass vs SST Anomaly") +
  annotate("text", x = Inf, y = Inf,
    label = paste("R² =", round(r_squared, 3),
                "\np =", round(p_value, 4)),
    hjust = 1.2,
    vjust = 1.2)