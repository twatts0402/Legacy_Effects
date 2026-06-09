library(tidyverse)

#the purpose of this program is to find a correlation of SEBA biomass to previous months sst anomaly

biomass <- read_csv("/Users/tobiahwatts/Downloads/biomass_data_stacked.csv")

# Get monthly SEBA biomass
seba_biomass <- biomass |> 
  mutate(month_date = as.Date(paste(year, month, "01", sep = "-"))) |> 
  filter(biomass.species == "SEBA") |> 
  group_by(month_date) |> 
  summarise(total_seba_biomass = sum(biomass, na.rm = TRUE))

sst <- read_csv("/Users/tobiahwatts/Downloads/sst.csv")

# Turn daily SST into monthly SST
monthly_sst <- sst |> 
  mutate(date = as.Date(date),
         month_date = as.Date(format(date, "%Y-%m-01"))) |> 
  group_by(month_date) |> 
  summarise(mean_sst = mean(sst, na.rm = TRUE))

# sort sst by date and then add a column for lagged sst
monthly_sst <- monthly_sst |> 
  arrange(month_date) |> 
  mutate(previous_month_sst = lag(mean_sst))


# Join SEBA biomass with previous month SST gets rid of rows w/ na values
seba_lagged <- seba_biomass |> 
  left_join(
    monthly_sst |> select(month_date, previous_month_sst),
    by = "month_date") |> 
  filter(!is.na(previous_month_sst))

# performs a linear regression are puts R^2 and p_values in variables
model <- lm(total_seba_biomass ~ previous_month_sst, data = seba_lagged)
model_summary <- summary(model)

r_squared <- model_summary$r.squared
p_value <- model_summary$coefficients["previous_month_sst", "Pr(>|t|)"]

# Creates a scatterplot with a line of best fit and annotates the R^2 and p values in top right
ggplot(seba_lagged,
       aes(x = previous_month_sst,
           y = total_seba_biomass)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "SEBA Biomass vs Previous Month SST Anomaly",
    x = "Previous Month Sea Surface Temperature Anomaly (°C)",
    y = "Total SEBA Biomass") +
  annotate("text", x = Inf, y = Inf,
    label = paste("R² =", round(r_squared, 3),
                  "\np =", round(p_value, 4)),
    hjust = 1.2,
    vjust = 1.2)
