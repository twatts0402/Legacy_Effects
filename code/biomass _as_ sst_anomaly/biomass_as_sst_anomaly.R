library(tidyverse)

biomass_data <- read_csv("/Users/tobiahwatts/Downloads/biomass_data_stacked.csv")

#isolated control plots only. Removed columns of unneeded information.
#created a column for total_biomass in each site for each date
#added an as.date column 

biomass_control_tidy <- biomass_data |>
  filter(plot == "C") |>
  mutate(year = as.integer(year),
        month = as.integer(month)) |> 
  group_by(year, month, survey_group, site, plot) |>
  summarise(
    total_biomass = first(plot_biomass),
    .groups = "drop") |>
  mutate(date = as.Date(paste(year, month, "01", sep = "-")))

sst_var_data <-  read_csv('/Users/tobiahwatts/Downloads/sst.csv')

#averages monthly SST values to create one row per month, lists days per month

sst_data_tidy <- sst_var_data |> 
  group_by(year, month) |> 
   summarise(
    mean_var_sst = mean(sst),
    n_days = n(),
    .groups = "drop")

#joined the biomass and SST data by year and month, organized the table by better columns

biomass_sst <- biomass_control_tidy |> 
  left_join(sst_data_tidy, by = c('year', 'month')) |> 
  select(year, month, total_biomass, mean_var_sst, everything())

#create the linear regression model and its summary

model <- lm(total_biomass ~ mean_var_sst, data = biomass_sst)
model_summary <- summary(model)

r_squared <- model_summary$r.squared
p_value <- model_summary$coefficients["mean_var_sst", "Pr(>|t|)"]

#plot the data points with a lobf. include R^2 and p-values in upper right corner

ggplot(biomass_sst, aes(x = mean_var_sst, y = total_biomass)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = 'SST Anomaly (°C)', y = 'Total Biomass (g)') +
  annotate("text", x = Inf, y = Inf,
    label = paste("R² =", round(r_squared, 3),
                "\np =", round(p_value, 4)),
    hjust = 1.2,
    vjust = 1.2)

