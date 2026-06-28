library(tidyverse)
library(broom)

# Test whether plant species richness and species relative cover are associated
# with current and lagged peak-winter snow depth. All lag years are included in
# the same model so each coefficient estimates the unique association of that
# year's snow depth while holding the other lag years constant.

output_folder <- '/Users/tobiahwatts/Desktop/SMART OUTPUTS/Snowpack_depth_vegetation_outputs'
vegetation_plot_year_path <- file.path(output_folder, "01_vegetation_plot_year.csv")
vegetation_species_path <- file.path(output_folder, "01_vegetation_species.csv")
snow_predictors_path <- file.path(output_folder, "02_snow_predictors.csv")

dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)

snow_predictors <- read_csv(snow_predictors_path, show_col_types = FALSE)
vegetation_plot_year <- read_csv(vegetation_plot_year_path, show_col_types = FALSE)
vegetation_species <- read_csv(vegetation_species_path, show_col_types = FALSE)

#identifies important species im wanting to do analysis on
selected_species <- c("ARSC", "FEBR", "GEROT", "POBI6", "MIOB2", 
                      "ERSI3", "DECE", "TRPAP", "POARG", "CAOC4")

# Lookup table used to keep lag IDs, model column names, and plot/table labels
snow_lag_predictors <- tribble(
  ~snow_lag, ~predictor, ~snow_lag_label,
  "current", "snow_depth_current_year", "Current year",
  "lag1", "snow_depth_1yr_prior", "1 year prior",
  "lag2", "snow_depth_2yr_prior", "2 years prior",
  "lag3", "snow_depth_3yr_prior", "3 years prior",
  "lag4", "snow_depth_4yr_prior", "4 years prior",
  "lag5", "snow_depth_5yr_prior", "5 years prior"
)

#creates a table with snow data and vegetation data for plot-year
richness_analysis_data <- vegetation_plot_year |>
  left_join(snow_predictors, by = c("plot", "year")) |>
  filter(!is.na(species_richness)) |>
  filter(if_all(all_of(snow_lag_predictors$predictor), ~ !is.na(.x)))

#performs lm 
fit_richness_model <- function(data) {
  model <- lm(
  species_richness ~ snow_depth_current_year +
    snow_depth_1yr_prior +
    snow_depth_2yr_prior +
    snow_depth_3yr_prior +
    snow_depth_4yr_prior +
    snow_depth_5yr_prior,
  data = data)

  #creates a tidy table with predictors and the statistical columns below
  model_results <- tidy(model) |>
    filter(term %in% snow_lag_predictors$predictor) |>
    left_join(snow_lag_predictors, by = c("term" = "predictor")) |>
    transmute(
      n_observations = nrow(data),
      snow_lag,
      snow_lag_label,
      term,
      coefficient = estimate,
      std_error = std.error,
      statistic = statistic,
      p_value = p.value,
      r_squared = glance(model)$r.squared,
      adjusted_r_squared = glance(model)$adj.r.squared
    )
}
#fits tibble to variable and and arranges in order of lag year
richness_models <- fit_richness_model(richness_analysis_data) |>
  arrange(factor(snow_lag, levels = snow_lag_predictors$snow_lag))

#creates table just for species of interest and joins with snow predictors.
species_analysis_data <- vegetation_species |>
  filter(USDA_code %in% selected_species) |>
  left_join(snow_predictors, by = c("plot", "year")) |>
  filter(!is.na(relative_cover)) |>
  filter(if_all(all_of(snow_lag_predictors$predictor), ~ !is.na(.x)))

#failsafe requiring there to be enough data for the model to run.
#There has to be a change in relative cover and snowdepth and more than 8 observations
# if one of these tests fail, it returns a tibble saying such
fit_species_model <- function(data) {
  if (nrow(data) < length(snow_lag_predictors$predictor) + 2 || n_distinct(data$relative_cover) < 2) {
    return(tibble(
      n_observations = nrow(data),
      snow_lag = snow_lag_predictors$snow_lag,
      snow_lag_label = snow_lag_predictors$snow_lag_label,
      term = snow_lag_predictors$predictor,
      coefficient = NA_real_,
      std_error = NA_real_,
      statistic = NA_real_,
      p_value = NA_real_,
      r_squared = NA_real_,
      adjusted_r_squared = NA_real_
    ))
  }
# lm of relative cover as an effect of snowdepth with prev years held constant
  model <- lm(
  relative_cover ~ snow_depth_current_year +
    snow_depth_1yr_prior +
    snow_depth_2yr_prior +
    snow_depth_3yr_prior +
    snow_depth_4yr_prior +
    snow_depth_5yr_prior,
  data = data)

#creates a tidy table with statistical columns below
  tidy(model) |>
    filter(term %in% snow_lag_predictors$predictor) |>
    left_join(snow_lag_predictors, by = c("term" = "predictor")) |>
    transmute(
      n_observations = nrow(data),
      snow_lag,
      snow_lag_label,
      term,
      coefficient = estimate,
      std_error = std.error,
      statistic = statistic,
      p_value = p.value,
      r_squared = glance(model)$r.squared,
      adjusted_r_squared = glance(model)$adj.r.squared)}

#matches table to variable and groups by each species.
#runs lm for each species indpendently (.x) and fits it to a richness model. 
#arranges by USDA_code and sorts by lag_year
species_models <- species_analysis_data |>
  group_by(USDA_code, USDA_name) |>
  group_modify(~ fit_species_model(.x)) |>
  ungroup() |>
  arrange(USDA_code, factor(snow_lag, levels = snow_lag_predictors$snow_lag))

write_csv(richness_analysis_data, file.path(output_folder, "03_richness_analysis_data.csv"))
write_csv(species_analysis_data, file.path(output_folder, "03_species_analysis_data.csv"))
write_csv(richness_models, file.path(output_folder, "03_richness_models.csv"))
write_csv(species_models, file.path(output_folder, "03_species_relative_cover_models.csv"))

print(richness_models)
print(species_models)
