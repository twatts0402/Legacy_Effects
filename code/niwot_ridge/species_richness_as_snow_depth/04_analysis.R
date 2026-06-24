library(tidyverse)
library(broom)

# Test whether plant species richness and species relative cover are associated
# with rolling mean snowpack depth across current-year, 1-year, 3-year, and
# 5-year snow-history windows.

output_folder <- "/Users/tobiahwatts/Desktop/Snowpack_depth_vegetation_outputs"
vegetation_plot_year_path <- file.path(output_folder, "01_vegetation_plot_year.csv")
vegetation_species_path <- file.path(output_folder, "01_vegetation_species.csv")
snow_predictors_path <- file.path(output_folder, "03_snow_predictors.csv")

dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)

snow_predictors <- read_csv(snow_predictors_path, show_col_types = FALSE)
vegetation_plot_year <- read_csv(vegetation_plot_year_path, show_col_types = FALSE)
vegetation_species <- read_csv(vegetation_species_path, show_col_types = FALSE)

snow_windows <- tribble(
  ~snow_window, ~predictor, ~snow_window_label,
  "0yr", "snow_depth_0yr_mean", "Current year",
  "1yr", "snow_depth_1yr_mean", "Current + 1 previous year",
  "3yr", "snow_depth_3yr_mean", "Current + 3 previous years",
  "5yr", "snow_depth_5yr_mean", "Current + 5 previous years"
)

richness_analysis_data <- vegetation_plot_year |>
  left_join(snow_predictors, by = c("plot", "year")) |>
  pivot_longer(
    cols = all_of(snow_windows$predictor),
    names_to = "predictor",
    values_to = "snow_depth_mean"
  ) |>
  left_join(snow_windows, by = "predictor") |>
  filter(!is.na(snow_depth_mean), !is.na(species_richness))

fit_richness_model <- function(data) {
  model <- lm(species_richness ~ snow_depth_mean, data = data)

  tidy(model) |>
    filter(term == "snow_depth_mean") |>
    transmute(
      n_observations = nrow(data),
      slope = estimate,
      std_error = std.error,
      statistic = statistic,
      p_value = p.value,
      r_squared = glance(model)$r.squared,
      adjusted_r_squared = glance(model)$adj.r.squared
    )
}

richness_models <- richness_analysis_data |>
  group_by(snow_window, snow_window_label) |>
  group_modify(~ fit_richness_model(.x)) |>
  ungroup() |>
  arrange(snow_window)

species_analysis_data <- vegetation_species |>
  left_join(snow_predictors, by = c("plot", "year")) |>
  pivot_longer(
    cols = all_of(snow_windows$predictor),
    names_to = "predictor",
    values_to = "snow_depth_mean"
  ) |>
  left_join(snow_windows, by = "predictor") |>
  filter(!is.na(snow_depth_mean), !is.na(relative_cover))

fit_species_model <- function(data) {
  if (nrow(data) < 10 || n_distinct(data$relative_cover) < 2) {
    return(tibble(
      n_observations = nrow(data),
      slope = NA_real_,
      std_error = NA_real_,
      statistic = NA_real_,
      p_value = NA_real_,
      r_squared = NA_real_,
      adjusted_r_squared = NA_real_
    ))
  }

  model <- lm(relative_cover ~ snow_depth_mean, data = data)

  tidy(model) |>
    filter(term == "snow_depth_mean") |>
    transmute(
      n_observations = nrow(data),
      slope = estimate,
      std_error = std.error,
      statistic = statistic,
      p_value = p.value,
      r_squared = glance(model)$r.squared,
      adjusted_r_squared = glance(model)$adj.r.squared
    )
}

species_models <- species_analysis_data |>
  group_by(snow_window, snow_window_label, USDA_code, USDA_name) |>
  group_modify(~ fit_species_model(.x)) |>
  ungroup() |>
  arrange(snow_window, p_value)

write_csv(richness_analysis_data, file.path(output_folder, "04_richness_analysis_data.csv"))
write_csv(species_analysis_data, file.path(output_folder, "04_species_analysis_data.csv"))
write_csv(richness_models, file.path(output_folder, "04_richness_models.csv"))
write_csv(species_models, file.path(output_folder, "04_species_relative_cover_models.csv"))

print(richness_models)
print(species_models)
