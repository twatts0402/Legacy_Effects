library(tidyverse)

# Build individual annual peak-winter snow-depth lag predictors ending in the
# vegetation sampling year. Each lag remains a separate predictor rather than
# being averaged with other years.

output_folder <- '/Users/tobiahwatts/Desktop/SMART OUTPUTS/Snowpack_depth_vegetation_outputs'
snow_annual_path <- file.path(output_folder, "01_snow_annual.csv")

dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)

snow_annual <- read_csv(snow_annual_path, show_col_types = FALSE)

#creates data table with lagged snow depths
snow_predictors <- snow_annual |>
  select(plot, year, mean_peak_winter_snow_depth, snow_measurement_count) |>
  group_by(plot) |>
  complete(year = full_seq(year, period = 1)) |>
  arrange(year, .by_group = TRUE) |>
  mutate(
    snow_depth_current_year = mean_peak_winter_snow_depth,
    snow_depth_1yr_prior = lag(mean_peak_winter_snow_depth, 1),
    snow_depth_2yr_prior = lag(mean_peak_winter_snow_depth, 2),
    snow_depth_3yr_prior = lag(mean_peak_winter_snow_depth, 3),
    snow_depth_4yr_prior = lag(mean_peak_winter_snow_depth, 4),
    snow_depth_5yr_prior = lag(mean_peak_winter_snow_depth, 5)
  ) |>
  ungroup()

write_csv(snow_predictors, file.path(output_folder, "02_snow_predictors.csv"))

print(glimpse(snow_predictors))
