library(tidyverse)

# Build rolling mean snow-depth predictors ending in the vegetation sampling
# year. A 0-year window is the current year's snow depth only. A 1-year window
# averages the current year and one previous year, a 3-year window averages the
# current year and three previous years, and a 5-year window averages the current
# year and five previous years.

output_folder <- '/Users/tobiahwatts/Desktop/SMART OUTPUTS/Snowpack_depth_vegetation_outputs'
snow_annual_path <- file.path(output_folder, "01_snow_annual.csv")

dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)

snow_annual <- read_csv(snow_annual_path, show_col_types = FALSE)

rolling_mean <- function(depth, year, current_year, years_back) {
  window_years <- current_year - seq(0, years_back)
  window_depth <- depth[year %in% window_years]

  if (length(window_depth) != length(window_years) || any(is.na(window_depth))) {
    return(NA_real_)
  }

  mean(window_depth)
}

snow_predictors <- snow_annual |>
  select(plot, year, mean_annual_snow_depth, snow_measurement_count) |>
  group_by(plot) |>
  complete(year = full_seq(year, period = 1)) |>
  arrange(year, .by_group = TRUE) |>
  mutate(
    snow_depth_0yr_mean = mean_annual_snow_depth,
    snow_depth_1yr_mean = map_dbl(year, ~ rolling_mean(mean_annual_snow_depth, year, .x, 1)),
    snow_depth_3yr_mean = map_dbl(year, ~ rolling_mean(mean_annual_snow_depth, year, .x, 3)),
    snow_depth_5yr_mean = map_dbl(year, ~ rolling_mean(mean_annual_snow_depth, year, .x, 5))
  ) |>
  ungroup()

write_csv(snow_predictors, file.path(output_folder, "02_snow_predictors.csv"))

print(glimpse(snow_predictors))
