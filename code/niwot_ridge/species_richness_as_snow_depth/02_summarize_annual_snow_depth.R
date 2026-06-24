library(tidyverse)

# Estimate plot-year snowmelt timing from the cleaned snow-depth observations.
# The main proxy is the first zero-depth observation after the final positive
# snow-depth observation in a calendar year. Because these are intermittent
# field observations, the script also saves the final positive snow date.

output_folder <- "/Users/tobiahwatts/Desktop/Snowpack_depth_vegetation_outputs"
snow_clean_path <- file.path(output_folder, "01_snow_clean.csv")

dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)

snow_clean <- read_csv(snow_clean_path, show_col_types = FALSE) |>
  mutate(date = as.Date(date))

last_positive_date <- function(depth, date) {
  positive_dates <- date[depth > 0]

  if (length(positive_dates) == 0) {
    return(as.Date(NA))
  }

  max(positive_dates)
}

first_zero_date <- function(depth, date) {
  zero_dates <- date[depth == 0]

  if (length(zero_dates) == 0) {
    return(as.Date(NA))
  }

  min(zero_dates)
}

first_zero_after_last_snow <- function(depth, date) {
  final_snow_date <- last_positive_date(depth, date)

  if (is.na(final_snow_date)) {
    return(as.Date(NA))
  }

  candidate_dates <- date[depth == 0 & date > final_snow_date]

  if (length(candidate_dates) == 0) {
    return(as.Date(NA))
  }

  min(candidate_dates)
}

snowmelt_dates <- snow_clean |>
  filter(!is.na(mean_depth)) |>
  group_by(plot, year) |>
  summarise(
    last_snow_observed_date = last_positive_date(mean_depth, date),
    first_snow_free_observed_date = first_zero_date(mean_depth, date),
    snowmelt_date = first_zero_after_last_snow(mean_depth, date),
    snowmelt_day_of_year = as.integer(format(snowmelt_date, "%j")),
    .groups = "drop"
  )

write_csv(snowmelt_dates, file.path(output_folder, "02_snowmelt_dates.csv"))

print(glimpse(snowmelt_dates))
