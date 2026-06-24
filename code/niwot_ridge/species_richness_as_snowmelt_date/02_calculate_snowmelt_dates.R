# Calculate plot-year snowmelt dates from cleaned snow-depth observations.

library(tidyverse)

output_dir <- "/Users/tobiahwatts/Desktop/snowmelt_date_species_richness"
minimum_snowmelt_doy <- 100
maximum_snowmelt_doy <- 250

snow_depth <- read_csv(
  file.path(output_dir, "01_clean_snow_depth_by_plot_year.csv"),
  show_col_types = FALSE
)

snowpack_peaks <- snow_depth |>
  arrange(plot, snow_year, date) |>
  group_by(plot, snow_year) |>
  filter(snow_depth_cm == max(snow_depth_cm, na.rm = TRUE)) |>
  slice(1) |>
  ungroup() |>
  transmute(
    plot,
    snowmelt_year = snow_year,
    peak_date = date,
    peak_snow_depth_cm = snow_depth_cm
  )

# Snowmelt is only searched for after the seasonal snowpack peak. This prevents
# winter or early-season zero-snow observations from being counted as snowmelt
# when snow later accumulates.
snowmelt_dates <- snow_depth |>
  rename(snowmelt_year = snow_year) |>
  inner_join(snowpack_peaks, by = c("plot", "snowmelt_year")) |>
  filter(
    date >= peak_date,
    peak_snow_depth_cm > 0
  ) |>
  arrange(plot, snowmelt_year, date) |>
  group_by(plot, snowmelt_year) |>
  mutate(
    previous_date = lag(date),
    previous_snow_depth_cm = lag(snow_depth_cm),
    positive_to_zero = previous_snow_depth_cm > 0 & snow_depth_cm == 0
  ) |>
  filter(positive_to_zero) |>
  slice_max(date, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(
    crossing_fraction = if_else(
      snow_depth_cm == previous_snow_depth_cm,
      1,
      previous_snow_depth_cm / (previous_snow_depth_cm - snow_depth_cm)
    ),
    estimated_snowmelt_date = previous_date + round(as.numeric(date - previous_date) * crossing_fraction),
    day_of_year_snowmelt = yday(estimated_snowmelt_date),
    gap_days = as.numeric(date - previous_date),
    quality_flag = "good"
  ) |>
  filter(
    day_of_year_snowmelt >= minimum_snowmelt_doy,
    day_of_year_snowmelt <= maximum_snowmelt_doy
  ) |>
  select(
    plot,
    snowmelt_year,
    peak_date,
    peak_snow_depth_cm,
    previous_date,
    previous_snow_depth_cm,
    first_zero_date = date,
    first_zero_depth_cm = snow_depth_cm,
    estimated_snowmelt_date,
    day_of_year_snowmelt,
    gap_days,
    quality_flag
  ) |>
  arrange(snowmelt_year, plot)

write_csv(
  snowmelt_dates,
  file.path(output_dir, "02_calculated_snowmelt_dates.csv")
)

cat("Step 02 complete.\n")
cat("Snowmelt rows:", nrow(snowmelt_dates), "\n")
cat("Snowmelt DOY range kept:", minimum_snowmelt_doy, "to", maximum_snowmelt_doy, "\n")
cat("Minimum calculated snowmelt DOY:", min(snowmelt_dates$day_of_year_snowmelt), "\n")
cat("Maximum calculated snowmelt DOY:", max(snowmelt_dates$day_of_year_snowmelt), "\n")
cat("Outputs written to:", output_dir, "\n")
