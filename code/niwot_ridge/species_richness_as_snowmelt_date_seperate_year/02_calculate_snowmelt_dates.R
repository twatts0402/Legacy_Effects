# Calculate plot-year snowmelt dates from cleaned snow-depth observations.

library(tidyverse)

output_dir <- "/Users/tobiahwatts/Desktop/SMART OUTPUTS/seperate_year_snowmelt_species-richness"

#values outside of these are errors
minimum_snowmelt_doy <- 100
maximum_snowmelt_doy <- 250

snow_depth <- read_csv(
  file.path(output_dir, "01_clean_snow_depth_by_plot_year.csv"),
  show_col_types = FALSE)

#new table with snowpack max depth per year
snowpack_peaks <- snow_depth |>
  arrange(plot, snow_year, date) |>
  group_by(plot, snow_year) |>
  filter(snow_depth_cm == max(snow_depth_cm, na.rm = TRUE)) |>
  slice(1) |> #keeps 1 row per year
  ungroup() |>
  transmute(
    plot,
    snowmelt_year = snow_year,
    peak_date = date,
    peak_snow_depth_cm = snow_depth_cm)

# creates a new table with all previous information, estimates snowmelt date as halfway between observations
snowmelt_dates <- snow_depth |>
  rename(snowmelt_year = snow_year) |>
  inner_join(snowpack_peaks, by = c("plot", "snowmelt_year")) |>
  filter(
    date >= peak_date,     # when estimating snowmelt date, only use days after peak_date
    peak_snow_depth_cm > 0
  ) |>
  arrange(plot, snowmelt_year, date) |>
  group_by(plot, snowmelt_year) |>
  mutate(
    last_snow_present_date = lag(date),
    last_snow_present_depth_cm = lag(snow_depth_cm),
    positive_to_zero = last_snow_present_depth_cm > 0 & snow_depth_cm == 0
  ) |>
  filter(positive_to_zero) |>
  slice_max(date, n = 1, with_ties = FALSE) |> #uses most-recent 0 snow. If plot were to clear then be snowed on again. 
  ungroup() |>
  mutate(
    first_snow_free_date = date,
    first_snow_free_depth_cm = snow_depth_cm,
    snowmelt_window_days = as.numeric(first_snow_free_date - last_snow_present_date),
    estimated_snowmelt_date = last_snow_present_date + round(snowmelt_window_days / 2), #date halfway between last measured snow date and first 0 snow date
    day_of_year_snowmelt = yday(estimated_snowmelt_date),
    quality_flag = case_when(                 # sets quality flags if days between is too high
      snowmelt_window_days <= 25 ~ "good",
      snowmelt_window_days <= 35 ~ "moderate",
      TRUE ~ "uncertain")
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
    last_snow_present_date,
    last_snow_present_depth_cm,
    first_snow_free_date,
    first_snow_free_depth_cm,
    estimated_snowmelt_date,
    day_of_year_snowmelt,
    snowmelt_window_days,
    quality_flag) |>
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
