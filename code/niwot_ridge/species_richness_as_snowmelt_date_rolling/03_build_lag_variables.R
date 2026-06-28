# Build model-ready datasets for current-year and rolling legacy snowmelt effects.

library(tidyverse)

output_dir <- "/Users/tobiahwatts/Desktop/SMART OUTPUTS/rolling_snowmelt_species-richness"
lag_years_to_model <- c(0, 1, 3, 5)

species_richness <- read_csv(
  file.path(output_dir, "01_species_richness_by_plot_year.csv"),
  show_col_types = FALSE
)

snowmelt_dates <- read_csv(
  file.path(output_dir, "02_calculated_snowmelt_dates.csv"),
  show_col_types = FALSE)

#builds the dataset when lag_year=0, matches species richness to snowmelt date
build_same_year_snowmelt_dataset <- function(lag_years) {
  same_year_snowmelt <- snowmelt_dates |>
    transmute(
      plot,
      vegetation_year = snowmelt_year, #since 0 lag
      snowmelt_years_used = as.character(snowmelt_year),
      n_snowmelt_years_used = 1, #only current year
      rolling_mean_snowmelt_doy = day_of_year_snowmelt,
      rolling_sd_snowmelt_doy = NA_real_,
      rolling_min_snowmelt_doy = day_of_year_snowmelt,
      rolling_max_snowmelt_doy = day_of_year_snowmelt)

  species_richness |>
    inner_join(same_year_snowmelt, by = c("plot", "vegetation_year")) |>
    mutate(lag_years = .env$lag_years)
}

#makes dataset for lagged years
build_previous_year_snowmelt_dataset <- function(lag_years) {
  species_richness |>
    inner_join(snowmelt_dates, by = "plot", relationship = "many-to-many") |>
    filter(
      snowmelt_year >= vegetation_year - .env$lag_years, #keeps the previous vegetation years but the year itself.
      snowmelt_year <= vegetation_year - 1               #example: veg_year = 2010 lag_years = 3, keeps 07,08,09
    ) |>
    group_by(plot, vegetation_year, species_richness) |>
    summarize(
      lag_years = .env$lag_years,
      snowmelt_years_used = paste(sort(snowmelt_year), collapse = ", "),
      n_snowmelt_years_used = n_distinct(snowmelt_year),
      rolling_mean_snowmelt_doy = mean(day_of_year_snowmelt, na.rm = TRUE),
      rolling_sd_snowmelt_doy = sd(day_of_year_snowmelt, na.rm = TRUE),
      rolling_min_snowmelt_doy = min(day_of_year_snowmelt, na.rm = TRUE),
      rolling_max_snowmelt_doy = max(day_of_year_snowmelt, na.rm = TRUE),
      .groups = "drop"
    ) |>
    filter(n_snowmelt_years_used == .env$lag_years)
}

#helper function - creates a tidied dataset!
format_lag_dataset <- function(lag_dataset) {
  lag_dataset |>
    select(
      lag_years,
      plot,
      vegetation_year,
      species_richness,
      snowmelt_years_used,
      n_snowmelt_years_used,
      rolling_mean_snowmelt_doy,
      rolling_sd_snowmelt_doy,
      rolling_min_snowmelt_doy,
      rolling_max_snowmelt_doy
    ) |>
    arrange(lag_years, vegetation_year, plot)
}
#builds correct dataset based on lag, then formats
build_rolling_lag_dataset <- function(lag_years) {
  if (lag_years == 0) {
    build_same_year_snowmelt_dataset(lag_years)
  } else {
    build_previous_year_snowmelt_dataset(lag_years)
  } |>
    format_lag_dataset()}

#runs function above for each desired lag_year
lag_model_data <- lag_years_to_model |>
  map(build_rolling_lag_dataset) |>
  list_rbind() #makes one big data frame

write_csv(
  lag_model_data,
  file.path(output_dir, "03_snowmelt_species_richness_rolling_lag_model_data.csv"))

#writes csv for each lag
lag_years_to_model |>
  walk(\(lag_years) {
    lag_model_data |>
      filter(lag_years == !!lag_years) |>
      write_csv(file.path(output_dir, paste0("03_rolling_lag_", lag_years, "_model_data.csv")))})

#creates a csv for the sample size per lag
lag_counts <- lag_model_data |>
  count(lag_years, name = "matched_plot_years")

#creates a csv for the sample size per lag
write_csv(
  lag_counts,
  file.path(output_dir, "03_rolling_lag_model_sample_sizes.csv"))

cat("Step 03 complete.\n")
cat("Rolling lag definitions:\n")
cat("  lag 0 = same-year snowmelt DOY\n")
cat("  lag 1 = previous 1 year's snowmelt DOY\n")
cat("  lag 3 = mean snowmelt DOY across previous 1-3 years\n")
cat("  lag 5 = mean snowmelt DOY across previous 1-5 years\n")
print(lag_counts)
cat("Outputs written to:", output_dir, "\n")
