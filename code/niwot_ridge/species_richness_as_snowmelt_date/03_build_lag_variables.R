# Build model-ready datasets for current-year and rolling legacy snowmelt effects.

library(tidyverse)

output_dir <- "/Users/tobiahwatts/Desktop/snow_vegetation_outputs"
lag_years_to_model <- c(0, 1, 3, 5)

species_richness <- read_csv(
  file.path(output_dir, "01_species_richness_by_plot_year.csv"),
  show_col_types = FALSE
)

snowmelt_dates <- read_csv(
  file.path(output_dir, "02_calculated_snowmelt_dates.csv"),
  show_col_types = FALSE
)

build_rolling_lag_dataset <- function(lag_years) {
  if (lag_years == 0) {
    species_richness |>
      inner_join(
        snowmelt_dates |>
          transmute(
            plot,
            vegetation_year = snowmelt_year,
            snowmelt_years_used = as.character(snowmelt_year),
            n_snowmelt_years_used = 1,
            rolling_mean_snowmelt_doy = day_of_year_snowmelt,
            rolling_sd_snowmelt_doy = NA_real_,
            rolling_min_snowmelt_doy = day_of_year_snowmelt,
            rolling_max_snowmelt_doy = day_of_year_snowmelt
          ),
        by = c("plot", "vegetation_year")
      ) |>
      mutate(lag_years = lag_years)
  } else {
    species_richness |>
      inner_join(snowmelt_dates, by = "plot", relationship = "many-to-many") |>
      filter(
        snowmelt_year >= vegetation_year - lag_years,
        snowmelt_year <= vegetation_year - 1
      ) |>
      group_by(plot, vegetation_year, species_richness) |>
      summarize(
        lag_years = lag_years,
        snowmelt_years_used = paste(sort(snowmelt_year), collapse = ", "),
        n_snowmelt_years_used = n_distinct(snowmelt_year),
        rolling_mean_snowmelt_doy = mean(day_of_year_snowmelt, na.rm = TRUE),
        rolling_sd_snowmelt_doy = sd(day_of_year_snowmelt, na.rm = TRUE),
        rolling_min_snowmelt_doy = min(day_of_year_snowmelt, na.rm = TRUE),
        rolling_max_snowmelt_doy = max(day_of_year_snowmelt, na.rm = TRUE),
        .groups = "drop"
      ) |>
      filter(n_snowmelt_years_used == lag_years)
  } |>
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

lag_model_data <- lag_years_to_model |>
  map(build_rolling_lag_dataset) |>
  list_rbind()

write_csv(
  lag_model_data,
  file.path(output_dir, "03_snowmelt_species_richness_rolling_lag_model_data.csv")
)

lag_years_to_model |>
  walk(\(lag_years) {
    lag_model_data |>
      filter(lag_years == !!lag_years) |>
      write_csv(file.path(output_dir, paste0("03_rolling_lag_", lag_years, "_model_data.csv")))
  })

lag_counts <- lag_model_data |>
  count(lag_years, name = "matched_plot_years")

write_csv(
  lag_counts,
  file.path(output_dir, "03_rolling_lag_model_sample_sizes.csv")
)

cat("Step 03 complete.\n")
cat("Rolling lag definitions:\n")
cat("  lag 0 = same-year snowmelt DOY\n")
cat("  lag 1 = previous 1 year's snowmelt DOY\n")
cat("  lag 3 = mean snowmelt DOY across previous 1-3 years\n")
cat("  lag 5 = mean snowmelt DOY across previous 1-5 years\n")
print(lag_counts)
cat("Outputs written to:", output_dir, "\n")
