# Build model-ready datasets with separate current-year and previous-year
# snowmelt predictors.

library(tidyverse)

output_dir <- '/Users/tobiahwatts/Desktop/SMART OUTPUTS/seperate_year_snowmelt_species-richness'
max_previous_years <- 5

species_richness <- read_csv(
  file.path(output_dir, "01_species_richness_by_plot_year.csv"),
  show_col_types = FALSE)

snowmelt_dates <- read_csv(
  file.path(output_dir, "02_calculated_snowmelt_dates.csv"),
  show_col_types = FALSE)

#creates names for the lag based on max_previous_years
lag_predictor_names <- c(
  "current_year_doy",
  paste0("prev_", seq_len(max_previous_years), "_year_doy"))

#builds data set for testing lag snowmelt date vs species richness
lag_model_data <- species_richness |>
  crossing(lag_years_previous = 0:max_previous_years) |>  #Duplicates each richness row once for every lag value.
  mutate(snowmelt_year = vegetation_year - lag_years_previous) |> #caluclates which snowmelt year matches which vegetation year
  inner_join(
    snowmelt_dates |>
      select(plot, snowmelt_year, day_of_year_snowmelt),
    by = c("plot", "snowmelt_year")
  ) |>
  #makes a readble label for lag snowmelt doy
  mutate(
    lag_predictor = if_else(
      lag_years_previous == 0,
      "current_year_doy",
      paste0("prev_", lag_years_previous, "_year_doy"))
  ) |>
  select(
    plot,
    vegetation_year,
    species_richness,
    lag_predictor,
    day_of_year_snowmelt
  ) |>
  #adds columns from lag_predictor above and then assigns values from day_of_year_snowmelt
  pivot_wider(
    names_from = lag_predictor,
    values_from = day_of_year_snowmelt
  ) |>
  drop_na(all_of(lag_predictor_names)) |>
  select(
    plot,
    vegetation_year,
    species_richness,
    all_of(lag_predictor_names)
  ) |>
  arrange(vegetation_year, plot)

#calculates sample sizes per lag
lag_counts <- lag_model_data |>
  summarize(
    max_previous_years = max_previous_years,
    matched_plot_years = n())

write_csv(
  lag_model_data,
  file.path(output_dir, "03_snowmelt_species_richness_lag_predictor_model_data.csv"))

write_csv(
  lag_counts,
  file.path(output_dir, "03_lag_predictor_model_sample_sizes.csv"))

cat("Step 03 complete.\n")
cat("Lag predictor definitions:\n")
cat("  current_year_doy = same-year snowmelt DOY\n")
seq_len(max_previous_years) |>
  walk(\(lag_years_previous) {
    cat(
      "  prev_",
      lag_years_previous,
      "_year_doy = snowmelt DOY ",
      lag_years_previous,
      " year(s) before the vegetation year\n",
      sep = ""
    )
  })
print(lag_counts)
cat("Outputs written to:", output_dir, "\n")
