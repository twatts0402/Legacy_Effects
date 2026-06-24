# Run linear models for current-year and rolling legacy snowmelt effects.

library(tidyverse)

output_dir <- "/Users/tobiahwatts/Desktop/snowmelt_date_species_richness"

lag_model_data <- read_csv(
  file.path(output_dir, "03_snowmelt_species_richness_rolling_lag_model_data.csv"),
  show_col_types = FALSE
)

fit_one_lag <- function(data) {
  model <- lm(species_richness ~ rolling_mean_snowmelt_doy, data = data)
  model_summary <- summary(model)
  coefficient_table <- model_summary$coefficients
  confint_table <- confint(model)

  tibble(
    lag_years = first(data$lag_years),
    n = nrow(data),
    intercept = coefficient_table["(Intercept)", "Estimate"],
    slope = coefficient_table["rolling_mean_snowmelt_doy", "Estimate"],
    slope_std_error = coefficient_table["rolling_mean_snowmelt_doy", "Std. Error"],
    slope_t_value = coefficient_table["rolling_mean_snowmelt_doy", "t value"],
    p_value = coefficient_table["rolling_mean_snowmelt_doy", "Pr(>|t|)"],
    r_squared = model_summary$r.squared,
    adjusted_r_squared = model_summary$adj.r.squared,
    residual_standard_error = model_summary$sigma,
    slope_conf_low = confint_table["rolling_mean_snowmelt_doy", 1],
    slope_conf_high = confint_table["rolling_mean_snowmelt_doy", 2]
  )
}

model_results <- lag_model_data |>
  group_by(lag_years) |>
  group_split() |>
  map(fit_one_lag) |>
  list_rbind() |>
  arrange(lag_years)

write_csv(
  model_results,
  file.path(output_dir, "04_lag_model_results.csv")
)

summary_lines <- lag_model_data |>
  group_by(lag_years) |>
  group_split() |>
  map(\(data) {
    lag_years <- first(data$lag_years)
    model <- lm(species_richness ~ rolling_mean_snowmelt_doy, data = data)
    c(
      paste0("Rolling lag ", lag_years, " year(s)"),
      paste("Matched plot-year observations:", nrow(data)),
      "Model formula: species_richness ~ rolling_mean_snowmelt_doy",
      "",
      capture.output(summary(model)),
      "",
      "95% confidence intervals:",
      capture.output(confint(model)),
      "",
      strrep("-", 72),
      ""
    )
  }) |>
  unlist()

writeLines(
  summary_lines,
  file.path(output_dir, "04_lag_model_summaries.txt")
)

cat("Step 04 complete.\n")
print(model_results)
cat("Outputs written to:", output_dir, "\n")
