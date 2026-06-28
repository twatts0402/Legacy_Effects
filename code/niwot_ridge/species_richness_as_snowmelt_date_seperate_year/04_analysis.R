# Run linear model with separate current-year and previous-year snowmelt effects.

library(tidyverse)

output_dir <- '/Users/tobiahwatts/Desktop/SMART OUTPUTS/seperate_year_snowmelt_species-richness'

lag_model_data <- read_csv(
  file.path(output_dir, "03_snowmelt_species_richness_lag_predictor_model_data.csv"),
  show_col_types = FALSE
)
#takes all the columns from lag_model_data that end in '_doy' and saves them as c(###,###,###,...)
snowmelt_predictors <- lag_model_data |>
  select(ends_with("_doy")) |>
  names()
#takes those names and turns them into an LM so later can do an LM (model_formula, data = lag_model_data)
model_formula <- reformulate(
  termlabels = snowmelt_predictors,
  response = "species_richness")

#does as mentioned above
snowmelt_lag_model <- lm(model_formula, data = lag_model_data)
model_summary <- summary(snowmelt_lag_model)
coefficient_table <- model_summary$coefficients
confint_table <- confint(snowmelt_lag_model)

#creates a tidy table with coefficients and statistically significant values.
model_results <- as_tibble(coefficient_table, rownames = "term") |>
  rename(
    estimate = Estimate,
    std_error = `Std. Error`,
    t_value = `t value`,
    p_value = `Pr(>|t|)`
  ) |>
  mutate(
    n = nobs(snowmelt_lag_model),
    r_squared = model_summary$r.squared,
    adjusted_r_squared = model_summary$adj.r.squared,
    residual_standard_error = model_summary$sigma,
    conf_low = confint_table[term, 1],
    conf_high = confint_table[term, 2],
    .before = estimate)

#runs a variance inflation factor, testing for colinearity (if one predictor can predict another predictor)
calculate_vif <- function(data, predictors) {
  predictors |>
    map_dfr(\(predictor) {
      other_predictors <- setdiff(predictors, predictor)
      vif_formula <- reformulate(
        termlabels = other_predictors,
        response = predictor
      )
      vif_model <- lm(vif_formula, data = data)
      r_squared <- summary(vif_model)$r.squared

      tibble(
        term = predictor,
        vif = 1 / (1 - r_squared)
      )
    })}

#stores vif value in a variable. close to 1 is good.
vif_results <- calculate_vif(lag_model_data, snowmelt_predictors)

write_csv(
  model_results,
  file.path(output_dir, "04_lag_predictor_model_results.csv"))

write_csv(
  vif_results,
  file.path(output_dir, "04_lag_predictor_vif.csv"))

#writes plain text summary
summary_lines <- c(
  "Separate-year snowmelt lag model",
  paste("Matched plot-year observations:", nrow(lag_model_data)),
  paste("Model formula:", deparse(model_formula)),
  "",
  capture.output(model_summary),
  "",
  "95% confidence intervals:",
  capture.output(confint_table),
  "",
  "Variance inflation factors:",
  capture.output(vif_results))

writeLines(
  summary_lines,
  file.path(output_dir, "04_lag_predictor_model_summary.txt"))

cat("Step 04 complete.\n")
print(model_results)
cat("\nVariance inflation factors:\n")
print(vif_results)
cat("Outputs written to:", output_dir, "\n")
