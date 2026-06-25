# Create figures for separate current-year and previous-year snowmelt effects.

library(tidyverse)

output_dir <- '/Users/tobiahwatts/Desktop/SMART OUTPUTS/seperate_year_snowmelt_species-richness'

lag_model_data <- read_csv(
  file.path(output_dir, "03_snowmelt_species_richness_lag_predictor_model_data.csv"),
  show_col_types = FALSE
)

model_results <- read_csv(
  file.path(output_dir, "04_lag_predictor_model_results.csv"),
  show_col_types = FALSE
)

snowmelt_predictors <- lag_model_data |>
  select(ends_with("_doy")) |>
  names()

predictor_labels <- tibble(term = snowmelt_predictors) |>
  mutate(
    predictor_label = case_when(
      term == "current_year_doy" ~ "Current year",
      TRUE ~ term |>
        str_remove("_year_doy") |>
        str_replace("prev_", "Previous ") |>
        str_replace("_", " ") |>
        paste("year")
    ),
    predictor_label = factor(predictor_label, levels = predictor_label)
  )

plot_data <- lag_model_data |>
  pivot_longer(
    cols = all_of(snowmelt_predictors),
    names_to = "term",
    values_to = "snowmelt_doy"
  ) |>
  left_join(predictor_labels, by = "term")

scatter_plot <- plot_data |>
  ggplot(aes(x = snowmelt_doy, y = species_richness)) +
  geom_point(color = "#2f6f73", alpha = 0.55, size = 1.4) +
  geom_smooth(method = "lm", se = TRUE, color = "#b23a48", linewidth = 0.9) +
  facet_wrap(~ predictor_label, ncol = 2) +
  labs(
    title = "Species richness by snowmelt date for each lag year",
    x = "Snowmelt date (day of year)",
    y = "Species richness"
  ) +
  theme_minimal(base_size = 13)

ggsave(
  filename = file.path(output_dir, "05_lag_predictor_scatter.png"),
  plot = scatter_plot,
  width = 11,
  height = 8,
  dpi = 200
)

coefficient_plot_data <- model_results |>
  filter(term != "(Intercept)") |>
  left_join(predictor_labels, by = "term") |>
  mutate(
    predictor_label = fct_rev(predictor_label),
    p_label = paste0("p = ", format.pval(p_value, digits = 3, eps = 0.001))
  )

coefficient_plot <- coefficient_plot_data |>
  ggplot(aes(x = estimate, y = predictor_label)) +
  geom_vline(xintercept = 0, color = "#666666", linetype = "dashed") +
  geom_errorbar(
    aes(xmin = conf_low, xmax = conf_high),
    width = 0.18,
    color = "#2f6f73",
    linewidth = 0.9
  ) +
  geom_point(color = "#b23a48", size = 2.8) +
  geom_text(
    aes(label = p_label),
    nudge_y = 0.28,
    size = 3.6,
    color = "#222222"
  ) +
  labs(
    title = "Separate-year snowmelt effects from the multivariable model",
    x = "Estimated change in species richness per snowmelt DOY",
    y = NULL
  ) +
  theme_minimal(base_size = 13)

ggsave(
  filename = file.path(output_dir, "05_lag_predictor_coefficients.png"),
  plot = coefficient_plot,
  width = 9,
  height = 6,
  dpi = 200
)

cat("Step 05 complete.\n")
cat("Figures written to:", output_dir, "\n")
