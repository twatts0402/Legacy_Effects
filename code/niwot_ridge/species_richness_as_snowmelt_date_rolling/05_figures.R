# Create regression figures for current-year and rolling legacy snowmelt effects.

library(tidyverse)

output_dir <- "/Users/tobiahwatts/Desktop/SMART OUTPUTS/rolling_snowmelt_species-richness"

lag_model_data <- read_csv(
  file.path(output_dir, "03_snowmelt_species_richness_rolling_lag_model_data.csv"),
  show_col_types = FALSE
)

model_results <- read_csv(
  file.path(output_dir, "04_lag_model_results.csv"),
  show_col_types = FALSE
)

#creates a clean labeled data table with needed info and labels for lag_years
plot_data <- lag_model_data |>
  mutate(
    lag_label = case_when(
      lag_years == 0 ~ "Current year",
      lag_years == 1 ~ "Previous 1 year",
      TRUE ~ paste0("Previous ", lag_years, " years")),
    lag_label = factor(
      lag_label,
      levels = c("Current year", "Previous 1 year", "Previous 3 years", "Previous 5 years"))) #creates fixed display order

#creates plotting labels for model_results table
plot_labels <- model_results |>
  mutate(
    lag_label = case_when(
      lag_years == 0 ~ "Current year",
      lag_years == 1 ~ "Previous 1 year",
      TRUE ~ paste0("Previous ", lag_years, " years") #makes a nicer to read format name
    ),
    lag_label = factor(
      lag_label,
      levels = c("Current year", "Previous 1 year", "Previous 3 years", "Previous 5 years") #creates fixed display order
    ),
    label = paste0( #annotation label
      "R^2 = ", round(r_squared, 3),
      "\np = ", format.pval(p_value, digits = 3, eps = 0.001)))

#creates the 4-panel graphs =in one file, 4 graphs one for each lag
combined_plot <- plot_data |>
  ggplot(aes(x = rolling_mean_snowmelt_doy, y = species_richness)) +
  geom_point(color = "#2f6f73", alpha = 0.55, size = 1.4) +
  geom_smooth(method = "lm", se = TRUE, color = "#b23a48", linewidth = 0.9) +
  geom_text(
    data = plot_labels,
    aes(x = Inf, y = Inf, label = label),
    hjust = 1.1,
    vjust = 1.4,
    inherit.aes = FALSE,
    size = 3.8,
    color = "#222222"
  ) +
  facet_wrap(~ lag_label, ncol = 2) +
  labs(
    title = "Species richness by current and rolling legacy snowmelt date",
    x = "Rolling mean snowmelt date (day of year)",
    y = "Species richness"
  ) +
  theme_minimal(base_size = 13)

ggsave(
  filename = file.path(output_dir, "05_all_lags_regression.png"),
  plot = combined_plot,
  width = 11,
  height = 8,
  dpi = 200)

cat("Step 05 complete.\n")
cat("Figures written to:", output_dir, "\n")
