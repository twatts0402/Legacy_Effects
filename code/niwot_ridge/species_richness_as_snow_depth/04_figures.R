library(tidyverse)

# Create summary figures for the peak-winter snow-depth lag analysis.

output_folder <- '/Users/tobiahwatts/Desktop/SMART OUTPUTS/Snowpack_depth_vegetation_outputs'
richness_analysis_data_path <- file.path(output_folder, "03_richness_analysis_data.csv")
species_analysis_data_path <- file.path(output_folder, "03_species_analysis_data.csv")
richness_models_path <- file.path(output_folder, "03_richness_models.csv")

dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)

richness_analysis_data <- read_csv(richness_analysis_data_path, show_col_types = FALSE)
species_analysis_data <- read_csv(species_analysis_data_path, show_col_types = FALSE)
richness_models <- read_csv(richness_models_path, show_col_types = FALSE)

#different lag years im doing
lag_levels <- c("current", "lag1", "lag2", "lag3", "lag4", "lag5")

# Lookup table used to keep lag IDs, model column names, and plot/table labels
snow_lag_predictors <- tribble(
  ~snow_lag, ~predictor, ~snow_lag_label,
  "current", "snow_depth_current_year", "Current year",
  "lag1", "snow_depth_1yr_prior", "1 year prior",
  "lag2", "snow_depth_2yr_prior", "2 years prior",
  "lag3", "snow_depth_3yr_prior", "3 years prior",
  "lag4", "snow_depth_4yr_prior", "4 years prior",
  "lag5", "snow_depth_5yr_prior", "5 years prior")

#new table with columns from the above tibble. Orders snow_lag and snow_lag_label by year
richness_plot_data <- richness_analysis_data |>
  pivot_longer(
    cols = all_of(snow_lag_predictors$predictor),
    names_to = "predictor",
    values_to = "snow_depth"
  ) |>
  left_join(snow_lag_predictors, by = "predictor") |>
  mutate(
    snow_lag = factor(snow_lag, levels = lag_levels),
    snow_lag_label = factor(snow_lag_label, levels = snow_lag_predictors$snow_lag_label)
  )

#creates graph annotations such as slope and pvalue
richness_annotations <- richness_models |>
  mutate(
    snow_lag = factor(snow_lag, levels = lag_levels),
    snow_lag_label = factor(snow_lag_label, levels = snow_lag_predictors$snow_lag_label),
    label = paste0(
      "slope = ", signif(coefficient, 3),
      "\np = ", signif(p_value, 3))
  )

#creates plot for species richness vs snow depth
richness_plot <- ggplot(richness_plot_data, aes(x = snow_depth, y = species_richness)) +
  geom_point(alpha = 0.55) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  geom_text(
    data = richness_annotations,
    aes(label = label),
    x = Inf,
    y = Inf,
    hjust = 1.15,
    vjust = 1.2,
    inherit.aes = FALSE
  ) +
  facet_wrap(vars(snow_lag_label), scales = "free_x") + #one panel per snow lag
  labs(
    x = "Mean peak-winter snow depth",
    y = "Species richness",
    title = "Species richness and lagged peak-winter snow depth"
  ) +
  theme_bw()

ggsave(
  filename = file.path(output_folder, "04_species_richness_peak_winter_snow_depth_lags.png"),
  plot = richness_plot,
  width = 11,
  height = 7,
  dpi = 300
)

#for species-level data, adds columns from snow_lag_predictors and left_joins
species_plot_data <- species_analysis_data |>
  pivot_longer(
    cols = all_of(snow_lag_predictors$predictor),
    names_to = "predictor",
    values_to = "snow_depth"
  ) |>
  left_join(snow_lag_predictors, by = "predictor") |>
  mutate(
    snow_lag = factor(snow_lag, levels = lag_levels),
    snow_lag_label = factor(snow_lag_label, levels = snow_lag_predictors$snow_lag_label),
    species_label = paste(USDA_code, USDA_name, sep = " - ")
  )

#creates plot for species cover vs snow depth
species_plot <- ggplot(species_plot_data, aes(x = snow_depth, y = relative_cover)) +
  geom_point(alpha = 0.25, size = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.4) +
  facet_grid(vars(species_label), vars(snow_lag_label), scales = "free_y") + #creates panel per species per lag
  labs(
    x = "Mean peak-winter snow depth",
    y = "Relative cover",
    title = "Relative cover of selected species and lagged peak-winter snow depth"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text.y = element_text(angle = 0, hjust = 0)
  )

ggsave(
  filename = file.path(output_folder, "04_selected_species_relative_cover_peak_winter_snow_depth_lags.png"),
  plot = species_plot,
  width = 14,
  height = 8,
  dpi = 300
)

print(richness_plot)
print(species_plot)
