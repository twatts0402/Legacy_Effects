library(tidyverse)

# Create summary figures for the rolling snowpack-depth analysis.

output_folder <- "/Users/tobiahwatts/Desktop/Snowpack_depth_vegetation_outputs"
richness_analysis_data_path <- file.path(output_folder, "04_richness_analysis_data.csv")
species_analysis_data_path <- file.path(output_folder, "04_species_analysis_data.csv")
richness_models_path <- file.path(output_folder, "04_richness_models.csv")

dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)

richness_analysis_data <- read_csv(richness_analysis_data_path, show_col_types = FALSE)
species_analysis_data <- read_csv(species_analysis_data_path, show_col_types = FALSE)
richness_models <- read_csv(richness_models_path, show_col_types = FALSE)

window_levels <- c("0yr", "1yr", "3yr", "5yr")

richness_plot_data <- richness_analysis_data |>
  mutate(
    snow_window = factor(snow_window, levels = window_levels),
    snow_window_label = factor(snow_window_label, levels = unique(snow_window_label[order(snow_window)]))
  )

richness_annotations <- richness_models |>
  mutate(
    snow_window = factor(snow_window, levels = window_levels),
    label = paste0(
      "R² = ", round(r_squared, 3),
      "\np = ", signif(p_value, 3)
    )
  )

richness_plot <- ggplot(richness_plot_data, aes(x = snow_depth_mean, y = species_richness)) +
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
  facet_wrap(vars(snow_window_label), scales = "free_x") +
  labs(
    x = "Rolling mean snow depth",
    y = "Species richness",
    title = "Species richness and rolling snowpack depth"
  ) +
  theme_bw()

ggsave(
  filename = file.path(output_folder, "05_species_richness_rolling_snow_depth.png"),
  plot = richness_plot,
  width = 11,
  height = 7,
  dpi = 300
)

top_species <- species_analysis_data |>
  group_by(USDA_code, USDA_name) |>
  summarise(total_hits = sum(species_hits), .groups = "drop") |>
  slice_max(total_hits, n = 12) |>
  mutate(species_label = paste(USDA_code, USDA_name, sep = " - "))

species_plot_data <- species_analysis_data |>
  filter(snow_window %in% window_levels) |>
  inner_join(top_species, by = c("USDA_code", "USDA_name")) |>
  mutate(
    snow_window = factor(snow_window, levels = window_levels),
    snow_window_label = factor(snow_window_label, levels = unique(snow_window_label[order(snow_window)])),
    species_label = fct_reorder(species_label, total_hits)
  )

species_plot <- ggplot(species_plot_data, aes(x = snow_depth_mean, y = relative_cover)) +
  geom_point(alpha = 0.25, size = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.4) +
  facet_grid(vars(species_label), vars(snow_window_label), scales = "free_y") +
  labs(
    x = "Rolling mean snow depth",
    y = "Relative cover",
    title = "Relative cover of common species and rolling snowpack depth"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text.y = element_text(angle = 0, hjust = 0)
  )

ggsave(
  filename = file.path(output_folder, "05_top_species_relative_cover_rolling_snow_depth.png"),
  plot = species_plot,
  width = 14,
  height = 16,
  dpi = 300
)

print(richness_plot)
print(species_plot)
