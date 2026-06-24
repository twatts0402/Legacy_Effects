library(tidyverse)

# This script asks whether plant communities respond to snow conditions from the
# previous year, which is a simple one-year ecological memory / legacy effect.
#
# Snow predictor in this script:
#   previous_year_snow_depth = the prior calendar year's mean annual snow depth
#   for the same Saddle Grid plot/stake.

snow_path <- "/Users/tobiahwatts/Desktop/knb-lter-nwt.31.22/saddsnow.dw.data.csv"
vegetation_path <- "/Users/tobiahwatts/Desktop/knb-lter-nwt.93.10/saddptqd.hh.data.csv"
output_folder <- "/Users/tobiahwatts/Desktop/snow_vegetation_outputs"

non_plant_codes <- c(
  "2BARE",
  "2RF",
  "2X",
  "2LTR",
  "2LITT",
  "2LICHN",
  "2MOSS",
  "2HOLE",
  "2SCAT",
  "2SCATE",
  "2UNK",
  "2UNKSC",
  "2WATER",
  "2SNOW",
  "2UNKNOWN"
)

snow_annual <- read_csv(snow_path, na = c("", "NA", "NaN"), show_col_types = FALSE) |>
  mutate(
    date = as.Date(date),
    year = as.integer(format(date, "%Y")),
    plot = as.integer(point_ID),
    # Some legacy snow records are written as values like "210+"; parse_number()
    # keeps the numeric depth while dropping the plus sign.
    mean_depth = parse_number(as.character(mean_depth))
  ) |>
  group_by(plot, year) |>
  summarise(
    mean_annual_snow_depth = mean(mean_depth, na.rm = TRUE),
    snow_measurement_count = n(),
    .groups = "drop"
  ) |>
  filter(!is.nan(mean_annual_snow_depth)) |>
  arrange(plot, year) |>
  group_by(plot) |>
  mutate(previous_year_snow_depth = lag(mean_annual_snow_depth, n = 1)) |>
  ungroup()

vegetation_species <- read_csv(vegetation_path, show_col_types = FALSE) |>
  mutate(
    plot = as.integer(plot),
    year = as.integer(year),
    USDA_code = as.character(USDA_code),
    USDA_name = as.character(USDA_name)
  ) |>
  filter(!is.na(USDA_code), !USDA_code %in% non_plant_codes) |>
  count(plot, year, USDA_code, USDA_name, name = "species_hits") |>
  group_by(plot, year) |>
  mutate(
    total_species_hits = sum(species_hits),
    relative_cover = species_hits / total_species_hits,
    species_richness = n_distinct(USDA_code)
  ) |>
  ungroup()

vegetation_plot_year <- vegetation_species |>
  distinct(plot, year, species_richness, total_species_hits)

richness_snow <- vegetation_plot_year |>
  left_join(
    snow_annual |> select(plot, year, previous_year_snow_depth),
    by = c("plot", "year")
  ) |>
  filter(!is.na(previous_year_snow_depth), !is.na(species_richness))

richness_model <- lm(species_richness ~ previous_year_snow_depth, data = richness_snow)
richness_model_summary <- summary(richness_model)

richness_r_squared <- richness_model_summary$r.squared
richness_p_value <- richness_model_summary$coefficients["previous_year_snow_depth", "Pr(>|t|)"]

richness_plot <- ggplot(
  richness_snow,
  aes(x = previous_year_snow_depth, y = species_richness)
) +
  geom_point(alpha = 0.55) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(
    x = "Previous-year snow depth",
    y = "Species richness",
    title = "Species richness vs. previous-year snow depth"
  ) +
  annotate(
    "text",
    x = Inf,
    y = Inf,
    label = paste(
      "R² =", round(richness_r_squared, 3),
      "\np =", round(richness_p_value, 4)
    ),
    hjust = 1.2,
    vjust = 1.2
  ) +
  theme_bw()

ggsave(
  filename = file.path(output_folder, "02_previous_year_snow_depth_richness.png"),
  plot = richness_plot,
  width = 7,
  height = 5,
  dpi = 300
)

species_snow <- vegetation_species |>
  left_join(
    snow_annual |> select(plot, year, previous_year_snow_depth),
    by = c("plot", "year")
  ) |>
  filter(!is.na(previous_year_snow_depth), !is.na(relative_cover))

species_models <- species_snow |>
  group_by(USDA_code, USDA_name) |>
  nest() |>
  mutate(
    n_observations = map_int(data, nrow),
    cover_values = map_int(data, ~ n_distinct(.x$relative_cover)),
    model = map2(
      data,
      cover_values,
      ~ if (nrow(.x) >= 10 && .y > 1) {
        lm(relative_cover ~ previous_year_snow_depth, data = .x)
      } else {
        NULL
      }
    ),
    model_summary = map(model, ~ if (is.null(.x)) NULL else summary(.x)),
    r_squared = map_dbl(model_summary, ~ if (is.null(.x)) NA_real_ else .x$r.squared),
    p_value = map_dbl(
      model_summary,
      ~ if (is.null(.x)) {
        NA_real_
      } else {
        .x$coefficients["previous_year_snow_depth", "Pr(>|t|)"]
      }
    ),
    slope = map_dbl(
      model,
      ~ if (is.null(.x)) NA_real_ else coef(.x)[["previous_year_snow_depth"]]
    )
  ) |>
  select(USDA_code, USDA_name, n_observations, slope, r_squared, p_value) |>
  arrange(p_value)

write_csv(
  species_models,
  file.path(output_folder, "02_previous_year_snow_depth_species_models.csv")
)

top_species <- species_snow |>
  group_by(USDA_code, USDA_name) |>
  summarise(total_hits = sum(species_hits), .groups = "drop") |>
  slice_max(total_hits, n = 12) |>
  mutate(species_label = paste(USDA_code, USDA_name, sep = " - "))

species_plot_data <- species_snow |>
  inner_join(top_species, by = c("USDA_code", "USDA_name")) |>
  mutate(species_label = fct_reorder(species_label, total_hits))

species_plot <- ggplot(
  species_plot_data,
  aes(x = previous_year_snow_depth, y = relative_cover)
) +
  geom_point(alpha = 0.35) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  facet_wrap(vars(species_label), scales = "free_y") +
  labs(
    x = "Previous-year snow depth",
    y = "Relative cover",
    title = "Relative cover of common species vs. previous-year snow depth"
  ) +
  theme_bw()

ggsave(
  filename = file.path(output_folder, "02_previous_year_snow_depth_species_relative_cover.png"),
  plot = species_plot,
  width = 12,
  height = 9,
  dpi = 300
)

print(richness_model_summary)
print(species_models)
