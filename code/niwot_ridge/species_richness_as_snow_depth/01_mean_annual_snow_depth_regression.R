library(tidyverse)

# This script asks whether plot-year plant community structure is related to
# contemporary snow conditions in the same year.
#
# Snow predictor in this script:
#   mean_annual_snow_depth = average of all measured snow depths for a plot/stake
#   within a calendar year.
#
# Vegetation responses created here:
#   1. species_richness = number of unique taxa observed in each plot-year
#   2. relative_cover = each species' share of all biological hits in a plot-year

snow_path <- "/Users/tobiahwatts/Desktop/knb-lter-nwt.31.22/saddsnow.dw.data.csv"
vegetation_path <- "/Users/tobiahwatts/Desktop/knb-lter-nwt.93.10/saddptqd.hh.data.csv"

# Folder where plots and model summaries will be written.
output_folder <- "/Users/tobiahwatts/Desktop/snow_vegetation_outputs"

# These records are not living plant taxa, so they are removed before calculating
non_plant_codes <- c(
  "2BARE",    # bare ground
  "2RF",      # rock fragments
  "2X",       # registration marker
  "2LTR",     # litter
  "2LITT",    # litter, alternate spelling if present
  "2LICHN",   # lichen
  "2MOSS",    # moss; remove from vascular plant community response
  "2HOLE",    # hole
  "2SCAT",    # scat
  "2SCATE",   # elk scat
  "2UNK",     # unknown
  "2UNKSC",   # unknown soil crust
  "2WATER",   # water
  "2SNOW",    # snow
  "2UNKNOWN"  # unknown/non-identifiable records, if present
  )

# Average repeated snow measurements within each Saddle Grid stake and year.
# point_ID is the snow stake identifier and corresponds to the vegetation plot
# number in the plant point-intercept dataset.
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
  filter(!is.nan(mean_annual_snow_depth))

# Convert the vegetation hit data into one row per plot-year-species.
# Relative cover is calculated as a species' hits divided by all biological
# species hits in that plot-year, so the species proportions sum to 1 within
# each plot-year.
vegetation_species <- read_csv(vegetation_path, show_col_types = FALSE) |>
  mutate(
    plot = as.integer(plot),
    year = as.integer(year),
    USDA_code = as.character(USDA_code),
    USDA_name = as.character(USDA_name)
  ) |>
  filter(!is.na(USDA_code), !USDA_code  %in% non_plant_codes) |>
  count(plot, year, USDA_code, USDA_name, name = "species_hits") |>
  group_by(plot, year) |>
  mutate(
    total_species_hits = sum(species_hits),
    relative_cover = species_hits / total_species_hits,
    species_richness = n_distinct(USDA_code)
  ) |>
  ungroup()

# Collapse the long species table to one row per plot-year for the richness
# regression.
vegetation_plot_year <- vegetation_species |>
  distinct(plot, year, species_richness, total_species_hits)

# Join contemporary snow depth to plot-year vegetation.
richness_snow <- vegetation_plot_year |>
  left_join(snow_annual, by = c("plot", "year")) |>
  filter(!is.na(mean_annual_snow_depth), !is.na(species_richness))

# Linear regression for the plot-year richness response.
richness_model <- lm(species_richness ~ mean_annual_snow_depth, data = richness_snow)
richness_model_summary <- summary(richness_model)

richness_r_squared <- richness_model_summary$r.squared
richness_p_value <- richness_model_summary$coefficients["mean_annual_snow_depth", "Pr(>|t|)"]

richness_plot <- ggplot(
  richness_snow,
  aes(x = mean_annual_snow_depth, y = species_richness)
) +
  geom_point(alpha = 0.55) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(
    x = "Mean annual snow depth",
    y = "Species richness",
    title = "Species richness vs. mean annual snow depth") +
  annotate("text", x = Inf, y = Inf,
    label = paste(
      "R² =", round(richness_r_squared, 3),
      "\np =", round(richness_p_value, 4)),
    hjust = 1.2,
    vjust = 1.2)

# Join contemporary snow depth to each species' relative cover.
species_snow <- vegetation_species |>
  left_join(snow_annual, by = c("plot", "year")) |>
  filter(!is.na(mean_annual_snow_depth), !is.na(relative_cover))

# Fit the same simple linear model separately for each species. Species with too
# few non-missing observations or no variation in relative cover are skipped,
# because lm() cannot estimate a meaningful slope for those cases.
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
        lm(relative_cover ~ mean_annual_snow_depth, data = .x)
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
        .x$coefficients["mean_annual_snow_depth", "Pr(>|t|)"]
      }
    ),
    slope = map_dbl(
      model,
      ~ if (is.null(.x)) NA_real_ else coef(.x)[["mean_annual_snow_depth"]]
    )
  ) |>
  select(USDA_code, USDA_name, n_observations, slope, r_squared, p_value) |>
  arrange(p_value)

species_models_to_save <- species_models |>
  mutate(
    slope = round(slope, 15),
    r_squared = round(r_squared, 8),
    p_value = round(p_value, 5)
  )

write_csv(
  species_models_to_save,
  file.path(output_folder, "01_mean_annual_snow_depth_species_models.csv")
)

# Plot the most frequently observed species so the figure remains readable.
top_species <- species_snow |>
  group_by(USDA_code, USDA_name) |>
  summarise(total_hits = sum(species_hits), .groups = "drop") |>
  slice_max(total_hits, n = 12) |>
  mutate(species_label = paste(USDA_code, USDA_name, sep = " - "))

species_plot_data <- species_snow |>
  inner_join(top_species, by = c("USDA_code", "USDA_name")) |>
  mutate(species_label = fct_reorder(species_label, total_hits))

top_species_plot <- ggplot(
  species_plot_data,
  aes(x = mean_annual_snow_depth, y = relative_cover)
) +
  geom_point(alpha = 0.35) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  facet_wrap(vars(species_label), scales = "free_y") +
  labs(
    x = "Mean annual snow depth",
    y = "Relative cover",
    title = "Relative cover of common species vs. mean annual snow depth"
  ) +
  theme_bw()

ggsave(
  filename = file.path(output_folder, "01_top_species_plot.png"),
  plot = top_species_plot,
  width = 11,
  height = 8,
  dpi = 300
)

print(richness_model_summary)
print(species_models)
