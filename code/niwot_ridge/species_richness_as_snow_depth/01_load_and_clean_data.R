library(tidyverse)

# Load the Saddle Grid snow-depth and vegetation point-intercept data, clean the
# core fields, and save reusable plot-year vegetation and snow summaries.

snow_path <- "/Users/tobiahwatts/Desktop/knb-lter-nwt.31.22/saddsnow.dw.data.csv"
vegetation_path <- "/Users/tobiahwatts/Desktop/knb-lter-nwt.93.10/saddptqd.hh.data.csv"
output_folder <- "/Users/tobiahwatts/Desktop/Snowpack_depth_vegetation_outputs"

dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)

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

snow_clean <- read_csv(snow_path, na = c("", "NA", "NaN"), show_col_types = FALSE) |>
  mutate(
    date = as.Date(date),
    year = as.integer(format(date, "%Y")),
    plot = as.integer(point_ID),
    mean_depth = parse_number(as.character(mean_depth))
  ) |>
  filter(!is.na(plot), !is.na(year), !is.na(date))

snow_annual <- snow_clean |>
  group_by(plot, year) |>
  summarise(
    mean_annual_snow_depth = mean(mean_depth, na.rm = TRUE),
    snow_measurement_count = sum(!is.na(mean_depth)),
    first_snow_measurement_date = min(date, na.rm = TRUE),
    last_snow_measurement_date = max(date, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(!is.nan(mean_annual_snow_depth))

vegetation_species <- read_csv(vegetation_path, show_col_types = FALSE) |>
  mutate(
    plot = as.integer(plot),
    year = as.integer(year),
    USDA_code = as.character(USDA_code),
    USDA_name = as.character(USDA_name)
  ) |>
  filter(!is.na(plot), !is.na(year), !is.na(USDA_code)) |>
  filter(!USDA_code %in% non_plant_codes) |>
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

write_csv(snow_clean, file.path(output_folder, "01_snow_clean.csv"))
write_csv(snow_annual, file.path(output_folder, "01_snow_annual.csv"))
write_csv(vegetation_species, file.path(output_folder, "01_vegetation_species.csv"))
write_csv(vegetation_plot_year, file.path(output_folder, "01_vegetation_plot_year.csv"))

print(glimpse(snow_annual))
print(glimpse(vegetation_plot_year))
