# Load and clean vegetation and snow-depth data for legacy snowmelt analyses.

library(tidyverse)

snow_file <- "/Users/tobiahwatts/Desktop/SMART DATA/knb-lter-nwt.31.22/saddsnow.dw.data.csv"
vegetation_file <- "/Users/tobiahwatts/Desktop/SMART DATA/knb-lter-nwt.93.10/saddptqd.hh.data.csv"
output_dir <- '/Users/tobiahwatts/Desktop/SMART OUTPUTS/seperate_year_snowmelt_species-richness'

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

snow_raw <- read_csv(
  snow_file,
  na = c("NA", "", "NaN"),
  show_col_types = FALSE)

vegetation_raw <- read_csv(
  vegetation_file,
  na = c("NA", ""),
  show_col_types = FALSE)

# Codes beginning with "2" are non-species surface/marker categories, it filters thme out
species_richness <- vegetation_raw |>
  filter(
    !is.na(USDA_code),
    USDA_code != "",
    !str_starts(USDA_code, "2")
  ) |>
  group_by(plot, vegetation_year = year) |>
  summarize(
    species_richness = n_distinct(USDA_code),
    .groups = "drop"
  ) |>
  arrange(vegetation_year, plot)

#makes new snow_depth table
#defines a snow year as september onward. So if it snows sept 2024, thats part of 2025 snow year.
snow_depth <- snow_raw |>
  mutate(
    date = ymd(date),
    snow_depth_cm = parse_number(as.character(mean_depth)),
    snow_year = if_else(month(date) >= 9, year(date) + 1, year(date)),
    day_of_year = yday(date)
  ) |>
  filter(
    !is.na(date),
    !is.na(snow_depth_cm)
  ) |>
  transmute(
    plot = point_ID,
    snow_year,
    date,
    day_of_year,
    snow_depth_cm
  ) |>
  arrange(snow_year, plot, date)

write_csv(
  species_richness,
  file.path(output_dir, "01_species_richness_by_plot_year.csv")
)

write_csv(
  snow_depth,
  file.path(output_dir, "01_clean_snow_depth_by_plot_year.csv")
)

cat("Step 01 complete.\n")
cat("Species richness rows:", nrow(species_richness), "\n")
cat("Clean snow-depth rows:", nrow(snow_depth), "\n")
cat("Outputs written to:", output_dir, "\n")
