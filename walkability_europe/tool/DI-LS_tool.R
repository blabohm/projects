################################################################################
# DETOUR INDEX & LOCAL SIGNIFICANCE TOOL
# AUTHOR: Benjamin Labohm, Berlin, 2022
################################################################################
# DESCRIPTION:
# This tool calculates two indices to assess accessibility of public green spaces:
# 
# 1. Detour Index (DI): 
#    Ratio of Euclidean vs. network distance from building entrances to nearest 
#    green space entrances. Ranges from 0 (large detours) to 1 (direct access).
#
# 2. Local Significance Index (LS): 
#    Considers green space area, network distance, and number of inhabitants 
#    in each building. Higher LS indicates larger and more accessible GS.
#
# These indices help identify areas with limited access or potential overuse.
################################################################################

# Load required packages
suppressPackageStartupMessages({
  library(dplyr)
  library(sf)
})

# Define input/output directories
input_dir <- "Z:/MA_data/input/"
output_dir <- gsub("input/", "output/", input_dir)

if (!dir.exists(output_dir)) dir.create(output_dir)

# Path to city boundaries file
city_boundaries_path <- file.path(input_dir, "cities.gpkg")

# Optional preprocessing step (already saved as GPKG)
# Uncomment and edit if regeneration is needed
# read_sf("Z:/cities_europe_kernel/cities_boundary.shp") %>%
#   select(URAU_CODE = URAU_COD_1, FUA_CODE, SelectBenn) %>%
#   mutate(
#     URAU_CODE = substr(URAU_CODE, 1, 5),
#     FUA_CODE = substr(FUA_CODE, 1, 5)
#   ) %>%
#   write_sf(city_boundaries_path, layer = "cities")

# Filter cities to process
done <- list.files(output_dir)
city_list <- read_sf(city_boundaries_path, query = "SELECT URAU_CODE, SelectBenn FROM cities") %>%
  arrange(desc(SelectBenn)) %>%
  filter(!(URAU_CODE %in% done))

# Loop through each city and run the full processing pipeline
for (city_code in city_list$URAU_CODE) {

  city_output_dir <- file.path(output_dir, city_code)
  if (!dir.exists(city_output_dir)) dir.create(city_output_dir)

  ################################################################################
  # MODULE 1 - OSM DOWNLOAD
  # Download OpenStreetMap (OSM) data covering the city polygon
  ################################################################################

  module1_path <- file.path(getwd(), "tool", "Module 1 - OSM download")
  module1_files <- list.files(
    path = module1_path,
    pattern = "^1-[1-9].*\\.R$",
    full.names = TRUE
  )
  purrr::walk(module1_files, source)

  download_OSM(
    city_code = city_code,
    input_directory = input_dir
  )

  ################################################################################
  # MODULE 2 - DATA PREPARATION
  ################################################################################

  module2_path <- file.path(getwd(), "tool", "Module 2 - data preparation")
  module2_files <- list.files(
    path = module2_path,
    pattern = "^2-[1-9].*\\.R$",
    full.names = TRUE
  )
  purrr::walk(module2_files, source)

  # 2.1 - NETWORK CLEANING
  networkPrep(
    city_code = city_code,
    input_directory = input_dir,
    output_directory = city_output_dir
  )

  # 2.2 - BUILDING PREPARATION
  buildingPrep(
    city_code = city_code,
    input_directory = input_dir,
    output_directory = city_output_dir
  )

  # 2.3 - GREEN SPACE ENTRY DETECTION
  greenSpacePrep(
    city_code = city_code,
    input_directory = input_dir,
    output_directory = city_output_dir
  )

  # 2.4 - NETWORK BLENDING
  try({
    networkBlend(
      city_code = city_code,
      input_directory = input_dir,
      output_directory = city_output_dir
    )
  })

  ################################################################################
  # MODULE 3 - INDEX BUILDING
  ################################################################################

  module3_path <- file.path(getwd(), "tool", "Module 3 - index building")
  module3_files <- list.files(
    path = module3_path,
    pattern = "^3-[1-9].*\\.R$",
    full.names = TRUE
  )
  purrr::walk(module3_files, source)

  # Compute Detour Index (DI) and Local Significance (LS)
  getIndices(working_directory = city_output_dir)
}

################################################################################
# END OF SCRIPT
################################################################################
