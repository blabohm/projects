#' Download OSM Data for a Specified City
#'
#' This function downloads OpenStreetMap (OSM) network data for a given city,
#' based on its city code and boundary geometry.
#'
#' @param city_code Character. The code identifying the city (e.g., Eurostat FUA code).
#' @param input_directory Character. Path to the input data directory.
#' @param city_boundaries Character. Path to the geopackage file containing city boundaries.
#'   Default is `paste0(input_directory, "/cities.gpkg")`.
#'
#' @return This function does not return a value but saves OSM data to the specified directory.
#' @export
#'
#' @examples
#' \dontrun{
#' download_OSM(
#'   city_code = "DE001",
#'   input_directory = "data/input"
#' )
#' }

download_OSM <- function(
  city_code,
  input_directory,
  city_boundaries = paste0(input_directory, "/cities.gpkg")
) {
  # Load required packages
  library(dplyr)

  # Dynamically load all helper functions in the functions directory
  function_dir <- file.path(getwd(), "tool", "Module 1 - OSM download", "functions")
  function_files <- list.files(
    path = function_dir,
    pattern = "^1[A-Za-z].*\\.R$",
    full.names = TRUE
  )
  lapply(function_files, source)

  # Load city boundary with a 1000m buffer
  city_boundary <- boundaryLoader1(
    city_code = city_code,
    city_boundaries = city_boundaries,
    buffer_dist = 1000
  )

  # Download OSM network data for the city
  dlOSM(city_boundary, input_directory)
}
