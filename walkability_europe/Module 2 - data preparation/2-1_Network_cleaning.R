#' Prepare and Clean OSM Network Data
#'
#' This function prepares and cleans OSM-derived network tiles for a given city,
#' including loading boundaries, merging relevant network tiles, and cleaning the result.
#'
#' @param city_code Character. The first 5 digits of the URAU code identifying the city.
#' @param input_directory Character. Path to the input directory containing city boundaries and OSM tiles.
#' @param output_directory Character. Path to the output directory for the cleaned network.
#' @param city_boundaries Character. Path to the city boundaries geopackage file.
#'   Default is `file.path(input_directory, "cities.gpkg")`.
#' @param network_tile_dir Character. Directory containing OSM network tiles.
#'   Default is `file.path(input_directory, "osm_network")`.
#' @param network_out Character. Path for writing the cleaned network output file.
#'   Default is `file.path(output_directory, "network_clean.gpkg")`.
#'
#' @return This function saves the cleaned network as a `.gpkg` file. It does not return a value.
#' @export
#'
#' @examples
#' \dontrun{
#' networkPrep(
#'   city_code = "DE001",
#'   input_directory = "data/input",
#'   output_directory = "data/output"
#' )
#' }

networkPrep <- function(
  city_code,
  input_directory,
  output_directory,
  city_boundaries = file.path(input_directory, "cities.gpkg"),
  network_tile_dir = file.path(input_directory, "osm_network"),
  network_out = file.path(output_directory, "network_clean.gpkg")
) {
  # Load required package
  require(dplyr, quietly = TRUE)

  # Load helper functions from module folder
  function_dir <- file.path(getwd(), "tool", "Module 2 - data preparation", "functions")
  function_files <- list.files(
    path = function_dir,
    pattern = "2-1[A-Za-z].*\\.R|2_.*\\.R",
    full.names = TRUE
  )
  lapply(function_files, source)

  # Load city core boundary with buffer
  city_boundary <- boundaryLoader(
    city_boundaries = city_boundaries,
    city_code = city_code,
    buffer_dist = 1000
  )

  # List tiles for city, load and combine network tiles, clean result
  listTiles(network_tile_directory = network_tile_dir, city_code = city_code) %>%
    net_combinator(file_list = ., boundary = city_boundary, tmp_dir = output_directory) %>%
    networkCleaner() %>%
    st_write(network_out, quiet = TRUE, append = FALSE)
}
