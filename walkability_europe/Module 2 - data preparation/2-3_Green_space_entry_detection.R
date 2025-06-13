#' Detect Park Entry Points for a Given City
#'
#' This function prepares data for identifying park entry points using Urban Atlas (UA) green space layers,
#' city boundaries, and a street network. It handles proximity filtering to avoid overlapping cities, 
#' loads relevant spatial data, and saves the detected entry points as an output file.
#'
#' @param city_code Character. The code identifying the city (e.g., Eurostat FUA code).
#' @param input_directory Character. Path to the input data directory (contains city boundaries and UA data).
#' @param output_directory Character. Path to the output directory.
#' @param city_boundaries Character. Path to the city boundaries geopackage.
#'   Default is `paste0(input_directory, "/cities.gpkg")`.
#' @param ua_directory Character. Path to the Urban Atlas data directory.
#'   Default is `paste0(input_directory, "/UA2018/")`.
#' @param network_directory Character. Path to the cleaned network file.
#'   Default is `paste0(output_directory, "/network_clean.gpkg")`.
#' @param green_space_out Character. Path to output file for green space entry points.
#'   Default is `paste0(output_directory, "/green_space_entries.gpkg")`.
#'
#' @return This function writes a spatial file of park entry points. No object is returned.
#' @export
#'
#' @examples
#' \dontrun{
#' greenSpacePrep(
#'   city_code = "DE001",
#'   input_directory = "data/input",
#'   output_directory = "data/output"
#' )
#' }

greenSpacePrep <- function(
  city_code,
  input_directory,
  output_directory,
  city_boundaries = paste0(input_directory, "/cities.gpkg"),
  ua_directory = paste0(input_directory, "/UA2018/"),
  network_directory = paste0(output_directory, "/network_clean.gpkg"),
  green_space_out = paste0(output_directory, "/green_space_entries.gpkg")
) {
  # Load required packages
  require(dplyr, quietly = TRUE)

  # Source all relevant helper functions in module directory
  function_dir <- file.path(getwd(), "tool", "Module 2 - data preparation", "functions")
  function_files <- list.files(
    path = function_dir,
    pattern = "^2-3_[A-Za-z].*\\.R$|^2_.*\\.R$",
    full.names = TRUE
  )
  lapply(function_files, source)

  # Step 1: Load city boundaries and filter nearby cities
  code_list <- proximity_checker1(
    city_boundaries = city_boundaries,
    city_code = city_code
  )

  # Step 2: Load green spaces for selected cities and detect entry points
  UAgreen_space(
    code_list = code_list,
    ua_directory = ua_directory,
    city_boundaries = city_boundaries,
    city_code = city_code,
    output = "sf"
  ) %>%
    findGSentries(
      green_spaces = .,
      network = network_directory
    ) %>%
    roundGeometry() %>%
    st_write(green_space_out, quiet = TRUE, append = FALSE)
}
