#' Prepare OSM Building Data Using Urban Atlas Residential Areas
#'
#' This function processes OSM building tiles by filtering them using Urban Atlas 2018 residential land-use data,
#' adds estimated population, assigns building IDs, and exports the resulting dataset.
#'
#' @param city_code Character. Code identifying the city (e.g., Eurostat FUA code).
#' @param input_directory Character. Path to the input data directory.
#' @param output_directory Character. Path to the output data directory.
#' @param city_boundaries Character. Path to the geopackage with city boundaries.
#'   Default is `paste0(input_directory, "/cities.gpkg")`.
#' @param osm_directory Character. Path to the directory containing OSM building tiles.
#'   Default is `paste0(input_directory, "/osm_buildings/")`.
#' @param ua_directory Character. Path to the directory with Urban Atlas 2018 land-use data.
#'   Default is `paste0(input_directory, "/UA2018/")`.
#' @param building_out Character. Path to the output GPKG file for building polygons.
#'   Default is `paste0(output_directory, "/buildings.gpkg")`.
#'
#' @return Writes building polygons and building entry points to specified output files.
#' @export
#'
#' @examples
#' \dontrun{
#' buildingPrep(
#'   city_code = "DE001",
#'   input_directory = "data/input",
#'   output_directory = "data/output"
#' )
#' }

buildingPrep <- function(
  city_code,
  input_directory,
  output_directory,
  city_boundaries = paste0(input_directory, "/cities.gpkg"),
  osm_directory = paste0(input_directory, "/osm_buildings/"),
  ua_directory = paste0(input_directory, "/UA2018/"),
  building_out = paste0(output_directory, "/buildings.gpkg")
) {
  # Load required package
  require(dplyr, quietly = TRUE)

  # Source custom functions for this module
  function_dir <- file.path(getwd(), "tool", "Module 2 - data preparation", "functions")
  function_files <- list.files(
    path = function_dir,
    pattern = "^2-2[A-Za-z].*\\.R$|^2_.*\\.R$",
    full.names = TRUE
  )
  lapply(function_files, source)

  # Load city boundary
  city_boundary <- boundaryLoader(
    city_boundaries = city_boundaries,
    city_code = city_code
  )

  # List OSM building files for the city
  osm_file_list <- list.files(
    path = osm_directory,
    pattern = city_code,
    full.names = TRUE
  )

  # Load Urban Atlas residential polygons
  UAresidential <- UAresLoader(
    ua_dir = ua_directory,
    fua_code = city_boundary$FUA_CODE,
    boundary = city_boundary
  )

  # Process each OSM tile
  for (osm_file in osm_file_list) {
    osmTmp <- OSMloader(osm_file, boundary = city_boundary)

    if (!is.null(osmTmp) && nrow(osmTmp) > 0) {
      osmTmp %>%
        OSMfilter(ua_residential = UAresidential) %>%
        OSMpop() %>%
        OSMbuildID() %>%
        st_write(
          dsn = building_out,
          layer = "osm_buildings",
          quiet = TRUE,
          append = TRUE
        )
    }
  }

  # Extract building entry points
  st_read(building_out, quiet = TRUE) %>%
    st_point_on_surface() %>%
    roundGeometry() %>%
    st_write(
      dsn = gsub("buildings", "building_entries", building_out),
      quiet = TRUE,
      append = FALSE
    )
}
