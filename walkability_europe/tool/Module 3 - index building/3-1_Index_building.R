#' Generate Spatial Accessibility Indices
#'
#' This function calculates various spatial indices (e.g., detour index and local significance)
#' using input geospatial datasets (nodes, edges, buildings). Intermediate calculations
#' are performed for features associated with green space nodes.
#'
#' @param working_directory Character. Root directory containing input and output subdirectories.
#' @param output Character. Directory path to store calculated index layers.
#'   Default is `paste0(working_directory, "indices/")`.
#' @param nodes Character. Path to the nodes GeoPackage file. 
#'   Default is `paste0(working_directory, "nodes.gpkg")`.
#' @param edges Character. Path to the edges GeoPackage file. 
#'   Default is `paste0(working_directory, "edges.gpkg")`.
#' @param buildings Character. Path to the buildings GeoPackage file. 
#'   Default is `paste0(working_directory, "buildings.gpkg")`.
#'
#' @return No return value. Writes output GeoPackages containing index results.
#' @export
#'
#' @details
#' Steps performed:
#' 1. Loads helper functions from module directory.
#' 2. Loads node data and extracts unique green space identifiers.
#' 3. Computes indices per green space unit using `calcIndices()`.
#' 4. Aggregates index outputs using `gatherDI()` and `gatherLS()`.

getIndices <- function(
  working_directory,
  output = paste0(working_directory, "indices/"),
  nodes = paste0(working_directory, "nodes.gpkg"),
  edges = paste0(working_directory, "edges.gpkg"),
  buildings = paste0(working_directory, "buildings.gpkg")
) {
  # Load required packages
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)

  # Load supporting functions from the module's function folder
  function_dir <- file.path(getwd(), "tool", "Module 3 - index building", "functions")
  function_files <- list.files(
    path = function_dir,
    pattern = "^3-1[A-Za-z].*\\.R$",
    full.names = TRUE
  )
  lapply(function_files, source)

  # Load green space node identifiers
  green_space_IDs <- st_read(
    dsn = nodes,
    query = "SELECT identifier FROM nodes WHERE identifier IS NOT NULL",
    quiet = TRUE
  ) %>%
    pull(identifier) %>%
    unique()

  # Run index calculations, handling errors gracefully
  tryCatch({
    calcIndices(
      green_space_IDs = green_space_IDs,
      in_directory = working_directory,
      out_directory = output
    )
  }, error = function(e) {
    message("Error during index calculation: ", e$message)
  })

  # Aggregate Detour Index into single output file
  gatherDI(
    building_polygons = buildings,
    index_dir = output,
    output_dir = file.path(working_directory, "detour_index.gpkg")
  )

  # Aggregate Local Significance Index into single output file
  gatherLS(
    edges = edges,
    index_dir = output,
    output_dir = file.path(working_directory, "local_significance.gpkg")
  )
}
