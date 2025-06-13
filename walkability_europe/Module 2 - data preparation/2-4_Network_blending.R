#' Blend Network with Building and Green Space Entries
#'
#' This function integrates building and park entry points into a cleaned street network,
#' snapping them to the nearest network edges and outputting a blended network.
#'
#' @param city_code Character. The city code (e.g., "DE001") used for filtering the boundary layer.
#' @param input_directory Character. Path to the input directory containing source data files.
#' @param output_directory Character. Path to the directory where outputs will be saved.
#' @param city_boundaries Character. Path to the GPKG file containing city boundaries.
#'   Default: `paste0(input_directory, "cities.gpkg")`.
#' @param network_directory Character. Path to the cleaned network file (GPKG).
#'   Default: `paste0(output_directory, "/network_clean.gpkg")`.
#' @param green_space_directory Character. Path to park/green space entries (GPKG).
#'   Default: `paste0(output_directory, "/green_space_entries.gpkg")`.
#' @param building_directory Character. Path to building entry points with population data (GPKG).
#'   Default: `paste0(output_directory, "/building_entries.gpkg")`.
#' @param blend_out Character. Output path for intermediate blended network data.
#'   Default: `paste0(outputDir, "/net_blend/")`.
#'
#' @return This function writes blended network files (nodes and edges) to disk.
#' @export
#'
#' @examples
#' \dontrun{
#' networkBlend(
#'   city_code = "DE001",
#'   input_directory = "data/input/",
#'   output_directory = "data/output/"
#' )
#' }

networkBlend <- function(
  city_code,
  input_directory,
  output_directory,
  city_boundaries = paste0(input_directory, "cities.gpkg"),
  network_directory = paste0(output_directory, "/network_clean.gpkg"),
  green_space_directory = paste0(output_directory, "/green_space_entries.gpkg"),
  building_directory = paste0(output_directory, "/building_entries.gpkg"),
  blend_out = paste0(output_directory, "/net_blend/")
) {
  # Load required packages
  require(dplyr, quietly = TRUE, warn.conflicts = FALSE)
  require(sf, quietly = TRUE)

  # Source all helper functions for Module 2
  function_dir <- file.path(getwd(), "tool", "Module 2 - data preparation", "functions")
  function_files <- list.files(
    path = function_dir,
    pattern = "2-4[A-Za-z].*\\.R|2_.*\\.R",
    full.names = TRUE
  )
  lapply(function_files, source)

  # Create output directory if it does not exist
  if (!dir.exists(blend_out)) dir.create(blend_out, recursive = TRUE)

  # Snap and blend building and park entries to network
  try({
    snapAndBlend(
      city_code = city_code,
      city_boundaries = city_boundaries,
      build_entries = building_directory,
      gs_entries = green_space_directory,
      network = network_directory,
      output_dir = blend_out
    )
  })

  # Combine output files
  nodes <- list.files(blend_out, pattern = "node", full.names = TRUE)
  edges <- list.files(blend_out, pattern = "edge", full.names = TRUE)

  networkCombinator(file_list = edges, output_dir = blend_out, out = "sf") %>%
    mutate(edge_id = row_number()) %>%
    st_write(file.path(output_directory, "edges.gpkg"), append = FALSE, quiet = TRUE)

  combinator(nodes, output_dir = output_directory)

  # Clean up temporary files
  unlink(nodes)
  unlink(edges)
}
