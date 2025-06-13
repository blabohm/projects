#' Load Grid-Serving Entries from `nodes.gpkg`
#'
#' Extracts entries from a nodes file based on a specified identifier.
#'
#' @param ID Character. Identifier for the grid-serving unit (e.g., service area).
#' @param folder Character. Path to the folder containing the `nodes.gpkg` file.
#'
#' @return An `sf` object with columns: `identifier`, `area`, and `geom`.
#' @export
load_gs_entries <- function(ID, folder) {
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)

  nodes <- list.files(folder, pattern = "nodes.gpkg", full.names = TRUE)[1]

  query <- paste0(
    "SELECT identifier, area, geom FROM nodes WHERE identifier = '", ID, "'"
  )

  read_sf(nodes, query = query) %>%
    distinct()
}


#' Load Building Entries Within a Buffer Around GS Entries
#'
#' Loads buildings with non-null population located within a specified buffer around grid-serving units.
#'
#' @param folder Character. Path to folder containing the `nodes.gpkg` file.
#' @param gs_entries `sf` object. Grid-serving entries from `load_gs_entries()`.
#' @param d Numeric. Buffer distance in meters around grid-serving geometries. Default is 500.
#'
#' @return An `sf` object of filtered building entries.
#' @export
load_build_entries <- function(folder, gs_entries, d = 500) {
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)

  nodes <- list.files(folder, pattern = "nodes.gpkg$", full.names = TRUE)[1]

  gs_filter <- gs_entries$geom %>%
    st_buffer(d) %>%
    st_union() %>%
    st_as_text()

  read_sf(nodes,
          query = "SELECT * FROM nodes WHERE population IS NOT NULL",
          wkt_filter = gs_filter) %>%
    filter(population > 0) %>%
    distinct()
}


#' Load Network Edges Within Buffer Around GS Entries
#'
#' Reads the street network edges that fall within a buffered geometry around GS entries.
#'
#' @param folder Character. Path to the folder containing the `edges.gpkg` file.
#' @param gs_entries `sf` object. Grid-serving entries.
#' @param d Numeric. Buffer distance in meters. Default is 500.
#'
#' @return An `sf` object representing the street network edges.
#' @export
load_network <- function(folder, gs_entries, d = 500) {
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)

  edges <- list.files(folder, pattern = "edges.gpkg$", full.names = TRUE)[1]

  gs_filter <- gs_entries$geom %>%
    st_buffer(d) %>%
    st_union() %>%
    st_as_text()

  st_read(edges, wkt_filter = gs_filter, quiet = TRUE)
}


#' Add Accessibility and Distance Parameters to Building Entries
#'
#' Enhances the building dataset with accessibility and spatial indicators.
#'
#' @param build_entries `sf` object. Building entries from `load_build_entries()`.
#' @param gs_entries `sf` object. Grid-serving entries from `load_gs_entries()`.
#' @param network `sf` object. Street network from `load_network()`.
#'
#' @return An enriched `sf` object of building entries with computed indicators.
#' @export
add_params <- function(build_entries, gs_entries, network) {
  require(dplyr, quietly = TRUE)

  # 1. Convert network to sf_network
  sf_network <- make_sf_network(network)

  # 2. Calculate origin-destination cost matrix
  odc_matrix <- calc_OD_cost(build_entries, gs_entries, sf_network)

  # 3–7. Add various metrics
  build_entries %>%
    add_nearest_entry(gs_entries, odc_matrix) %>%
    add_euklid_dist() %>%
    add_shortest_path(gs_entries, sf_network) %>%
    add_net_dist(sf_network) %>%
    add_indices(sf_network)
}


#' Write Output to Disk
#'
#' Saves final building and network outputs to disk if entries are available.
#'
#' @param out `sf` object. Final output of buildings with indices.
#' @param network `sf` object. Network data associated with the output.
#' @param out_dir Character. Output directory to save results.
#' @param ID Character. City or area identifier for filename generation.
#'
#' @return Writes files to disk; no return value.
#' @export
write_output <- function(out, network, out_dir, ID) {
  if (nrow(out) < 1) {
    message("No buildings in service area.")
  } else {
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    write_di(out, out_dir, ID)
    write_ls(out, network, out_dir, ID)
  }
}
