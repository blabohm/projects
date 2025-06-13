#' Convert sf Network to Undirected sfnetwork with Edge Weights
#'
#' Takes an sf network and converts it into an undirected sfnetwork object
#' with edge weights based on spatial length.
#'
#' @param network `sf` or `sfnetwork` object representing a spatial network.
#'
#' @return An undirected `sfnetwork` object with weighted edges.
#' @export
make_sf_network <- function(network) {
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)
  require(sfnetworks, quietly = TRUE)
  require(tidygraph, quietly = TRUE)

  network %>%
    mutate(weight = st_length(.)) %>%
    as_sfnetwork() %>%
    convert(to_undirected)
}

#' Calculate Origin-Destination Cost Matrix Based on Network Distances
#'
#' Computes the shortest network cost from each building entry to each green space entry.
#'
#' @param build_entries `sf` object. Building entries with geometry column `geom`.
#' @param gs_entries `sf` object. Green space entries with geometry column `geom`.
#' @param sf_network `sfnetwork` object representing the spatial network.
#'
#' @return Integer vector of nearest green space indices for each building.
#' @export
calc_OD_cost <- function(build_entries, gs_entries, sf_network) {
  require(dplyr, quietly = TRUE)
  require(sfnetworks, quietly = TRUE)

  cost_matrix <- st_network_cost(
    x = sf_network,
    from = build_entries$geom,
    to = gs_entries$geom
  )

  apply(cost_matrix, 1, function(x) which.min(x)[1])
}

#' Add Nearest Green Space Entry to Building Entries
#'
#' Adds nearest green space geometry, ID, and area to building entries based on OD cost matrix.
#'
#' @param build_entries `sf` object. Buildings with geometry column `geom`.
#' @param gs_entries `sf` object. Green spaces with geometry and area columns.
#' @param odc_matrix Integer vector. Index of nearest green space for each building.
#'
#' @return `sf` object of building entries with nearest entry info added.
#' @export
add_nearest_entry <- function(build_entries, gs_entries, odc_matrix) {
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)

  build_entries %>%
    mutate(
      nearest_entry = gs_entries$geom[odc_matrix],
      ne_id = odc_matrix,
      area = gs_entries$area[1]
    )
}

#' Calculate Euclidean Distance Between Buildings and Their Nearest Entries
#'
#' Adds Euclidean distance from building geometry to nearest green space entry.
#'
#' @param build_entries `sf` object. Buildings with columns `geom` and `nearest_entry`.
#'
#' @return `sf` object with new `euklid_dis` column and sorted by `ne_id`.
#' @export
add_euklid_dist <- function(build_entries) {
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)

  build_entries %>%
    mutate(euklid_dis = st_distance(nearest_entry, geom, by_element = TRUE)) %>%
    arrange(ne_id)
}

#' Add Shortest Network Path to Building Entries
#'
#' Selects and applies appropriate shortest path function based on nearest entry IDs.
#'
#' @param build_entries `sf` object. Building entries with nearest entry IDs (`ne_id`).
#' @param gs_entries `sf` object. Green space entries.
#' @param sf_network `sfnetwork` object. Spatial network.
#'
#' @return `sf` object with shortest path information added.
#' @export
add_shortest_path <- function(build_entries, gs_entries, sf_network) {
  require(dplyr, quietly = TRUE)

  unique_ne <- unique(build_entries$ne_id)
  counts <- sapply(unique_ne, function(x) sum(build_entries$ne_id == x))

  if (length(unique_ne) == 2 && all(counts == 2)) {
    s_path1(build_entries, gs_entries, sf_network)
  } else if (length(unique_ne) > 1) {
    s_path2(build_entries, gs_entries, sf_network)
  } else {
    s_path3(build_entries, gs_entries, sf_network)
  }
}

#' Filter Building Entries by Network Distance Threshold
#'
#' Adds `net_dist` (network distance) column and filters buildings with distances
#' less than threshold `d` and greater than zero.
#'
#' @param build_entries `sf` object. Building entries with `sPath` list-column.
#' @param sf_network `sfnetwork` object. Spatial network with weighted edges.
#' @param d Numeric. Maximum distance threshold for filtering (default 500).
#'
#' @return Filtered `sf` object with network distances.
#' @export
add_net_dist <- function(build_entries, sf_network, d = 500) {
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)
  require(sfnetworks, quietly = TRUE)

  dist_df <- sf_network %>%
    activate("edges") %>%
    pull(weight)

  build_entries %>%
    st_drop_geometry() %>%
    mutate(net_dist = sapply(sPath, function(x) sum(dist_df[x], na.rm = TRUE))) %>%
    filter(net_dist < d & net_dist > 0)
}

#' Calculate Accessibility Indices for Building Entries
#'
#' Adds location score (`ls`) and distance index (`di`) columns to building entries.
#'
#' @param build_entries `sf` object. Buildings with columns `population`, `area`, `net_dist`, and `euklid_dis`.
#' @param sf_network `sfnetwork` object (not directly used but kept for consistency).
#'
#' @return `sf` object with added indices.
#' @export
add_indices <- function(build_entries, sf_network) {
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)
  require(sfnetworks, quietly = TRUE)

  build_entries %>%
    mutate(
      ls = (population * area) / (net_dist ^ 2),
      di = euklid_dis / net_dist
    )
}
