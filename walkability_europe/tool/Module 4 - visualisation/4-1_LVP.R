################################################################################
# MODULE 4 - LENE-VOIGT-PARK ANALYSIS
# AUTHOR: BENJAMIN LABOHM, BERLIN, 2022
################################################################################

library(sf)
library(sfnetworks)
library(dplyr)
library(tidygraph)

#' Source Module 3 Functions
#'
#' Reuses the function from Module 3 to source function scripts.
#'
#' @param path Character. Directory path for function scripts.
#' @param pattern Character. Regex pattern to match filenames.
source_module_functions <- function(path, pattern = "3.*\\.R") {
  list.files(path, pattern = pattern, full.names = TRUE) %>%
    purrr::walk(source)
}

# Set working directory and identifiers ----
wd <- "C:/Users/labohben/Desktop/DE008/"
ID <- "23473-DE008L2"

# Paths to spatial data files ----
green_space_path <- file.path(wd, "DE008L2_LEIPZIG_UA2018_v012.gpkg")
green_space_entries_path <- file.path(wd, "green_space_entries.gpkg")
buildings_path <- file.path(wd, "buildings.gpkg")
nodes_path <- file.path(wd, "nodes.gpkg")
edges_path <- file.path(wd, "edges.gpkg")

# Source supporting functions from Module 3 ----
functions_path <- file.path(getwd(), "tool", "Module 3 - index building", "functions")
source_module_functions(functions_path)

#' Query Green Space Polygons by Identifier
#'
#' Retrieves green space polygons matching an identifier from a GeoPackage.
#'
#' @param gpkg_path Character. Path to green space GeoPackage.
#' @param identifier Character. Identifier string to filter green spaces.
#' @return sf object of green space polygons.
get_green_spaces <- function(gpkg_path, identifier) {
  layer_name <- st_layers(gpkg_path)$name[1]
  query <- sprintf("SELECT area, geom FROM %s WHERE identifier LIKE '%s'", layer_name, identifier)
  read_sf(gpkg_path, query = query)
}

#' Query Green Space Entries by Identifier
#'
#' Retrieves green space entry geometries matching an identifier.
#'
#' @param gpkg_path Character. Path to green space entries GeoPackage.
#' @param identifier Character. Identifier string.
#' @return sf object of green space entry geometries.
get_green_space_entries <- function(gpkg_path, identifier) {
  query <- sprintf("SELECT geom FROM green_space_entries WHERE identifier LIKE '%s'", identifier)
  read_sf(gpkg_path, query = query)
}

# Retrieve green space polygons and entries ----
green_spaces <- get_green_spaces(green_space_path, ID)
green_space_entries <- get_green_space_entries(green_space_entries_path, ID)

# Create buffer WKT around green space entries for filtering ----
buffer_distance <- 500

buffer_wkt <- green_space_entries %>%
  st_buffer(buffer_distance) %>%
  st_union() %>%
  st_geometry() %>%
  st_as_text()

#' Read Filtered Building Polygons with Non-Null Population
#'
#' Reads building polygons within buffered area where population is not null.
#'
#' @param gpkg_path Character. Path to buildings GeoPackage.
#' @param wkt_filter Character. WKT filter string.
#' @return sf object of building polygons.
get_filtered_buildings <- function(gpkg_path, wkt_filter) {
  query <- "SELECT * FROM 'osm_buildings' WHERE population IS NOT NULL"
  read_sf(gpkg_path, wkt_filter = wkt_filter, query = query)
}

#' Read Filtered Nodes with Population > 0
#'
#' Reads nodes within buffered area with population information.
#'
#' @param gpkg_path Character. Path to nodes GeoPackage.
#' @param wkt_filter Character. WKT filter string.
#' @return sf object of nodes with population > 0.
get_filtered_nodes <- function(gpkg_path, wkt_filter) {
  query <- "SELECT population, geom FROM 'nodes' WHERE population IS NOT NULL"
  read_sf(gpkg_path, wkt_filter = wkt_filter, query = query) %>%
    filter(population > 0)
}

buildings <- get_filtered_buildings(buildings_path, buffer_wkt)
building_entries <- get_filtered_nodes(nodes_path, buffer_wkt)
network_edges <- read_sf(edges_path, wkt_filter = buffer_wkt) %>%
  mutate(weight = st_length(.))

# Convert network to undirected sfnetwork ----
network_sfnet <- as_sfnetwork(network_edges) %>%
  convert(to_undirected)

# Calculate nearest green space ID for each building entry ----
building_entries$ne_id <- calc_OD_cost(
  build_entries = building_entries,
  gs_entries = green_space_entries,
  sf_network = network_sfnet
)

# Extract shortest path edges for a sample building (index 301) ----
shortest_path_edges <- st_network_paths(
  network_sfnet,
  from = building_entries$geom[301],
  to = green_space_entries$geom[building_entries$ne_id[301]]
) %>%
  pull(edge_paths) %>%
  unlist()

# Plotting the spatial data ----
plot(green_spaces$geom, col = "lightgreen", main = "Lene-Voigt-Park Spatial Data")
plot(buildings$geom, col = "darkred", add = TRUE)
plot(network_edges$geom, add = TRUE)
plot(building_entries$geom, col = "blue", add = TRUE, lwd = 3)
plot(green_space_entries$geom, col = "orange", add = TRUE, pch = 3, cex = 2, lwd = 4)
plot(buildings$geom[300], add = TRUE, pch = 5, cex = 3, lwd = 4, col = "yellow")
plot(building_entries$geom[301], add = TRUE, pch = 5, cex = 3, lwd = 4, col = "yellow")
plot(green_space_entries$geom[building_entries$ne_id[301]], add = TRUE, pch = 5, cex = 3, lwd = 4, col = "yellow")
plot(network_edges$geom[shortest_path_edges], add = TRUE, col = "red", lwd = 3)

# Calculate summary statistics for network and spatial distances ----
total_path_weight <- sum(network_edges$weight[shortest_path_edges])
distance_be_to_gse <- st_distance(building_entries$geom[301], green_space_entries$geom[building_entries$ne_id[301]])

# Calculate example DI and LS values ----
di_example <- 118 / 169
ls_example <- buildings$population[300] * green_spaces$area / 169

# Gather Distance Index (DI) and Link Score (LS) outputs ----
gatherDI(
  building_polygons = buildings_path,
  index_dir = file.path(wd, "lvp/"),
  output_dir = file.path(wd, "lvp/di.gpkg")
)

gatherLS(
  edges = edges_path,
  index_dir = file.path(wd, "lvp/"),
  output_dir = file.path(wd, "lvp/ls.gpkg")
)
