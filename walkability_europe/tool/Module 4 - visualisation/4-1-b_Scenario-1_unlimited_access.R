################################################################################
# MODULE 4 - SCENARIO 1: UNLIMITED ACCESS
# AUTHOR: BENJAMIN LABOHM, BERLIN, 2022
################################################################################

library(dplyr)
library(sf)
library(sfnetworks)
library(ggplot2)
library(tidygraph)

#' Source Module Functions
#'
#' Loads all R scripts from a given directory matching a pattern.
#'
#' @param path Character. Directory path where function scripts are located.
#' @param pattern Character. Regex pattern to filter R scripts.
#' @return Invisibly returns NULL; side effect: sources R scripts.
source_module_functions <- function(path, pattern = "3.*\\.R") {
  list.files(path, pattern = pattern, full.names = TRUE) %>%
    purrr::walk(source)
}

# Set working directory and scenario ID ----
wd <- "C:/Users/labohben/Desktop/DE008/"
scenario_id <- "23473-DE008L2"
buffer_distance <- 1000

# Define paths to spatial data ----
nodes_path <- file.path(wd, "nodes.gpkg")
edges_path <- file.path(wd, "edges.gpkg")
green_space_path <- file.path(wd, "DE008L2_LEIPZIG_UA2018_v012.gpkg")
new_gse_path <- file.path(wd, "new_gse.gpkg")
scenario_out_dir <- file.path(wd, "scenario1")
index_dir <- file.path(scenario_out_dir, "indices")

# Source helper functions ----
functions_path <- file.path(getwd(), "tool", "Module 3 - index building", "functions")
source_module_functions(functions_path)

#' Get Buffered Geometry WKT Filter for Nodes Based on Identifier
#'
#' @param nodes_path Character. Path to nodes GeoPackage.
#' @param identifier Character. Filter identifier.
#' @param buffer_dist Numeric. Buffer distance.
#' @return Character. WKT geometry string for spatial filtering.
get_buffer_wkt <- function(nodes_path, identifier, buffer_dist) {
  query <- sprintf("SELECT * FROM nodes WHERE identifier IS '%s'", identifier)
  entries <- read_sf(nodes_path, query = query)
  st_buffer(entries, buffer_dist) %>%
    st_geometry() %>%
    st_as_text()
}

# Obtain spatial filter WKT for the target identifier ----
wkt_filter <- get_buffer_wkt(nodes_path, scenario_id, buffer_distance)

#' Read Green Space Polygons for Given Identifier
#'
#' @param gs_path Character. Path to green space GeoPackage.
#' @param identifier Character. Identifier to filter.
#' @return sf object. Green space polygons for the identifier.
read_green_spaces <- function(gs_path, identifier) {
  layer_name <- st_layers(gs_path)$name[1]
  query <- sprintf("SELECT area, geom FROM %s WHERE identifier LIKE '%s'", layer_name, identifier)
  read_sf(gs_path, query = query)
}

# Read green space polygons ----
green_spaces <- read_green_spaces(green_space_path, scenario_id)

#' Read and Prepare LVP Outline as MULTILINESTRING
#'
#' @param wd Character. Working directory path.
#' @return sf object. MULTILINESTRING geometry of LVP outline.
read_lvp_outline <- function(wd) {
  lvp_outline_path <- file.path(wd, "lvp_outline.gpkg")
  read_sf(lvp_outline_path) %>%
    st_union() %>%
    st_cast("MULTILINESTRING", warn = FALSE)
}

lvp_outline <- read_lvp_outline(wd)

# Calculate number of regularly spaced points every 10m along outline ----
num_points <- round(as.numeric(st_length(lvp_outline) / 10))

#' Prepare New Green Space Entries
#'
#' Reads new GSE points if available or constructs them (commented out).
#'
#' @param gse_path Character. Path to new green space entries GeoPackage.
#' @return sf object. New green space entries.
prepare_new_gse <- function(gse_path) {
  read_sf(gse_path)
  # If reconstruction needed:
  # points <- st_sample(lvp_outline, size = num_points, type = "regular") %>%
  #   st_cast("POINT") %>%
  #   st_as_sf() %>%
  #   mutate(area = green_spaces$area,
  #          identifier = scenario_id,
  #          ID = NA,
  #          population = NA) %>%
  #   rename(geom = x)
  # write_sf(points, gse_path)
  # points
}

new_gse <- prepare_new_gse(new_gse_path)

#' Blend New GSE Points Into Network and Convert to Undirected
#'
#' @param edges_path Character. Path to edges GeoPackage.
#' @param wkt_filter Character. WKT geometry filter for edges.
#' @param new_gse sf object. New green space entries.
#' @return sf object. Blended and undirected edges as sf.
blend_network <- function(edges_path, wkt_filter, new_gse) {
  read_sf(edges_path, wkt_filter = wkt_filter) %>%
    as_sfnetwork() %>%
    convert(to_undirected) %>%
    st_network_blend(new_gse) %>%
    activate("edges") %>%
    st_as_sf()
}

network <- blend_network(edges_path, wkt_filter, new_gse)

# Read build entries with positive population ----
build_entries <- read_sf(nodes_path, wkt_filter = wkt_filter) %>%
  filter(population > 0)

# Create output directory ----
dir.create(scenario_out_dir, showWarnings = FALSE, recursive = TRUE)

#' Merge and Save Updated Edges
#'
#' @param edges_path Character. Path to edges GeoPackage.
#' @param blended_net sf object. Blended network edges.
#' @param output_dir Character. Output directory for saving.
merge_and_save_edges <- function(edges_path, blended_net, output_dir) {
  original_edges <- read_sf(edges_path)
  updated_edges <- original_edges %>%
    filter(!(edge_id %in% blended_net$edge_id)) %>%
    bind_rows(blended_net) %>%
    mutate(edge_id = row_number()) %>%
    select(edge_id)
  
  write_sf(updated_edges, file.path(output_dir, "edges.gpkg"))
}

merge_and_save_edges(edges_path, network, scenario_out_dir)

# Read updated network edges for further processing ----
network <- read_sf(file.path(scenario_out_dir, "edges.gpkg"), wkt_filter = wkt_filter)

#' Merge and Save Updated Nodes
#'
#' @param nodes_path Character. Path to nodes GeoPackage.
#' @param exclude_id Character. Identifier to exclude.
#' @param exclude_ids Character vector. IDs to exclude.
#' @param build_entries sf object. Existing building entries.
#' @param new_gse sf object. New green space entries.
#' @param output_dir Character. Output directory for saving.
merge_and_save_nodes <- function(nodes_path, exclude_id, exclude_ids, build_entries, new_gse, output_dir) {
  original_nodes <- read_sf(nodes_path)
  updated_nodes <- original_nodes %>%
    filter(!(identifier %in% exclude_id),
           !(ID %in% exclude_ids)) %>%
    bind_rows(build_entries) %>%
    bind_rows(new_gse)
  
  write_sf(updated_nodes, file.path(output_dir, "nodes.gpkg"))
}

merge_and_save_nodes(nodes_path, scenario_id, build_entries$ID, build_entries, new_gse, scenario_out_dir)

#' Get Green Space IDs excluding the scenario ID
#'
#' @param nodes_path Character. Path to nodes GeoPackage.
#' @param wkt_filter Character. WKT geometry filter.
#' @param exclude_id Character. ID to exclude.
#' @return Character vector. Unique green space identifiers.
get_gs_ids_excluding <- function(nodes_path, wkt_filter, exclude_id) {
  read_sf(nodes_path, wkt_filter = wkt_filter) %>%
    filter(!is.na(identifier), identifier != exclude_id) %>%
    pull(identifier) %>%
    unique()
}

gs_ids <- get_gs_ids_excluding(file.path(scenario_out_dir, "nodes.gpkg"), wkt_filter, scenario_id)

# Create index directory ----
dir.create(index_dir, showWarnings = FALSE)

# Uncomment to run index calculations when ready
# calcIndices(green_space_IDs = gs_ids, in_directory = scenario_out_dir,
#             out_directory = index_dir)

# Run parameter addition and write output ----
out <- add_params(build_entries = build_entries, gs_entries = new_gse, network = network)
write_output(out, network = network, out_dir = index_dir, ID = scenario_id)

# Copy existing base indices to new scenario directory ----
base_indices_dir <- file.path(wd, "base_indices", "indices")
file_list <- list.files(base_indices_dir,
                        pattern = paste(gs_ids, collapse = "|"),
                        full.names = TRUE)
file.copy(file_list, index_dir)

# Paths to building polygons ----
buildings_path <- file.path(wd, "buildings.gpkg")

#' Gather Distance Index Data
#'
#' @param building_polygons Character. Path to building polygons GeoPackage.
#' @param index_dir Character. Directory containing index data.
#' @param output_dir Character. Directory to save DI output.
gather_distance_index <- function(building_polygons, index_dir, output_dir) {
  gatherDI(
    building_polygons = building_polygons,
    index_dir = index_dir,
    output_dir = output_dir
  )
}

gather_distance_index(buildings_path, index_dir, file.path(scenario_out_dir, "di.gpkg"))

#' Gather Link Score Data
#'
#' @param edges Character. Path to edges GeoPackage.
#' @param index_dir Character. Directory containing index data.
#' @param output_dir Character. Directory to save LS output.
gather_link_score <- function(edges, index_dir, output_dir) {
  gatherLS(
    edges = edges,
    index_dir = index_dir,
    output_dir = output_dir
  )
}

gather_link_score(file.path(scenario_out_dir, "edges.gpkg"), index_dir, file.path(scenario_out_dir, "ls.gpkg"))
