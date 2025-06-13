################################################################################
# MODULE 4 - VISUALISATION: BASE MAP CREATION
# AUTHOR: BENJAMIN LABOHM, BERLIN, 2022
################################################################################

library(dplyr)
library(sf)
library(sfnetworks)
library(ggplot2)
library(tidygraph)
library(stringr)

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

# Set working directory and identifiers ----
wd <- "C:/Users/labohben/Desktop/DE008/"
id <- "23473-DE008L2"
buffer_distance <- 1000

# Paths to spatial data files ----
nodes_path <- file.path(wd, "nodes.gpkg")
edges_path <- file.path(wd, "edges_new.gpkg")

# Source helper functions for module 3 ----
functions_path <- file.path(getwd(), "tool", "Module 3 - index building", "functions")
source_module_functions(functions_path)

#' Read Nodes for a Specific Identifier and Create Spatial Buffer
#'
#' Queries nodes matching an identifier, creates a buffer, and returns
#' the buffer geometry as WKT for filtering.
#'
#' @param nodes_path Character. Path to nodes GeoPackage.
#' @param identifier Character. Identifier to filter nodes.
#' @param buffer_dist Numeric. Buffer distance in units of CRS.
#' @return Character. WKT representation of buffered geometry.
get_buffer_wkt <- function(nodes_path, identifier, buffer_dist) {
  query <- sprintf("SELECT * FROM nodes WHERE identifier IS '%s'", identifier)
  entries <- read_sf(nodes_path, query = query)
  st_buffer(entries, buffer_dist) %>%
    st_geometry() %>%
    st_as_text()
}

# Get spatial filter for nodes ----
wkt_filter <- get_buffer_wkt(nodes_path, id, buffer_distance)

#' Retrieve Unique Green Space Identifiers within Buffer
#'
#' Reads nodes filtered by WKT and extracts unique non-missing identifiers.
#'
#' @param nodes_path Character. Path to nodes GeoPackage.
#' @param wkt_filter Character. WKT string to filter features.
#' @return Character vector of unique identifiers.
get_green_space_ids <- function(nodes_path, wkt_filter) {
  read_sf(nodes_path, wkt_filter = wkt_filter) %>%
    filter(!is.na(identifier)) %>%
    pull(identifier) %>%
    unique()
}

gs_ids <- get_green_space_ids(nodes_path, wkt_filter)

# Define output directories ----
out_dir <- file.path(wd, "base_indices")
index_dir <- file.path(out_dir, "indices")

dir.create(index_dir, recursive = TRUE, showWarnings = FALSE)

# Copy base data files to output directory ----
file.copy(nodes_path, file.path(out_dir, "nodes.gpkg"), overwrite = TRUE)
file.copy(edges_path, file.path(out_dir, "edges.gpkg"), overwrite = TRUE)

#' Calculate Network Indices
#'
#' Wrapper function calling the `calcIndices` function with parameters.
#'
#' @param green_space_IDs Character vector. IDs of green spaces to process.
#' @param in_directory Character. Input directory path.
#' @param out_directory Character. Output directory path.
calculate_network_indices <- function(green_space_IDs, in_directory, out_directory) {
  calcIndices(
    green_space_IDs = green_space_IDs,
    in_directory = in_directory,
    out_directory = out_directory
  )
}

calculate_network_indices(gs_ids, out_dir, index_dir)

# Define building polygons path ----
buildings_path <- file.path(wd, "buildings.gpkg")

#' Gather Distance Index Data
#'
#' Calls `gatherDI` to process building polygons and save DI output.
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

gather_distance_index(buildings_path, index_dir, file.path(out_dir, "di.gpkg"))

#' Gather Link Score Data
#'
#' Calls `gatherLS` to process edge data and save LS output.
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

gather_link_score(edges_path, index_dir, file.path(out_dir, "ls.gpkg"))
