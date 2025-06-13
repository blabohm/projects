################################################################################
# MODULE 4 - SCENARIO 2: GREEN SPACE DEVELOPMENT
# AUTHOR: BENJAMIN LABOHM, BERLIN, 2022
################################################################################

library(dplyr)
library(sf)
library(sfnetworks)
library(ggplot2)
library(tidygraph)

#' Source Module 3 Functions
#'
#' Loads R scripts from the module 3 functions directory.
#'
#' @param path Character. Directory path containing R scripts.
#' @param pattern Character. Regex pattern to filter files.
source_module_functions <- function(path, pattern = "3.*\\.R") {
  list.files(path, pattern = pattern, full.names = TRUE) %>%
    purrr::walk(source)
}

# Set working directory and constants ----
wd <- "C:/Users/labohben/Desktop/DE008/"
identifier <- "23473-DE008L2"
buffer_distance <- 1000

# Paths to spatial datasets ----
nodes_path <- file.path(wd, "nodes.gpkg")
edges_path <- file.path(wd, "scenario1", "edges.gpkg")

# Source helper functions ----
functions_path <- file.path(getwd(), "tool", "Module 3 - index building", "functions")
source_module_functions(functions_path)

#' Get buffered WKT geometry for spatial filtering by identifier
#'
#' @param nodes_path Character. Path to nodes GeoPackage.
#' @param id Character. Identifier to filter nodes.
#' @param buffer_dist Numeric. Buffer distance around filtered features.
#' @return Character. WKT string of buffered geometry.
get_buffer_wkt <- function(nodes_path, id, buffer_dist) {
  query <- sprintf("SELECT * FROM nodes WHERE identifier IS '%s'", id)
  entries <- read_sf(nodes_path, query = query)
  st_buffer(entries, buffer_dist) %>%
    st_geometry() %>%
    st_as_text()
}

# Obtain spatial filter WKT based on identifier ----
wkt_filter <- get_buffer_wkt(nodes_path, identifier, buffer_distance)

#' Query multiple target green space IDs from nodes dataset
#'
#' @param nodes_path Character. Path to nodes GeoPackage.
#' @param target_ids Character vector. Target identifiers to query.
#' @return sf object with queried green space entries.
query_target_green_spaces <- function(nodes_path, target_ids) {
  query <- paste0("SELECT * FROM nodes WHERE ",
                  paste(sprintf("identifier = '%s'", target_ids), collapse = " OR "))
  read_sf(nodes_path, query = query)
}

target_ids <- c("23502-DE008L2", "23493-DE008L2", "23485-DE008L2",
                "23508-DE008L2", "23509-DE008L2")

target_gs <- query_target_green_spaces(nodes_path, target_ids)

# Urban atlas data paths and layer ----
ua_dir <- "Z:/input/UA2018/DE008L2_LEIPZIG_UA2018_v013/Data/DE008L2_LEIPZIG_UA2018_v013.gpkg"
ua_layer <- st_layers(ua_dir)$name[1]

#' Extract and calculate 2018 population density per square meter from UA
#'
#' @param ua_dir Character. Path to Urban Atlas GeoPackage.
#' @param ua_layer Character. Urban Atlas layer name.
#' @param wkt_filter Character. WKT for spatial filtering.
#' @return sf object with population density added.
calculate_pop_density <- function(ua_dir, ua_layer, wkt_filter) {
  pop_query <- sprintf("SELECT Pop2018, area FROM %s WHERE code_2018 = 11100", ua_layer)
  read_sf(ua_dir, query = pop_query, wkt_filter = wkt_filter) %>%
    mutate(pop_per_m = Pop2018 / area)
}

hd_pop <- calculate_pop_density(ua_dir, ua_layer, wkt_filter)

pop_per_m_95 <- quantile(hd_pop$pop_per_m, probs = 0.95, na.rm = TRUE)

#' Create new building entries by distributing population evenly across target GS
#'
#' @param target_gs sf object. Green space entries to convert.
#' @param pop_density_95 Numeric. 95th percentile population density.
#' @return sf object with new building entries.
create_new_building_entries <- function(target_gs, pop_density_95) {
  target_gs %>%
    group_by(identifier) %>%
    mutate(population = round(area * pop_density_95 / n()),
           ID = identifier,
           area = NA_real_,
           identifier = NA_character_) %>%
    ungroup()
}

new_building_entries <- create_new_building_entries(target_gs, pop_per_m_95)

# Read existing building entries with population > 0 and combine with new buildings ----
building_entries <- read_sf(nodes_path, wkt_filter = wkt_filter) %>%
  filter(population > 0) %>%
  bind_rows(new_building_entries)

# Read green space entries excluding target IDs ----
green_space_entries <- read_sf(nodes_path, wkt_filter = wkt_filter) %>%
  filter(!is.na(area), !(identifier %in% target_ids))

# Output directories ----
out_dir <- file.path(wd, "scenario2")
index_dir <- file.path(out_dir, "indices")
dir.create(index_dir, recursive = TRUE, showWarnings = FALSE)

# Write updated nodes file without target IDs, append new buildings ----
read_sf(nodes_path) %>%
  filter(!(identifier %in% target_ids)) %>%
  bind_rows(new_building_entries) %>%
  write_sf(file.path(out_dir, "nodes.gpkg"))

# Get green space IDs for index calculation ----
gs_ids <- read_sf(file.path(out_dir, "nodes.gpkg"), wkt_filter = wkt_filter) %>%
  filter(!is.na(identifier), identifier != identifier) %>%
  pull(identifier) %>%
  unique()

# Copy edges file to output directory ----
file.copy(edges_path, out_dir, overwrite = TRUE)

# Read network edges with filter ----
network <- read_sf(file.path(out_dir, "edges.gpkg"), wkt_filter = wkt_filter)

# Run network parameter addition function ----
out <- add_params(build_entries = building_entries,
                  gs_entries = green_space_entries,
                  network = network)

write_output(out, network = network, out_dir = index_dir, ID = identifier)

# Copy relevant index files to scenario2 indices directory ----
base_indices_dir <- file.path(wd, "base_indices", "indices")
file_list <- list.files(base_indices_dir,
                        pattern = paste(gs_ids, collapse = "|"),
                        full.names = TRUE)
file.copy(file_list, index_dir, overwrite = TRUE)

# Paths to building polygons and index directory for gathering indices ----
buildings_path <- file.path(wd, "buildings.gpkg")

# Gather distance index (DI) ----
gatherDI(building_polygons = buildings_path,
         index_dir = index_dir,
         output_dir = file.path(out_dir, "di.gpkg"))

# Gather link score (LS) ----
gatherLS(edges = edges_path,
         index_dir = index_dir,
         output_dir = file.path(out_dir, "ls.gpkg"))
