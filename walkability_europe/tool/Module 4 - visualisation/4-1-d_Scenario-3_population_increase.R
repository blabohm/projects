################################################################################
# MODULE 4 - SCENARIO 3: POPULATION INCREASE
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
edges_path <- file.path(wd, "scenario1", "edges.gpkg")
ua_path <- file.path(wd, "DE008L2_LEIPZIG_UA2018_v013.gpkg")
buildings_path <- file.path(wd, "buildings.gpkg")

# Source helper functions for module 3 ----
functions_path <- file.path(getwd(), "tool", "Module 3 - index building", "functions")
source_module_functions(functions_path)

#' Retrieve Buffered WKT Geometry for a Given Identifier
#'
#' Queries nodes by identifier, creates spatial buffer, returns WKT geometry string for filtering.
#'
#' @param nodes_path Character. Path to nodes GeoPackage.
#' @param identifier Character. Node identifier to filter by.
#' @param buffer_dist Numeric. Buffer distance in CRS units.
#' @return Character. WKT string of buffered geometry.
get_buffer_wkt <- function(nodes_path, identifier, buffer_dist) {
  query <- sprintf("SELECT * FROM nodes WHERE identifier IS '%s'", identifier)
  entries <- read_sf(nodes_path, query = query)
  st_buffer(entries, buffer_dist) %>%
    st_geometry() %>%
    st_as_text()
}

# Get spatial buffer filter ----
wkt_filter <- get_buffer_wkt(nodes_path, id, buffer_distance)

#' Read Urban Atlas Population Data Filtered by WKT and Code Pattern
#'
#' Reads UA polygons filtered spatially and by code prefix; calculates high population density quantiles.
#'
#' @param ua_path Character. Path to Urban Atlas GeoPackage.
#' @param wkt_filter Character. WKT string to spatially filter UA polygons.
#' @param code_prefix Character. Regex prefix to filter UA polygons by code.
#' @return List with two data frames: full UA pop data and high density quantiles.
read_ua_population <- function(ua_path, wkt_filter, code_prefix = "^11") {
  ua_lyr <- st_layers(ua_path)$name[1]
  ua_pop <- read_sf(ua_path, wkt_filter = wkt_filter, layer = ua_lyr) %>%
    filter(grepl(code_prefix, code_2018)) %>%
    st_drop_geometry()
  
  hd_pop <- ua_pop %>%
    mutate(pop_per_m = Pop2018 / area) %>%
    group_by(code_2018) %>%
    summarise(pop_high = quantile(pop_per_m, 0.95, na.rm = TRUE))
  
  list(ua_pop = ua_pop, hd_pop = hd_pop)
}

ua_data <- read_ua_population(ua_path, wkt_filter)

#' Prepare Building Entries with Adjusted Population Estimates
#'
#' Joins building data with UA population, calculates adjusted population for scenario.
#'
#' @param nodes_path Character. Path to nodes GeoPackage.
#' @param wkt_filter Character. WKT string for spatial filtering.
#' @param ua_pop DataFrame. Urban Atlas population data.
#' @param hd_pop DataFrame. High density population quantiles.
#' @return sf object. Updated building entries with adjusted population.
prepare_build_entries <- function(nodes_path, wkt_filter, ua_pop, hd_pop) {
  read_sf(nodes_path, wkt_filter = wkt_filter) %>%
    filter(population > 0) %>%
    select(-area) %>%
    mutate(identifier = str_extract(ID, ".*L2")) %>%
    left_join(ua_pop, by = "identifier") %>%
    left_join(hd_pop, by = "code_2018") %>%
    mutate(
      Pop2018_new = area * pop_high,
      population_new = round(population / Pop2018 * Pop2018_new),
      population_new = ifelse(population_new < population,
                              round(population * 1.2),
                              population_new)
    ) %>%
    transmute(
      identifier = NA_character_,
      area = NA_real_,
      population = population_new,
      ID,
      geom = geometry
    ) %>%
    st_as_sf()
}

build_entries <- prepare_build_entries(nodes_path, wkt_filter, ua_data$ua_pop, ua_data$hd_pop)

# Read green space entries ----
new_gse <- read_sf(nodes_path, wkt_filter = wkt_filter) %>%
  filter(!is.na(area))

# Create output directories ----
out_dir <- file.path(wd, "scenario3")
index_dir <- file.path(out_dir, "indices")
dir.create(index_dir, recursive = TRUE, showWarnings = FALSE)

# Write updated nodes (replace old entries with adjusted population) ----
read_sf(nodes_path) %>%
  filter(!(ID %in% build_entries$ID)) %>%
  bind_rows(build_entries) %>%
  write_sf(file.path(out_dir, "nodes.gpkg"))

# Extract green space IDs for indices calculation ----
gs_ids <- read_sf(file.path(out_dir, "nodes.gpkg"), wkt_filter = wkt_filter) %>%
  filter(!is.na(identifier), identifier != id) %>%
  pull(identifier) %>%
  unique()

# Copy scenario edges to output ----
file.copy(edges_path, file.path(out_dir, "edges.gpkg"), overwrite = TRUE)

# Read network edges ----
net <- read_sf(file.path(out_dir, "edges.gpkg"), wkt_filter = wkt_filter)

# Calculate network indices ----
# Uncomment below to run index calculations
# calcIndices(green_space_IDs = gs_ids, in_directory = out_dir, out_directory = index_dir)

# Add parameters for new building and green space entries to network ----
out <- add_params(build_entries = build_entries, gs_entries = new_gse, network = net)

# Copy base index files for green spaces ----
base_indices_dir <- file.path(wd, "base_indices", "indices")
flist <- list.files(base_indices_dir,
                    pattern = paste(gs_ids, collapse = "|"),
                    full.names = TRUE)
file.copy(flist, index_dir, overwrite = TRUE)

# Gather Distance Index (DI) for updated buildings ----
gatherDI(building_polygons = buildings_path,
         index_dir = index_dir,
         output_dir = file.path(out_dir, "di.gpkg"))

# Gather Link Score (LS) for updated edges ----
gatherLS(edges = edges_path,
         index_dir = index_dir,
         output_dir = file.path(out_dir, "ls.gpkg"))
