################################################################################
# MODULE 4 - GATHER METRICS
# AUTHOR: BENJAMIN LABOHM, BERLIN, 2022
################################################################################

library(dplyr)
library(sf)
library(ggplot2)

#' Source Module Functions
#'
#' Sources all R scripts from the specified directory matching the given pattern.
#'
#' @param path Character. Directory path containing functions.
#' @param pattern Character. Regex pattern to filter function files.
#' @return Invisibly returns NULL; side effect: sources files.
source_module_functions <- function(path, pattern = "2-2[A-Za-z].*\\.R|2_.*\\.R") {
  list.files(path, pattern = pattern, full.names = TRUE) %>%
    purrr::walk(source)
}

# Define paths and load functions ----
wd <- "Z:/output/"
cities <- list.files(wd)
ua_directory <- sub("output/", "input/UA2018/", wd)
city_boundaries_path <- sub("output/", "input/cities.gpkg", wd)

functions_path <- file.path(getwd(), "tool", "Module 2 - data preparation", "functions")
source_module_functions(functions_path)

#' Process a single city to summarize Local Significance (LS) metrics
#'
#' Reads node and local significance data for a city,
#' joins them spatially, and aggregates LS by identifier.
#'
#' @param city_code Character. City code to process.
#' @param wd Character. Working directory containing city subfolders.
#' @return Tibble summarizing LS by identifier for the city.
process_city_ls <- function(city_code, wd) {
  message(sprintf("Processing city %s (%d of %d)", city_code, 
                  which(cities == city_code), length(cities)))
  tryCatch({
    node_query <- "SELECT * FROM nodes WHERE identifier IS NOT NULL"
    gse <- list.files(wd, pattern = city_code, full.names = TRUE) %>%
      file.path(., "nodes.gpkg") %>%
      read_sf(query = node_query) %>%
      st_buffer(dist = 1)

    ls_query <- "SELECT * FROM local_significance WHERE ls IS NOT NULL"
    ls <- list.files(wd, pattern = city_code, full.names = TRUE) %>%
      file.path(., "local_significance.gpkg") %>%
      read_sf(query = ls_query)

    gse %>%
      st_join(ls, left = TRUE) %>%
      st_drop_geometry() %>%
      filter(!is.na(ls)) %>%
      group_by(identifier) %>%
      summarise(ls = sum(ls, na.rm = TRUE), .groups = "drop") %>%
      mutate(city_code = city_code)

  }, error = function(e) {
    warning(sprintf("Failed processing city %s: %s", city_code, e$message))
    return(tibble())
  })
}

# Process all cities and combine results ----
out <- purrr::map_dfr(cities, process_city_ls, wd = wd)

# Write combined LS metrics to CSV ----
write.csv(out, file.path("Z:/", "ls_full.csv"), row.names = FALSE)

#' Load and prepare city information for visualization
#'
#' Reads city info and merges coverage and boundary data.
#'
#' @param city_boundaries_path Character. Path to city boundaries GeoPackage.
#' @param coverage_path Character. Path to percent OSM coverage CSV.
#' @param city_info_path Character. Path to city info CSV.
#' @return sf object with city boundaries enriched with coverage and info.
load_city_sf <- function(city_boundaries_path, coverage_path, city_info_path) {
  city_info <- read.csv(city_info_path) %>%
    tibble() %>%
    mutate(city_code = substr(URAU_COD_1, 1, 5),
           URAU_NAME = ifelse(city_code == "XK003", "Mitrovica", URAU_NAME))

  perc_coverage <- read.csv(coverage_path)

  read_sf(city_boundaries_path) %>%
    rename(city_code = URAU_CODE) %>%
    left_join(perc_coverage, by = "city_code") %>%
    left_join(city_info, by = "city_code")
}

city_sf <- load_city_sf(
  city_boundaries_path = city_boundaries_path,
  coverage_path = "Z:/MA/percent_OSM_coverage.csv",
  city_info_path = "Z:/city_info.csv"
)

# Load NUTS boundaries for context ----
nuts <- read_sf("Z:/nuts/NUTS_RG_20M_2021_3035.gpkg") %>%
  filter(LEVL_CODE == 0) %>%
  select(NUTS_ID)

#' Plot percent coverage map with NUTS context
#'
#' Creates a ggplot visualization showing percent coverage of UA residential polygons
#' covered by OSM buildings.
#'
#' @param nuts sf object of NUTS boundaries.
#' @param city_sf sf object of cities with coverage data.
plot_percent_coverage_map <- function(nuts, city_sf) {
  ggplot() +
    geom_sf(data = nuts) +
    geom_sf(data = city_sf, aes(col = percent_coverage, fill = percent_coverage)) +
    labs(
      title = "Percent of UA residential class polygons covered by OSM buildings",
      color = "Percent coverage"
    ) +
    guides(fill = "none") +
    theme(legend.position = "bottom") +
    coord_sf(
      xlim = c(2700000, 5748970),
      ylim = c(1500000, 4500000)
    )
}

plot_percent_coverage_map(nuts, city_sf)
