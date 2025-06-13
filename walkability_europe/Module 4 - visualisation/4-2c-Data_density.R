################################################################################
# MODULE 4 - DATA DENSITY EUROPE
# AUTHOR: BENJAMIN LABOHM, BERLIN, 2022
################################################################################

library(ggplot2)
library(sf)
library(dplyr)
library(RColorBrewer)
library(ggspatial)
library(cowplot)

#' Initialize Paths and Parameters
#'
#' Sets working directories, codes, and file paths.
#'
#' @param base_output_dir Character. Base output directory path.
#' @param base_input_dir Character. Base input directory path.
#' @param urban_atlas_dir Character. Urban Atlas data directory path.
#' @param codes Character vector. List of city codes to process.
#' @return Named list of paths and parameters.
init_paths_and_codes <- function(base_output_dir, base_input_dir, urban_atlas_dir, codes) {
  list(
    wd = base_output_dir,
    wd1 = base_input_dir,
    ua = urban_atlas_dir,
    codes = codes
  )
}

params <- init_paths_and_codes(
  base_output_dir = "Z:/output/",
  base_input_dir = "Z:/input/",
  urban_atlas_dir = "Z:/input/UA2018/",
  codes = c("PT001", "AT001")
)

code <- params$codes[1]

#' Build file paths based on code and directories
#'
#' @param wd Character. Output directory.
#' @param wd1 Character. Input directory.
#' @param ua Character. Urban atlas directory.
#' @param code Character. City code.
#' @return Named list with paths.
build_file_paths <- function(wd, wd1, ua, code) {
  list(
    build_poly = file.path(wd, code, "buildings.gpkg"),
    boundaries = file.path(wd1, "cities.gpkg"),
    ua_dir = list.files(
      ua, pattern = code, full.names = TRUE
    ) %>%
      list.files(pattern = "\\.gpkg$", recursive = TRUE, full.names = TRUE)
  )
}

paths <- build_file_paths(params$wd, params$wd1, params$ua, code)

#' Load city boundary and create spatial filter WKT
#'
#' @param boundaries_path Character. Path to cities geopackage.
#' @param code Character. City code to filter.
#' @return List with sf object (boundary) and WKT string (filter).
load_boundary_and_filter <- function(boundaries_path, code) {
  query <- sprintf("SELECT * FROM cities WHERE URAU_CODE = '%s'", code)
  boundary <- read_sf(boundaries_path, query = query)
  filter_wkt <- boundary %>%
    st_as_sfc() %>%
    st_as_text()
  list(boundary = boundary, filter_wkt = filter_wkt)
}

boundary_data <- load_boundary_and_filter(paths$boundaries, code)

#' Load Urban Atlas Data filtered by boundary
#'
#' @param ua_path Character. Path to Urban Atlas geopackage.
#' @param filter_wkt Character. WKT string to spatially filter data.
#' @return sf object filtered Urban Atlas data.
load_ua_data <- function(ua_path, filter_wkt) {
  layer_name <- st_layers(ua_path)$name[1]
  query <- sprintf("SELECT * FROM %s", layer_name)
  read_sf(ua_path, query = query, wkt_filter = filter_wkt)
}

ua_data_full <- load_ua_data(paths$ua_dir, boundary_data$filter_wkt)

#' Load Building Polygons filtered by boundary
#'
#' @param building_path Character. Path to buildings geopackage.
#' @param filter_wkt Character. WKT string for spatial filtering.
#' @return sf object filtered building polygons.
load_buildings <- function(building_path, filter_wkt) {
  read_sf(building_path, wkt_filter = filter_wkt) %>%
    select(geom)
}

buildings_full <- load_buildings(paths$build_poly, boundary_data$filter_wkt)

#' Filter residential Urban Atlas polygons
#'
#' @param ua_data sf object. Urban Atlas data.
#' @return sf object residential polygons only.
filter_residential <- function(ua_data) {
  filter(ua_data, grepl("^11.*", code_2018))
}

residential_full <- filter_residential(ua_data_full)

#' Identify covered and non-covered residential polygons by buildings
#'
#' @param residential_sf sf object. Residential polygons.
#' @param buildings_sf sf object. Building polygons.
#' @return List with covered and not covered residential polygons.
identify_coverage <- function(residential_sf, buildings_sf) {
  covered <- st_filter(residential_sf, buildings_sf)
  not_covered <- filter(residential_sf, !(identifier %in% covered$identifier))
  list(covered = covered, not_covered = not_covered)
}

coverage <- identify_coverage(residential_full, buildings_full)

#' Plot city boundary and residential coverage
#'
#' @param boundary sf object. City boundary.
#' @param covered sf object. Covered residential polygons.
#' @param not_covered sf object. Not covered residential polygons.
#' @return ggplot object.
plot_coverage <- function(boundary, covered, not_covered) {
  ggplot() +
    geom_sf(data = boundary, color = "grey30", fill = "grey70") +
    geom_sf(data = covered, color = "red3", fill = "red3") +
    geom_sf(data = not_covered, color = "grey50", fill = "grey50")
}

base_plot <- plot_coverage(boundary_data$boundary, coverage$covered, coverage$not_covered)

print(base_plot)

# Uncomment and adapt the following sections as needed for LS and DI plots
# # LS (Local Significance) Plotting (example placeholder)
# ls_query <- "SELECT * FROM local_significance WHERE ls IS NOT NULL"
# ls_values <- read_sf(file.path(params$wd, code, "local_significance.gpkg"),
#                      query = ls_query, wkt_filter = boundary_data$filter_wkt)
# ls_plot <- base_plot +
#   ls_values %>%
#   filter(!is.na(ls)) %>%
#   mutate(ls = log(ls)) %>%
#   arrange(ls) %>%
#   geom_sf(aes(color = ls), size = 2) +
#   scale_color_distiller(palette = "RdBu") +
#   labs(color = "LS") +
#   annotation_scale(style = "ticks")
# print(ls_plot)
#
# # DI (Detour Index) Plotting (example placeholder)
# di_query <- "SELECT * FROM detour_index WHERE di IS NOT NULL"
# di_values <- read_sf(file.path(params$wd, code, "detour_index.gpkg"),
#                      query = di_query, wkt_filter = boundary_data$filter_wkt)
# di_plot <- base_plot +
#   di_values %>%
#   filter(!is.na(di)) %>%
#   geom_sf(aes(fill = di, color = di)) +
#   scale_fill_distiller(palette = "RdBu") +
#   scale_color_distiller(palette = "RdBu") +
#   labs(fill = "DI", color = "DI") +
#   annotation_scale(style = "ticks")
# print(di_plot)
