################################################################################
# MODULE 4 - DELTA CALCULATION FOR INDICES
# AUTHOR: BENJAMIN LABOHM, BERLIN, 2022
################################################################################

library(dplyr)
library(sf)
library(ggplot2)
library(tidyr)

#' Load and Combine Scenario Layers
#'
#' Reads multiple spatial layers from given files, filters on non-null values
#' for a specified column, and combines them into one sf dataframe with a scenario column.
#'
#' @param file_paths Character vector of file paths to GeoPackages.
#' @param value_col Character. Name of the column to filter non-null and pivot on.
#' @param filter_condition Character. SQL WHERE condition on the column (e.g. "ls is not null").
#' @return sf object combining all layers with scenario column.
load_and_combine_scenarios <- function(file_paths, value_col, filter_condition) {
  combined_df <- NULL

  for (file_path in file_paths) {
    layer_name <- st_layers(file_path)$name[1]
    query <- paste0("SELECT * FROM ", layer_name, " WHERE ", filter_condition)
    temp_df <- read_sf(file_path, query = query) %>%
      mutate(scenario = layer_name)

    combined_df <- if (is.null(combined_df)) {
      temp_df
    } else {
      bind_rows(combined_df, temp_df)
    }
  }

  combined_df
}

# Define working directory and scenario files ----
wd <- "C:/Users/labohben/Desktop/DE008/scenarios/"
ls_scenarios <- list.files(wd, pattern = "ls", full.names = TRUE)
di_scenarios <- list.files(wd, pattern = "di.*\\.gpkg$", full.names = TRUE)

# Load and combine LS scenario data ----
ls_df <- load_and_combine_scenarios(
  file_paths = ls_scenarios,
  value_col = "ls",
  filter_condition = "ls is not null"
) %>%
  mutate(scenario = if_else(scenario == st_layers(ls_scenarios[1])$name[1], "base", scenario))

# Pivot wider and calculate delta LS values ----
ls_values <- ls_df %>%
  st_drop_geometry() %>%
  pivot_wider(names_from = scenario, values_from = ls) %>%
  mutate(
    d_ls1 = ls1 - base,
    d_ls2 = ls2 - base,
    d_ls3 = ls3 - base,
    d_ls4 = ls4 - base
  ) %>%
  left_join(read_sf(file.path(wd, "edges.gpkg")), by = "edge_id")

# Write LS delta values output ----
write_sf(ls_values, file.path(wd, "ls_values.gpkg"))

# Load and combine DI scenario data ----
di_df <- load_and_combine_scenarios(
  file_paths = di_scenarios,
  value_col = "di",
  filter_condition = "di is not null"
) %>%
  mutate(scenario = if_else(scenario == st_layers(di_scenarios[1])$name[1], "base", scenario))

# Pivot wider and calculate delta DI values ----
di_values <- di_df %>%
  st_drop_geometry() %>%
  na.omit() %>%
  distinct() %>%
  pivot_wider(names_from = scenario, values_from = di) %>%
  mutate(
    d_di1 = di1 - base,
    d_di2 = di2 - base,
    d_di3 = di3 - base,
    d_di4 = di4 - base
  ) %>%
  left_join(read_sf(file.path(gsub("scenarios/", "", wd), "buildings.gpkg")), by = "ID")

# Write DI delta values output ----
write_sf(di_values, file.path(wd, "di_values.gpkg"))

################################################################################
# Uncomment and adapt the following for scenario delta calculations as needed:
#
# base_indices_dir <- file.path(wd, "base_indices")
# base_di <- read_sf(file.path(base_indices_dir, "di.gpkg")) %>%
#   rename(base_di = di) %>%
#   na.omit() %>%
#   st_drop_geometry() %>%
#   mutate(base_di = ifelse(base_di > 1, 1, round(base_di, 3)))
#
# base_ls <- read_sf(file.path(base_indices_dir, "ls.gpkg")) %>%
#   rename(base_ls = ls) %>%
#   na.omit() %>%
#   st_drop_geometry()
#
# scenarios <- list.files(wd, pattern = "scenario", full.names = TRUE)
# for (scenario in scenarios) {
#   read_sf(file.path(scenario, "di.gpkg")) %>%
#     na.omit() %>%
#     mutate(di = ifelse(di > 1, 1, round(di, 3))) %>%
#     left_join(base_di, by = "ID") %>%
#     mutate(delta_di = di - base_di) %>%
#     write_sf(file.path(scenario, "delta_di.gpkg"))
#
#   read_sf(file.path(scenario, "ls.gpkg")) %>%
#     na.omit() %>%
#     left_join(base_ls, by = "ID") %>%
#     mutate(delta_ls = ls - base_ls) %>%
#     write_sf(file.path(scenario, "delta_ls.gpkg"))
# }
################################################################################
