#' Calculate Network-Based Indices for Green Spaces
#'
#' Loads input data and performs network-based spatial analysis per green space ID.
#' Writes spatial and CSV outputs for each processed entry.
#'
#' @param green_space_IDs Character vector. Urban Atlas identifiers of green spaces.
#' @param in_directory Character. Path to input data directory.
#' @param out_directory Character. Directory to store output index files.
#' @param perc_core Numeric. Fraction of available CPU cores to use (default = 0.75).
#'
#' @return No return value. Side effect: writes outputs to `out_directory`.
#' @export
calcIndices <- function(green_space_IDs, in_directory, out_directory,
                        perc_core = 0.75) {
  if (!dir.exists(out_directory)) dir.create(out_directory)

  require(doParallel)
  ncore <- round(parallel::detectCores() * perc_core)
  cl <- makeCluster(ncore)
  registerDoParallel(cl)

  foreach(ID = green_space_IDs) %dopar% {
    require(sf, quietly = TRUE)
    require(sfnetworks, quietly = TRUE)
    require(tidygraph, quietly = TRUE)
    require(dplyr, quietly = TRUE)

    list.files(file.path(getwd(), "tool", "Module 3 - index building", "functions"),
               pattern = "3-1[A-Za-z].*\\.R", full.names = TRUE) %>%
      lapply(source)

    gs_entries     <- load_gs_entries(ID, in_directory)
    build_entries  <- load_build_entries(in_directory, gs_entries)
    network        <- load_network(in_directory, gs_entries)

    if (nrow(build_entries) == 0) next
    out <- add_params(build_entries, gs_entries, network)
    if (nrow(out) < 1) next

    write_output(out, network, out_directory, ID)
  }

  parallel::stopCluster(cl)
}


#' Aggregate Destination Index Values
#'
#' Combines per-node DI CSV files and joins aggregated DI values to building polygons.
#'
#' @param building_polygons Character. Path to building polygons (e.g., GPKG file).
#' @param index_dir Character. Directory containing per-node DI CSVs.
#' @param output_dir Character. Output path to write enriched buildings file.
#'
#' @return No return value. Writes to `output_dir`.
#' @export
gatherDI <- function(building_polygons, index_dir, output_dir) {
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)

  di_files <- list.files(index_dir, pattern = "nodes[1-9]*.*csv$", full.names = TRUE)

  tmp_df <- NULL
  for (i in di_files) {
    di_values <- read.csv(i)
    if (nrow(di_values) > 0 && unique(di_values$di) != Inf) {
      tmp_df <- if (is.null(tmp_df)) di_values else bind_rows(tmp_df, di_values)
    }
  }

  di_out <- tmp_df %>%
    group_by(ID) %>%
    summarise(di = sum(di), .groups = "drop")

  st_read(building_polygons, quiet = TRUE) %>%
    left_join(di_out, by = "ID") %>%
    st_write(output_dir)
}


#' Aggregate Link Strength Values
#'
#' Combines per-edge LS CSV files and joins aggregated LS values to edge geometries.
#'
#' @param edges Character. Path to edge geometries (e.g., GPKG file).
#' @param index_dir Character. Directory containing per-edge LS CSVs.
#' @param output_dir Character. Output path to write enriched edges file.
#'
#' @return No return value. Writes to `output_dir`.
#' @export
gatherLS <- function(edges, index_dir, output_dir) {
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)

  ls_files <- list.files(index_dir, pattern = "edges[1-9]*.*csv$", full.names = TRUE)

  tmp_df <- NULL
  for (i in ls_files) {
    ls_values <- read.csv(i)
    if (nrow(ls_values) > 0 && unique(ls_values$ls) != Inf) {
      tmp_df <- if (is.null(tmp_df)) ls_values else bind_rows(tmp_df, ls_values)
    }
  }

  ls_out <- tmp_df %>%
    group_by(edge_id) %>%
    summarise(ls = sum(ls), .groups = "drop")

  st_read(edges, quiet = TRUE) %>%
    left_join(ls_out, by = "edge_id") %>%
    st_write(output_dir)
}


#' Append Link Strength (LS) Values to Edges with Layer Support
#'
#' Similar to `gatherLS()` but supports specifying an output layer name (for GPKG).
#'
#' @param edges Character. Path to edge geometries (e.g., GPKG file).
#' @param index_dir Character. Directory containing LS index files.
#' @param output_dir Character. Output GPKG file path.
#' @param lyr Character. Layer name for GPKG output.
#'
#' @return No return value. Writes to `output_dir` using the specified layer.
#' @export
appendLS <- function(edges, index_dir, output_dir, lyr) {
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)

  ls_files <- list.files(index_dir, pattern = "edges[1-9]*.*csv$", full.names = TRUE)

  tmp_df <- NULL
  for (i in ls_files) {
    ls_values <- read.csv(i)
    if (nrow(ls_values) > 0 && unique(ls_values$ls) != Inf) {
      tmp_df <- if (is.null(tmp_df)) ls_values else bind_rows(tmp_df, ls_values)
    }
  }

  ls_out <- tmp_df %>%
    group_by(edge_id) %>%
    summarise(ls = sum(ls), .groups = "drop")

  st_read(edges, quiet = TRUE) %>%
    left_join(ls_out, by = "edge_id") %>%
    st_write(output_dir, layer = lyr)
}


#' Append Destination Index (DI) Values to Buildings with Layer Support
#'
#' Similar to `gatherDI()` but supports specifying an output layer name (for GPKG).
#'
#' @param building_polygons Character. Path to building polygons.
#' @param index_dir Character. Directory containing DI index CSVs.
#' @param output_dir Character. Output GPKG path.
#' @param lyr Character. Layer name for GPKG output.
#'
#' @return No return value. Writes to `output_dir` using the specified layer.
#' @export
appendDI <- function(building_polygons, index_dir, output_dir, lyr) {
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)

  di_files <- list.files(index_dir, pattern = "nodes[1-9]*.*csv$", full.names = TRUE)

  tmp_df <- NULL
  for (i in di_files) {
    di_values <- read.csv(i)
    if (nrow(di_values) > 0 && unique(di_values$di) != Inf) {
      tmp_df <- if (is.null(tmp_df)) di_values else bind_rows(tmp_df, di_values)
    }
  }

  di_out <- tmp_df %>%
    group_by(ID) %>%
    summarise(di = mean(di), .groups = "drop")

  st_read(building_polygons, quiet = TRUE) %>%
    left_join(di_out, by = "ID") %>%
    st_write(output_dir, layer = lyr)
}
