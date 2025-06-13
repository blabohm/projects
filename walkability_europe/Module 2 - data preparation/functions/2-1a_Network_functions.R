#' List Network Tiles Matching City Code
#'
#' Lists `.gpkg` files in a directory that match a given city code.
#'
#' @param network_tile_directory Character. Path to the directory containing network tile files.
#' @param city_code Character. City code used to filter relevant files.
#'
#' @return A character vector of matching file paths.
#' @export
listTiles <- function(network_tile_directory, city_code) {
  require(dplyr, quietly = TRUE)

  list.files(
    path = network_tile_directory,
    pattern = city_code,
    full.names = TRUE
  ) %>%
    tibble(path = .) %>%
    filter(grepl("\\.gpkg$", path)) %>%
    pull(path)
}

#' Load and Combine Network Files
#'
#' Combines multiple network tile files into a single spatial object, filtering out motorways.
#'
#' @param file_list Character vector. File paths to `.gpkg` network tiles.
#' @param tmp_dir Character. Path to a temporary directory or file for intermediate output.
#' @param boundary sf object. Optional city boundary for spatial filtering. Default is `NULL`.
#' @param crs Integer. Coordinate reference system. Default is `3035` (ETRS89 / LAEA Europe).
#'
#' @return An `sf` object with the combined and filtered network lines.
#' @export
net_combinator <- function(file_list, tmp_dir, boundary = NULL, crs = 3035) {
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)

  tmpOut <- if (grepl("//$", tmp_dir)) {
    tmp_dir
  } else {
    file.path(dirname(tmp_dir), "tmp-file.gpkg")
  }

  if (file.exists(tmpOut)) unlink(tmpOut)

  if (!is.null(boundary)) {
    boundary <- boundary %>%
      st_transform(4326) %>%
      st_geometry() %>%
      st_as_text()
  }

  message("\nStarting network combination and filtering\n")

  for (file in file_list) {
    layer_name <- st_layers(file)$name %>%
      grep("line", ., ignore.case = TRUE, value = TRUE)

    tmp <- st_read(file, quiet = TRUE, layer = layer_name, wkt_filter = boundary) %>%
      st_transform(crs)

    if (nrow(tmp) == 0) next

    tmp %>%
      select(highway = matches("^highway$")) %>%
      filter(!grepl("motorway", highway)) %>%
      st_cast("MULTILINESTRING", do_split = TRUE, warn = FALSE) %>%
      st_cast("LINESTRING", do_split = TRUE, warn = FALSE) %>%
      st_write(dsn = tmpOut, layer = "osm_paths", quiet = TRUE, append = TRUE)
  }

  output <- st_read(tmpOut, quiet = TRUE)
  unlink(tmpOut)

  return(output)
}

#' Clean OSM Network
#'
#' Cleans and simplifies a LINESTRING network by removing overlaps, duplicates,
#' pseudo-nodes, and unconnected edges. Subdivides edges at interior points.
#'
#' @param network An `sf` object of class LINESTRING.
#' @param crs Integer. Target coordinate reference system. Default is `3035`.
#'
#' @return A cleaned `sf` LINESTRING object.
#' @export
networkCleaner <- function(network, crs = 3035) {
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)
  require(tidygraph, quietly = TRUE)
  require(sfnetworks, quietly = TRUE)

  network %>%
    distinct() %>%
    st_geometry() %>%
    lapply(function(x) round(x, 0)) %>%
    st_sfc(crs = crs) %>%
    as_sfnetwork() %>%
    convert(to_spatial_subdivision) %>%
    convert(to_spatial_smooth) %>%
    filter(group_components() == 1) %>%
    activate("edges") %>%
    st_geometry() %>%
    st_as_sf() %>%
    rename(geom = x) %>%
    remove_overlap()
}
