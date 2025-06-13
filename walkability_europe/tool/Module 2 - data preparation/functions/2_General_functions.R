#' Convert an `sf` Object to a Bounding Box Polygon
#'
#' Creates a rectangular polygon from the bounding box of an `sf` object.
#'
#' @param sfc_object `sf` object. The spatial object to be wrapped in a bounding box.
#' @param crs Numeric. Coordinate Reference System (default is 3035).
#'
#' @return A polygon `sf` object representing the bounding box.
#' @export
sfc2bb <- function(sfc_object, crs = 3035) {
  require(sf)
  require(dplyr)

  bb <- st_bbox(sfc_object)

  list(rbind(
    c(bb$xmin, bb$ymin),
    c(bb$xmax, bb$ymin),
    c(bb$xmax, bb$ymax),
    c(bb$xmin, bb$ymax),
    c(bb$xmin, bb$ymin)
  )) %>%
    st_polygon() %>%
    st_sfc(crs = crs) %>%
    st_sf()
}

#' Load a Buffered City Boundary from a Geopackage
#'
#' Loads a specific city boundary from a geopackage and optionally applies a buffer.
#'
#' @param city_boundaries Character. Path to the city boundaries geopackage.
#' @param city_code Character. The city code to filter (`URAU_CODE`).
#' @param buffer_dist Numeric. Buffer distance in meters (default = 0).
#' @param crs Numeric. Coordinate Reference System to transform to (default = 3035).
#'
#' @return An `sf` object of the buffered city boundary.
#' @export
boundaryLoader <- function(city_boundaries, city_code,
                           buffer_dist = 0, crs = 3035) {
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)

  query <- paste0("SELECT * FROM cities WHERE URAU_CODE LIKE '", city_code, "'")

  message("Loading city boundary...")

  st_read(city_boundaries, query = query, quiet = TRUE) %>%
    st_transform(crs) %>%
    st_buffer(buffer_dist)
}

#' Round Geometry Coordinates to Integers
#'
#' Rounds all coordinate values of an `sf` object's geometry to 0 decimal places.
#'
#' @param x `sf` object. The geometry to round.
#'
#' @return An `sf` object with rounded geometry and preserved attributes.
#' @export
roundGeometry <- function(x) {
  require(dplyr)
  require(sf)

  dat <- st_drop_geometry(x)

  st_geometry(x) %>%
    lapply(function(geom) round(geom, 0)) %>%
    st_sfc(crs = st_crs(x)) %>%
    st_sf() %>%
    bind_cols(dat) %>%
    rename(geom = geometry)
}

#' Remove Overlapping Line Geometries in a Network
#'
#' Identifies and removes overlapping lines using spatial coverage analysis.
#'
#' @param network `sf` object. A network of lines with column `geom` as geometry.
#'
#' @return An `sf` object with overlapping geometries removed.
#' @export
remove_overlap <- function(network) {
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)

  network <- network %>%
    select(geom) %>%
    distinct()

  buffered <- st_buffer(network, 1e-5)

  network$covers <- c(st_covers(buffered, network))

  covering <- filter(network, lengths(covers) > 1)
  not_covering <- filter(network, lengths(covers) < 2)

  if (nrow(covering) < 1) {
    return(select(network, geom))
  }

  difference <- st_difference(
    st_union(covering),
    st_union(not_covering)
  ) %>%
    st_cast("MULTILINESTRING", do_split = TRUE, warn = FALSE) %>%
    st_cast("LINESTRING", do_split = TRUE, warn = FALSE) %>%
    st_sf() %>%
    rename(geom = geometry)

  bind_rows(not_covering, difference) %>%
    distinct() %>%
    select(geom)
}
