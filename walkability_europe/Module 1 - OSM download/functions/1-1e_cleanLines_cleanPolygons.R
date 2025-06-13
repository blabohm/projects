#' Clean and Standardize OSM Building Polygons
#'
#' Converts the coordinate reference system (CRS) to EPSG:4326 and ensures the
#' geometry type is `POLYGON`. This function filters for building features only.
#'
#' @param osm_sf_object An `sf` object containing OSM data with polygon features.
#'
#' @return An `sf` object with clean building polygons, or `NULL` if an error occurs.
#' @export
#'
#' @examples
#' \dontrun{
#' cleanPolygons(osm_data)
#' }

cleanPolygons <- function(osm_sf_object) {
  suppressMessages({
    tryCatch({
      st_crs(osm_sf_object) <- st_crs(4326)

      osm_sf_object %>%
        select(matches("building$")) %>%
        st_cast("MULTIPOLYGON") %>%
        st_cast("POLYGON", do_split = TRUE, warn = FALSE)
    }, error = function(e) {
      return(NULL)
    }) %>%
      return()
  })
}

#' Clean and Standardize OSM Highway Lines
#'
#' Converts the CRS to EPSG:4326 and ensures geometry type is `LINESTRING`.
#' This function filters for highway features only.
#'
#' @param osm_sf_object An `sf` object containing OSM data with line features.
#'
#' @return An `sf` object with clean highway lines, or `NULL` if an error occurs.
#' @export
#'
#' @examples
#' \dontrun{
#' cleanLines(osm_data)
#' }

cleanLines <- function(osm_sf_object) {
  suppressMessages({
    tryCatch({
      st_crs(osm_sf_object) <- st_crs(4326)

      osm_sf_object %>%
        select(matches("highway")) %>%
        st_cast("MULTILINESTRING") %>%
        st_cast("LINESTRING", do_split = TRUE, warn = FALSE)
    }, error = function(e) {
      return(NULL)
    }) %>%
      return()
  })
}
