#' Load City Boundary with Optional Buffer
#'
#' This function queries a city boundary from a geopackage file using a city code,
#' transforms the coordinate reference system (CRS), and optionally applies a buffer.
#'
#' @param city_code Character. The unique identifier for the city (e.g., Eurostat FUA code).
#' @param city_boundaries Character. Path to the geopackage file containing all city boundaries.
#' @param buffer_dist Numeric. Buffer distance (in meters) to apply to the city boundary. Default is 0.
#' @param crs Numeric. Coordinate reference system to which the boundary should be transformed. Default is 3035 (ETRS89 / ETRS-LAEA).
#'
#' @return An `sf` object representing the city boundary with the specified buffer and CRS.
#' @export
#'
#' @examples
#' \dontrun{
#' boundary <- boundaryLoader1("DE001", "data/input/cities.gpkg", buffer_dist = 1000)
#' }

boundaryLoader1 <- function(city_code, city_boundaries, buffer_dist = 0, crs = 3035) {
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)

  # SQL query to extract specific city
  query <- paste0("SELECT * FROM cities WHERE URAU_CODE LIKE '", city_code, "'")

  # Inform user
  message("Loading city boundary...")

  # Read and process city boundary
  city_boundary <- st_read(city_boundaries, query = query, quiet = TRUE) %>%
    st_transform(crs) %>%
    st_buffer(buffer_dist)

  return(city_boundary)
}
