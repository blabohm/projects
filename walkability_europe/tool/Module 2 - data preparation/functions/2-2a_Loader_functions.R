#' Load Urban Atlas Residential Areas
#'
#' This function loads Urban Atlas (UA) residential land-use polygons for a given city.
#'
#' @param ua_dir Character. Directory containing Urban Atlas GPKG files.
#' @param fua_code Character. Functional Urban Area (FUA) code used to identify the city.
#' @param boundary sf object. City boundary geometry for spatial filtering.
#' @param crs Integer. Coordinate reference system to transform the output. Default is 3035 (ETRS89 / LAEA Europe).
#' @param res_class Numeric vector. List of UA residential land use class codes to filter. 
#'   Defaults to common residential classes.
#'
#' @return An `sf` object containing filtered residential areas.
#' @export
#'
#' @examples
#' \dontrun{
#' UAresLoader("data/UA2018", "DE001", city_boundary)
#' }

UAresLoader <- function(
  ua_dir,
  fua_code,
  boundary,
  crs = 3035,
  res_class = c(11100, 11210, 11220, 11230, 11240, 11300)
) {
  require(dplyr)
  require(sf)

  # Locate the relevant UA file
  ua_file <- list.files(
    path = ua_dir,
    pattern = fua_code,
    full.names = TRUE
  ) %>%
    list.files(
      pattern = "\\.gpkg$",
      full.names = TRUE,
      recursive = TRUE
    )

  # Extract the first layer name
  layer_name <- st_layers(ua_file)$name[1]

  # Convert boundary to WKT
  boundary_wkt <- boundary %>%
    st_geometry() %>%
    st_as_text()

  message("\nLoading residential areas...\n")

  # Read and process the Urban Atlas file
  st_read(ua_file, layer = layer_name, wkt_filter = boundary_wkt, quiet = TRUE) %>%
    filter(code_2018 %in% res_class) %>%
    st_transform(crs) %>%
    select(Pop2018, identifier, code_2018)
}

#' Load OSM Buildings
#'
#' Loads building footprints from OSM and processes the geometry.
#'
#' @param osm_file Character. Path to the OSM buildings GPKG file.
#' @param boundary sf object. City boundary to spatially filter buildings.
#' @param crs Integer. Desired CRS to transform results. Default is 3035.
#'
#' @return An `sf` object containing filtered building geometries.
#' @export
#'
#' @examples
#' \dontrun{
#' OSMloader("data/osm/buildings.gpkg", city_boundary)
#' }

OSMloader <- function(
  osm_file,
  boundary,
  crs = 3035
) {
  require(dplyr)
  require(sf)

  boundary_wkt <- boundary %>%
    st_transform(4326) %>%
    st_geometry() %>%
    st_as_text()

  buildings <- osm_file %>%
    st_read(wkt_filter = boundary_wkt, quiet = TRUE) %>%
    st_make_valid() %>%
    filter(!st_is_empty(.)) %>%
    st_cast("MULTIPOLYGON", do_split = TRUE, warn = FALSE) %>%
    st_cast("POLYGON", do_split = TRUE, warn = FALSE)

  if (!exists("buildings")) return(NULL)

  buildings %>%
    st_transform(crs) %>%
    select(matches("build|geom"))
}

#' Load OSM Network
#'
#' Loads and filters OSM road network data for a given city.
#'
#' @param network_dir Character. Path to the OSM network GPKG file.
#' @param osm_buildings Optional `sf` object. If provided, network is spatially filtered to building extents.
#' @param crs Integer. Desired CRS for the output. Default is 3035.
#'
#' @return An `sf` object containing the filtered or full road network.
#' @export
#'
#' @examples
#' \dontrun{
#' networkLoader("data/osm/network.gpkg", osm_buildings)
#' }

networkLoader <- function(
  network_dir,
  osm_buildings = NULL,
  crs = 3035
) {
  require(dplyr)
  require(sf)

  message("\nLoading OSM network...\n")

  network <- network_dir %>%
    st_read(quiet = TRUE) %>%
    select(highway) %>%
    st_transform(crs)

  if (!is.null(osm_buildings)) {
    st_filter(network, sfc2bb(osm_buildings), .predicate = st_intersects)
  } else {
    network
  }
}
