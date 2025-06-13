#' Create City Grid
#'
#' Generates a grid over the city boundary area with a defined cell size.
#'
#' @param city_code Character. Eurostat Urban Audit city code.
#' @param city_boundaries Character. Path to the geopackage or database table containing city boundaries.
#' @param cellsize Numeric. Cell size for grid creation in coordinate units.
#' @param crs Numeric. Coordinate reference system (default: 3035).
#'
#' @return An `sf` object representing the grid clipped to the city boundary.
#' @export
mkCityGrid <- function(city_code, city_boundaries, cellsize, crs = 3035) {
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)

  query <- paste0("SELECT * FROM 'cities' WHERE URAU_CODE = '", city_code, "'")

  city_boundary <- st_read(city_boundaries, query = query, quiet = TRUE) %>%
    st_transform(crs) %>%
    st_buffer(1000)

  st_make_grid(city_boundary, cellsize = cellsize) %>%
    st_as_sf() %>%
    st_filter(city_boundary, .pred = st_intersects)
}

#' Get Grid Box Geometry as WKT
#'
#' Extracts the geometry of a specific grid cell as WKT (well-known text).
#'
#' @param cityGrid `sf` object. The city grid produced by `mkCityGrid()`.
#' @param i Integer. The index of the grid cell to extract.
#'
#' @return Character. WKT representation of the selected grid cell geometry.
#' @export
getGb <- function(cityGrid, i) {
  cityGrid[i, ] %>%
    sf::st_geometry() %>%
    sf::st_as_text()
}

#' Get Nodes in Grid Box
#'
#' Reads spatial point features from file filtered by a WKT geometry.
#'
#' @param directory Character. Path to spatial file containing point data.
#' @param gridBox Character. WKT geometry to spatially filter the data.
#'
#' @return `sf` object with POINT geometry or `NULL` if reading fails.
#' @export
getNodes <- function(directory, gridBox) {
  tryCatch({
    st_read(directory, wkt_filter = gridBox, quiet = TRUE) %>%
      st_cast("POINT", do_split = TRUE, warn = FALSE)
  }, error = function(e) NULL)
}

#' Get Edges in Grid Box
#'
#' Reads spatial LINESTRING features from a network file using buffered grid box as filter.
#'
#' @param network Character. Path to spatial file containing line data (network edges).
#' @param cityGrid `sf` object. The grid over the city area.
#' @param i Integer. Index of the grid cell to query.
#'
#' @return `sf` object with LINESTRING geometry or `NULL` if reading fails.
#' @export
getEdges <- function(network, cityGrid, i) {
  grid_box <- cityGrid[i, ] %>%
    st_buffer(100, nQuadSegs = 1) %>%
    st_geometry() %>%
    st_as_text()

  tryCatch({
    st_read(network, wkt_filter = grid_box, quiet = TRUE) %>%
      st_cast("LINESTRING", do_split = TRUE, warn = FALSE)
  }, error = function(e) NULL)
}

#' Merge Node Datasets from Building and General Sources
#'
#' Harmonizes and merges node datasets from different sources, handling missing data gracefully.
#'
#' @param nodesBuild `sf` object. Nodes representing building entrances (may contain `population`, `ID`).
#' @param nodesGS `sf` object. General service nodes (may contain `city_code`, `class`, `identifier`, `area`).
#' @param node_out Character. Path to output file (unused here but passed from upstream logic).
#'
#' @return `sf` object with combined node features, or `NULL` if both inputs are empty.
#' @export
mergeNodes <- function(nodesBuild, nodesGS, node_out) {
  if (!is.null(nodesBuild) && nrow(nodesBuild) == 0) nodesBuild <- NULL
  if (!is.null(nodesGS) && nrow(nodesGS) == 0) nodesGS <- NULL

  if (is.null(nodesBuild) && is.null(nodesGS)) return(NULL)

  if (is.null(nodesBuild)) {
    return(mutate(nodesGS, population = NA, ID = NA))
  }

  if (is.null(nodesGS)) {
    return(mutate(nodesBuild, city_code = NA, class = NA, identifier = NA, area = NA))
  }

  bind_rows(nodesBuild, nodesGS) %>%
    distinct()
}

#' Blend Network with Nodes
#'
#' Integrates spatial nodes into a given network using `sfnetworks`.
#'
#' @param net_tile `sf` object. LINESTRING network segment.
#' @param nodes `sf` object. Point data to blend into the network.
#'
#' @return A `sfnetwork` object with blended nodes.
#' @export
blend <- function(net_tile, nodes) {
  require(sfnetworks)

  as_sfnetwork(net_tile) %>%
    st_network_blend(nodes, tolerance = 1e-4)
}

#' Write Blended Network Output to Disk
#'
#' Writes the edges and nodes of a blended network to separate output files.
#'
#' @param net `sfnetwork` object. The network to write to disk.
#' @param edge_out Character. File path for output edge shapefile or GeoPackage.
#' @param node_out Character. File path for output node shapefile or GeoPackage.
#'
#' @return None. Side effects: writes files to disk.
#' @export
writeOutput <- function(net, edge_out, node_out) {
  net %>%
    activate("edges") %>%
    st_as_sf() %>%
    select(-matches("from|to")) %>%
    distinct() %>%
    write_sf(edge_out, append = FALSE)

  net %>%
    activate("nodes") %>%
    st_as_sf() %>%
    select(-matches("city_code|class")) %>%
    distinct() %>%
    write_sf(node_out, append = FALSE)
}
