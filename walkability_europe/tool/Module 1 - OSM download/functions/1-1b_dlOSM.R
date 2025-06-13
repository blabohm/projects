#' Download OSM Data by Grid for a City Boundary
#'
#' This function divides a city boundary into a grid and downloads OSM building and network
#' data tile-by-tile. Output is saved to `input_directory`.
#'
#' @param city_boundary An `sf` object representing the boundary of the city.
#' @param input_directory Character. Path to the base input/output directory for saving data.
#'
#' @return This function does not return a value. It writes files to disk.
#' @export
#'
#' @examples
#' \dontrun{
#' dlOSM(city_boundary, "data/input")
#' }

dlOSM <- function(city_boundary, input_directory) {
  # Generate city grid
  city_grid <- mkCityGrid(city_boundary)

  # Define output paths
  build_out <- file.path(input_directory, "osm_buildings")
  net_out <- file.path(input_directory, "osm_network")

  # Create directories if they don't exist
  if (!dir.exists(build_out)) dir.create(build_out)
  if (!dir.exists(net_out)) dir.create(net_out)

  # Iterate over grid tiles
  for (i in seq_len(nrow(city_grid))) {
    id <- city_grid$id[i]
    code <- city_boundary$URAU_CODE

    # Download and save building polygons
    polygons <- OSM_downloader(city_grid$geom[i], key = "building")
    if (!is.null(polygons)) {
      dsn_build <- file.path(build_out, paste0(code, "_", id, ".gpkg"))
      OSM_build_writer(polygons, dsn_build)
    }

    # Download and save highway lines
    lines <- OSM_downloader(city_grid$geom[i], key = "highway")
    if (!is.null(lines)) {
      dsn_net <- file.path(net_out, paste0(code, "_", id, ".gpkg"))
      OSM_network_writer(lines, dsn_net)
    }
  }
}
