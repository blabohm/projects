#' Identify Neighboring Cities Based on Proximity
#'
#' Returns a list of neighboring city codes within a 1000m buffer of a selected city.
#'
#' @param city_boundaries Character. Path to city boundaries shapefile or geopackage.
#' @param city_code Character. City code to filter.
#'
#' @return Character vector of neighboring city codes.
#' @export
proximity_checker1 <- function(city_boundaries, city_code) {
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)

  city_boundary <- boundaryLoader(
    city_boundaries = city_boundaries,
    city_code = city_code,
    buffer_dist = 1000
  )

  filter_wkt <- city_boundary %>%
    st_geometry() %>%
    st_as_text()

  st_read(city_boundaries, wkt_filter = filter_wkt, quiet = TRUE) %>%
    pull(FUA_CODE) %>%
    substr(1, 5) %>%
    unique()
}

#' Load and Filter Green Space Layers from Urban Atlas
#'
#' Extracts green spaces based on UA codes within a city boundary.
#'
#' @param code_list Character vector. UA tile codes to match.
#' @param ua_directory Character. Path to the root UA data directory.
#' @param city_boundaries Character. Path to city boundaries shapefile.
#' @param city_code Character. City code to filter.
#' @param output Character. Return mode: `"directory"` or `"sf"` object.
#' @param crs Numeric. Target CRS (default is EPSG:3035).
#'
#' @return File path to saved green spaces (if `output = "directory"`),
#' or an `sf` object (if `output = "sf"`).
#' @export
UAgreen_space <- function(code_list, ua_directory, city_boundaries, city_code,
                          output = "directory", crs = 3035) {
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)

  green_codes <- c(14100, 30000, 31000)
  code_pattern <- paste(code_list, collapse = "|")

  ua_files <- list.files(ua_directory, pattern = code_pattern, full.names = TRUE) %>%
    list.files(pattern = "\\.gpkg$", full.names = TRUE, recursive = TRUE)

  message("\nLoading green spaces...\n")

  out_layer <- "green_spaces"
  gs_dsn <- file.path(tempdir(), paste0(out_layer, ".gpkg"))

  city_boundary_wkt <- boundaryLoader(
    city_boundaries = city_boundaries,
    city_code = city_code,
    buffer_dist = 1000
  ) %>%
    st_geometry() %>%
    st_as_text()

  for (ua_file in ua_files) {
    layer_name <- st_layers(ua_file)$name[1]

    st_read(ua_file, layer = layer_name, wkt_filter = city_boundary_wkt, quiet = TRUE) %>%
      st_transform(crs) %>%
      select(
        city_code = tidyselect::matches("FUA_OR|fua_code"),
        class = tidyselect::matches("code.*20"),
        tidyselect::contains("identifier"),
        tidyselect::contains("area")
      ) %>%
      filter(class %in% green_codes | identifier == "81210-DE001L1") %>%
      st_write(dsn = gs_dsn, layer = out_layer, append = TRUE, quiet = TRUE)
  }

  if (grepl("dir", output, ignore.case = TRUE)) {
    return(gs_dsn)
  } else {
    result <- st_read(gs_dsn, quiet = TRUE)
    unlink(gs_dsn)
    return(result)
  }
}

#' Identify Entry Points to Green Spaces from Network
#'
#' Intersects a road network with green space outlines to find potential entry points.
#'
#' @param green_spaces `sf` POLYGON or MULTIPOLYGON object. Green spaces.
#' @param network Character or `sf` LINESTRING object. Path to or loaded street network.
#' @param crs Numeric. Target coordinate reference system (default EPSG:3035).
#'
#' @return `sf` POINT object representing access points to green spaces.
#' @export
findGSentries <- function(green_spaces, network, crs = 3035) {
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)

  if (is.character(network)) {
    network <- st_read(network, quiet = TRUE)
  }

  green_spaces <- st_cast(green_spaces, "POLYGON", do_split = TRUE, warn = FALSE)

  message("\nFinding green space entries...\n")

  gs_entries <- green_spaces %>%
    st_buffer(-5) %>%
    st_cast("MULTILINESTRING", do_split = TRUE, warn = FALSE) %>%
    st_cast("LINESTRING", do_split = TRUE, warn = FALSE) %>%
    st_intersection(network)

  buffer_size <- 0
  while (
    green_spaces %>%
    filter(!(identifier %in% unique(gs_entries$identifier))) %>%
    nrow() > 0
  ) {
    buffer_size <- if (buffer_size <= 50) {
      buffer_size + 5
    } else if (buffer_size <= 100) {
      buffer_size + 10
    } else if (buffer_size <= 200) {
      buffer_size + 25
    } else {
      buffer_size + 50
    }

    new_entries <- green_spaces %>%
      filter(!(identifier %in% unique(gs_entries$identifier))) %>%
      st_buffer(buffer_size) %>%
      st_cast("MULTILINESTRING", do_split = TRUE, warn = FALSE) %>%
      st_cast("LINESTRING", do_split = TRUE, warn = FALSE) %>%
      st_intersection(network)

    gs_entries <- bind_rows(gs_entries, new_entries)
  }

  gs_entries %>%
    st_cast("MULTIPOINT", do_split = TRUE, warn = FALSE) %>%
    st_cast("POINT", do_split = TRUE, warn = FALSE)
}

