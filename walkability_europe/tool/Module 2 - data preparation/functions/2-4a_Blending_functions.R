#' Get Nearby Functional Urban Areas (FUA) by Proximity
#'
#' Identifies neighboring FUAs based on a buffered boundary of a target city.
#'
#' @param city_boundaries Path to a spatial file (e.g., `.shp`, `.gpkg`) containing city boundaries.
#' @param city_code Character. The code for the target city (e.g., `"DE001"`).
#'
#' @return A character vector of unique 5-digit FUA codes intersecting the target city.
#' @export
proximity_checker1 <- function(city_boundaries, city_code) {
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)

  # Load target city boundary
  city_boundary <- boundaryLoader(
    city_boundaries = city_boundaries,
    city_code = city_code,
    buffer_dist = 1000
  )

  # Create WKT geometry filter for spatial subsetting
  wkt_filter <- city_boundary %>%
    st_geometry() %>%
    st_as_text()

  # Identify neighboring FUAs by spatial intersection
  st_read(city_boundaries, wkt_filter = wkt_filter, quiet = TRUE) %>%
    pull(FUA_CODE) %>%
    substr(1, 5) %>%
    unique()
}

#' Extract Urban Atlas Green Spaces for Target FUA Region
#'
#' Loads and filters Urban Atlas green spaces intersecting a city boundary.
#'
#' @param code_list Character vector of city/FUA codes to filter Urban Atlas files.
#' @param ua_directory Path to Urban Atlas directory containing `.gpkg` files.
#' @param city_boundaries Path to spatial boundary file (e.g., `.shp`).
#' @param city_code Character. City identifier (e.g., `"DE001"`).
#' @param output Character. `"directory"` returns path to temp GPKG; `"sf"` returns `sf` object. Default is `"directory"`.
#' @param crs Numeric. Coordinate Reference System (default is 3035).
#'
#' @return Either a file path or an `sf` object with green space geometries.
#' @export
UAgreen_space <- function(code_list, ua_directory, city_boundaries, city_code,
                          output = "directory", crs = 3035) {
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)

  green_classes <- c(14100, 30000, 31000)  # Urban Atlas codes for green areas

  # Build file list from codes
  pattern <- paste(code_list, collapse = "|")
  ua_files <- list.files(ua_directory, pattern = pattern, full.names = TRUE, recursive = TRUE)
  ua_files <- ua_files[grepl("\\.gpkg$", ua_files)]

  message("\nLoading green spaces...\n")

  out_layer <- "green_spaces"
  output_path <- file.path(tempdir(), paste0(out_layer, ".gpkg"))

  # Create WKT geometry filter
  city_geom <- boundaryLoader(city_boundaries, city_code, 1000) %>%
    st_geometry() %>%
    st_as_text()

  # Extract green areas from each UA file
  for (ua_file in ua_files) {
    lyr <- st_layers(ua_file)$name[1]

    st_read(ua_file, layer = lyr, wkt_filter = city_geom, quiet = TRUE) %>%
      st_transform(crs) %>%
      select(
        city_code = matches("FUA_OR|fua_code", ignore.case = TRUE),
        class = matches("code.*20"),
        identifier = matches("identifier"),
        area = matches("area")
      ) %>%
      filter(class %in% green_classes | identifier %in% "81210-DE001L1") %>%
      st_write(dsn = output_path, layer = out_layer, append = TRUE, quiet = TRUE)
  }

  # Return as path or sf object
  if (grepl("dir", output, ignore.case = TRUE)) {
    return(output_path)
  } else {
    green_sf <- st_read(output_path, quiet = TRUE)
    unlink(output_path)
    return(green_sf)
  }
}

#' Identify Entry Points from Network into Green Spaces
#'
#' Intersects transportation networks with green spaces to find entry points.
#'
#' @param green_spaces `sf` POLYGON object of green spaces.
#' @param network Path to a cleaned network `.gpkg` file or `sf` LINESTRING object.
#' @param crs Numeric. Coordinate reference system (default = 3035).
#'
#' @return An `sf` POINT object representing entry points into green spaces.
#' @export
findGSentries <- function(green_spaces, network, crs = 3035) {
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)

  # Read network if path is provided
  if (is.character(network)) {
    network <- st_read(network, quiet = TRUE)
  }

  green_spaces <- st_cast(green_spaces, "POLYGON", do_split = TRUE, warn = FALSE)

  message("\nFinding green space entries...\n")

  # Initial intersection
  gs_entries <- green_spaces %>%
    st_buffer(-5) %>%
    st_cast("MULTILINESTRING", do_split = TRUE, warn = FALSE) %>%
    st_cast("LINESTRING", do_split = TRUE, warn = FALSE) %>%
    st_intersection(network)

  # Buffer and iterate until all green_spaces have an entry
  b <- 0
  while (nrow(filter(green_spaces, !(identifier %in% unique(gs_entries$identifier)))) > 0) {
    remaining <- filter(green_spaces, !(identifier %in% unique(gs_entries$identifier)))

    new_entries <- remaining %>%
      st_buffer(b) %>%
      st_cast("MULTILINESTRING", do_split = TRUE, warn = FALSE) %>%
      st_cast("LINESTRING", do_split = TRUE, warn = FALSE) %>%
      st_intersection(network)

    gs_entries <- bind_rows(gs_entries, new_entries)

    # Buffer size logic
    if (b <= 50) {
      b <- b + 5
    } else if (b <= 100) {
      b <- b + 10
    } else if (b <= 200) {
      b <- b + 25
    } else {
      b <- b + 50
    }
  }

  # Final conversion to entry points
  gs_entries %>%
    st_cast("MULTIPOINT", do_split = TRUE, warn = FALSE) %>%
    st_cast("POINT", do_split = TRUE, warn = FALSE)
}
