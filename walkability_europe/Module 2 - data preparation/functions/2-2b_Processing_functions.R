#' Check Proximity of OSM Tiles to City Boundary
#'
#' Filters OSM tile files that intersect with a given city boundary.
#'
#' @param city_boundary `sf` object. Boundary geometry of the city.
#' @param osm_file Character vector. File paths to OSM tiles (e.g., `.gpkg` or `.shp`).
#' @param city_code Character. City identifier (not used internally but may be logged).
#' @param crs Numeric. Coordinate reference system (default is 3035).
#'
#' @return A tibble containing the file paths of tiles intersecting the city boundary.
#' @export
proximity_checker <- function(city_boundary, osm_file, city_code, crs = 3035) {
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)

  message("Checking proximity...")

  tmp_df <- tibble()

  for (i in osm_file) {
    layer_info <- st_layers(i)
    lyr_index <- which.max(layer_info$features)
    lyr_name <- layer_info$name[lyr_index]

    tile_geom <- st_read(i, layer = lyr_name, quiet = TRUE) %>%
      st_transform(crs) %>%
      sfc2bb(crs = crs)

    if (any(st_intersects(tile_geom$geom, city_boundary, sparse = FALSE))) {
      tmp_df <- bind_rows(tmp_df, tibble(tile_dir = i))
    }
  }

  return(tmp_df)
}

#' Filter OSM Buildings Inside UA Residential Areas
#'
#' Removes non-residential OSM buildings and retains those within Urban Atlas residential polygons.
#'
#' @param osm_buildings `sf` object. OSM buildings as polygons.
#' @param ua_residential `sf` object. UA residential zones as polygons.
#'
#' @return A filtered `sf` object with selected attributes (`Pop2018`, `identifier`, `code_2018`).
#' @export
OSMfilter <- function(osm_buildings, ua_residential) {
  require(dplyr)
  require(sf)

  not_res <- c(
    "^mall$", "train_station", "garages", "hospital", "parking",
    "sports_centre", "university", "gas_station", "school", "hall",
    "government", "prison", "sports_hall", "carport", "garbage", "waste"
  )

  message("\nFiltering OSM buildings...\n")

  osm_buildings %>%
    st_join(ua_residential) %>%
    filter(!is.na(code_2018)) %>%
    filter(!(building %in% not_res)) %>%
    select(Pop2018, identifier, code_2018)
}

#' Estimate Population for OSM Buildings
#'
#' Calculates population estimates for each building based on area and Urban Atlas population data.
#'
#' @param osm_buildings `sf` object. Filtered buildings with `Pop2018`, `identifier`, and `code_2018`.
#'
#' @return An `sf` object with estimated population and identifiers.
#' @export
OSMpop <- function(osm_buildings) {
  require(dplyr)
  require(sf)

  message("\nCalculating population per building...\n")

  osm <- mutate(osm_buildings, area = st_area(geom))

  area_df <- osm %>%
    st_drop_geometry() %>%
    group_by(identifier) %>%
    summarise(area_sum = sum(area), .groups = "drop")

  pop_sf <- osm %>%
    merge(area_df) %>%
    mutate(population = round(area / area_sum * Pop2018)) %>%
    select(population, area, area_sum, Pop2018, identifier, code_2018)

  pop_df <- pop_sf %>%
    na.omit() %>%
    st_drop_geometry() %>%
    mutate(pop_per_sqm = population / area) %>%
    group_by(code_2018) %>%
    summarise(pop_per_sqm = mean(pop_per_sqm), .groups = "drop")

  pop_sf %>%
    merge(pop_df, all.x = TRUE) %>%
    mutate(
      pop_by_area = round(area * pop_per_sqm),
      population = as.double(population),
      pop_by_area = as.double(pop_by_area),
      population = ifelse(population == 0 & pop_by_area > population, pop_by_area, population)
    ) %>%
    select(identifier, population) %>%
    filter(population > 0)
}

#' Add Unique Building ID
#'
#' Generates unique IDs for each building based on their UA identifier.
#'
#' @param osm_buildings `sf` object. Buildings with `identifier` column.
#'
#' @return An `sf` object with a new column `ID`, and `identifier` removed.
#' @export
OSMbuildID <- function(osm_buildings) {
  require(dplyr)

  osm_buildings %>%
    group_by(identifier) %>%
    mutate(ID = paste0(identifier, row_number())) %>%
    ungroup() %>%
    select(-identifier)
}
