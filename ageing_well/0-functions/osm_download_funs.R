####
# Funs for downloading OSM data from polygon


clean_dir_create <- function(directory_name)
{
  if (dir.exists(directory_name)) {
    unlink(directory_name, recursive = TRUE)
  }
  dir.create(directory_name)
}

# Openstreetmap downloader
# testing code
# test_address <- "Rudower-Chaussee 16, Berlin"
# locs <- tibble(address = test_address) %>%
#   geocode(address = address) %>%
#   st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
#   st_transform(3035)
# d <- 300
# loc <- st_buffer(locs$geometry[1], d)
# key <- "highway"
# key = "building"
# key = "healthcare"
OSM_downloader <- function(loc, key, d)
{
  internet_access <- tryCatch(!curl::has_internet(), error = \(e) FALSE)
  if (!internet_access) assign("has_internet_via_proxy", TRUE,
                               environment(curl::has_internet))
  require(sf, quietly = TRUE)
  require(osmdata, quietly = TRUE)
  set_overpass_url(APIselect())
  ntry <- 0
  tmp <- NULL
  while (is.null(tmp) & ntry < 5) {
    try({
      if (ntry > 0) set_overpass_url(APIselect())
      ntry <- ntry + 1
      tmp <- loc %>%
        st_transform(4326) %>%
        st_buffer(d) %>%
        st_bbox() %>%
        opq() %>%
        add_osm_feature(key = key) %>%
        osmdata_sf()
    })
  }
  if (!exists("tmp")) return(NULL)
  return(tmp)
}


# Get residential polygons from urban atlas file
UAresLoader <- function(ua_path, boundary,
                        crs = 3035, 
                        res_class = c(11100, 11210, 11220,
                                      11230, 11240, 11300))
{
  # required packages
  require(dplyr)
  require(sf)
  
  # get name of UA land-use layer
  lr <- st_layers(ua_path)$name[1]
  boundary <- boundary %>%
    st_transform(crs = crs) %>% 
    st_geometry() %>%
    st_as_text()
  # load UA file and filter residential classes
  ua_path %>%
    st_read(lr, wkt_filter = boundary, quiet = TRUE) %>%
    filter(code_2018 %in% res_class) %>%
    st_transform(crs) %>%
    select(Pop2018, identifier, code_2018) %>%
    return()
}


# Select OSM download API that has not timed us out yet
APIselect <- function(
    api_list = dplyr::tibble(interpreter =
                               c('http://overpass-api.de/api/interpreter',
                                 'https://lz4.overpass-api.de/api/interpreter',
                                 'https://z.overpass-api.de/api/interpreter')))
{
  require(dplyr, quietly = TRUE)
  #N <- nrow(api_list)
  api_list <- api_list %>%
    mutate(slots = sapply(interpreter, getFreeSlots)) %>%
    arrange(desc(slots))

  while (sum(api_list$slots, na.rm = T) < 1) {
    message("Waiting for API slot.")
    Sys.sleep(30)
    api_list <- api_list %>%
      mutate(slots = sapply(interpreter, getFreeSlots)) %>%
      arrange(desc(slots))
  }
  return(api_list$interpreter[1])
}

# Get number of free slot for target OSM API
getFreeSlots <- function(APIlink)
{
  try({
    suppressWarnings({
      nSlots <- APIlink %>%
        sub('interpreter', 'status', .) %>%
        httr::GET() %>%
        httr::content() %>%
        strsplit("\n") %>%
        unlist() %>%
        grep("available", ., value = TRUE) %>%
        substr(1,1) %>%
        as.numeric() %>%
        .[1]})
  }, silent = TRUE)
  if (exists("nSlots")) {if (!is.na(nSlots)) return(nSlots) else return(0)
  } else return(0)
}

