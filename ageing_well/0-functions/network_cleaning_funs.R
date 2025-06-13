



networkCleaner <- function(network, crs = 3035)
{
  # load packages
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)
  require(tidygraph, quietly = TRUE)
  require(sfnetworks, quietly = TRUE)

  # filter out motorways and cast everything to lines
  #network_filt_cast <-
  network %>%
    filter(!grepl("motorway|secondary", highway)) %>%
    st_cast("MULTILINESTRING", do_split = TRUE, warn = FALSE) %>%
    st_cast("LINESTRING", do_split = TRUE, warn = FALSE) %>%
    # remove double entries
    distinct() %>%

  # give lines IDs and retrieve data
  #network_filt_cast$edge_id <- row_number(network_filt_cast)
  #network_data <- st_drop_geometry(network_filt_cast)

  # Round network geometries
  #network_geom_rounded <-
    #network_filt_cast %>%
    st_geometry() %>%
    # make sure point coordinates match
    lapply(function(x) round(x, 0)) %>%
    st_sfc(crs = crs) %>%

  #network_clean_1 <-
    # Further cleaning steps
    #network_filt_cast %>%
    #mutate(geom = network_geom_rounded) %>%
    as_sfnetwork() %>%
    # subdivide edges at interior points
    convert(to_spatial_subdivision) %>%
    # remove pseudo nodes
    convert(to_spatial_smooth) %>%
    # remove unconnected edges
    filter(group_components() == 1) %>%
  #network_clean_1 %>%
    activate("edges") %>%
    st_geometry() %>%
    st_as_sf() %>%
    rename(geom = x) %>%
    remove_overlap()
}


remove_overlap <- function(network, search_radius = 1e-5)
{
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)
  network <- distinct( select(network, geom) )
  eb <- st_buffer(network, search_radius)
  network$covers <- c(st_covers(eb, network))
  covering <- filter(network, lengths(covers) > 1)
  notCovering <- filter(network, lengths(covers) < 2)
  if (nrow(covering) < 1) return( select(network, geom) )
  st_difference(st_union(covering), st_union(notCovering)) %>%
    st_cast("MULTILINESTRING", do_split = TRUE, warn = FALSE) %>%
    st_cast("LINESTRING", do_split = TRUE, warn = FALSE) %>%
    st_sf() %>%
    rename(geom = geometry) %>%
    bind_rows(notCovering, .) %>%
    distinct() %>%
    select(geom) %>%
    return()
}