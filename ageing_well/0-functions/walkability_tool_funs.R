# tool
safe_load <- function(package_list, library_dir)
{
  # Try to unload all packages in package list (eternity loop?)
  pkg_df <- data.frame("package" = package_list,
                       "unloaded" = FALSE)

  while (any(pkg_df$unloaded) == FALSE) {
    for (pkg in pkg_df$package[!pkg_df$unloaded]) {
      try({
        unloadNamespace(pkg)
        pkg_df$unloaded[pkg_df$package == pkg] <- TRUE
      })
    }
  }

  # Try to load packages from package list
  for (pkg in package_list) {
      loading_succeeded <- FALSE
      try({
      require(pkg, character.only = TRUE, lib.loc = library_dir)
      loading_succeeded <- TRUE
    })
    # install if require didnt work
    if (!loading_succeeded) {
      install.packages(pkg, lib = library_dir)
    }
  }
}

clean_dir_create <- \(directory) if (!dir.exists(directory)) dir.create(directory)

vect_input_wkt <- \(file_name, loc_filter) read_sf(file_name,
                                                   wkt_filter = loc_filter)
vect_input_sql <- function(file_name,
                           optional_statement = NULL) {
  read_sf(file_name,
          query = paste("SELECT * FROM",
                        st_layers(file_name)$name[1],
                        optional_statement))
}


blender <- function(net_tile, nodes)
{
  require(sfnetworks)
  require(tidygraph)
  net_tile  %>%
    activate("nodes") %>%
    sfnetworks::st_network_blend(nodes, tolerance = 1e5)
}


make_sf_network <- function(network)
{
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)
  require(sfnetworks, quietly = TRUE)
  require(tidygraph, quietly = TRUE)
  network %>%
    mutate(weight = st_length(.)) %>%
    as_sfnetwork() %>%
    activate("nodes") %>%
    filter(group_components() == 1) %>%
    activate("edges") %>%
    arrange(edge_length()) %>%
    filter(!edge_is_multiple()) %>%
    convert(to_spatial_subdivision) %>%
    convert(to_spatial_smooth) %>%
    convert(to_undirected) %>%
    mutate(weight = edge_length())
}



rast_input <- function(file_name, edges)
{
  #file_name <- raster_input
  #edges <- osm_network
  rast_raw <- rast(file_name)
  edges_trans <- st_transform(edges, st_crs(rast_raw)) %>%
    mutate(ID = row_number())
  rast_crop <- crop(rast_raw, edges_trans)
  rast_mask <- mask(rast_crop, edges_trans)
  rast_extr <- terra::extract(rast_crop,
                              vect(st_buffer(edges_trans, 2)),
                              touches = TRUE,
                              #bind = TRUE,
                              fun = \(x) mean(x, na.rm = TRUE)) %>%
    left_join(edges_trans, .)
  rast_extr

}


### end


