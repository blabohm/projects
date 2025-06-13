# Idea:
# Start with max(d) (+security buffer?):
#   Create the clean sfnetwork
#   Add raster data / LS to edge data (use the function you already created)
#   Save network + edge data / buildings / pois
# Do shortest path workflow from building_entries to POIs / park entries (merge destination datasets and give park entry etc. a Dimension attribute)
#
# Extract Distance + raster variables per Dimension category
clean_dir_create(output_folder)
lor_sf <- read_sf(input_files[grepl("lor", input_files)])

distances <- c(
  100, 200, 300, 400, 500, 600, 700, 800, 900, 1000
)

max_d <- max(distances) + 100

# j <- 1
for(j in 1:nrow(lor_sf)) {

  buildings <- vect_input_wkt(input_files[grepl("build", input_files)],
                              st_as_text(lor_sf$geom[j,]))

  # buffer LOR by distance (d) and load POIS / network in buffered area
  lor_buff <- st_as_text(st_buffer(lor_sf$geom[j,], max_d))
  osm_pois <- vect_input_wkt(input_files[grepl("poi", input_files)],
                             lor_buff)
  osm_network <- vect_input_wkt(input_files[grepl("network", input_files)],
                                lor_buff)

  # join raster data to network
  edge_greenness <- rast_input(input_files[grepl("tcd", input_files)],
                               osm_network) %>%
    select(greenness = Class_Name) %>%
    st_drop_geometry()

  # load local significance data
  local_significance <-
    vect_input_wkt(input_files[grepl("local", input_files)],
                   lor_buff) %>%
    st_buffer(1)

  # convert osm network into sf network object
  osm_sf_network <- osm_network %>%
    bind_cols(edge_greenness) %>%
    st_join(local_significance) %>%
    make_sf_network()

  # Find building and POI entry points and blend them into network
  # Nearest point of building polygon on network
  nearest_id <- st_nearest_feature(buildings, osm_network)
  np_build_network <-
    st_nearest_points(buildings,
                      osm_network$geom[nearest_id],
                      pairwise = TRUE) %>%
    st_cast("POINT") %>%
    st_as_sf() %>%
    rename(geom = x) %>%
    slice(seq(2, nrow(.), 2)) %>%
    bind_cols(st_drop_geometry(buildings))

  # building entries
  # building_entries <- np_build_network %>%
  #   blender(osm_sf_network, .) %>%
  #   activate("nodes") %>%
  #   st_as_sf() %>%
  #   filter(!is.na(address))

  # POI entries
  poi_entries <- osm_pois %>%
    blender(osm_sf_network, .) %>%
    activate("nodes") %>%
    st_as_sf() %>%
    filter(!is.na(Dimension))

  # Create edge info df with length, ls, greenness
  edge_info_df <- osm_sf_network %>%
    activate("edges") %>%
    st_as_sf()

  # create OD cost matrix between buildings and POIs for nearest network distance
  net_cost <- st_network_cost(osm_sf_network, np_build_network, poi_entries)

  # d <- 100
  for (d in distances) {
    print(d)

    # Names and IDs of POIs that are in distance of respective buildings
    in_dist <- apply(net_cost, 1,
                     function(x) poi_entries$Dimension[which(x <= d)])
    in_dist_id <- apply(net_cost, 1, function(x) which(x <= d))

    # Routing between buildings and POIs
    edge_paths <- np_build_network[1,] %>%
      st_network_paths(x = osm_sf_network, from = .,
                       to = poi_entries$geom[in_dist_id[[1]]]) %>%
      pull(edge_paths)

    tmp_out <- tibble(Dimension = in_dist[[1]], edge_paths) %>%
      mutate(net_dist = sapply(edge_paths,
                               function(x) sum(edge_info_df$weight[x],
                                               na.rm = TRUE)),
             local_significance = sapply(edge_paths,
                                         function(x) sum(edge_info_df$ls[x],
                                                         na.rm = TRUE)),
             greenness = sapply(edge_paths,
                                function(x) sum(edge_info_df$greenness[x],
                                                na.rm = TRUE))) %>%
      group_by(Dimension) %>%
      summarise(avg_net_dist = mean(net_dist, na.rm = TRUE),
                min_net_dist = min(net_dist, na.rm = TRUE),
                n = n(),
                avg_greenness = mean(greenness, na.rm = TRUE),
                avg_local_significance = mean(local_significance, na.rm = TRUE),
                max_local_significance = max(local_significance, na.rm = TRUE)) %>%
      mutate(address = np_build_network$address[1])

    for(i in 2:nrow(np_build_network)){
      edge_paths <- np_build_network[i,] %>%
        st_network_paths(x = osm_sf_network, from = .,
                         to = poi_entries$geom[in_dist_id[[i]]]) %>%
        pull(edge_paths)
      if (length(edge_paths) == 0) next
      tmp_out <- tibble(Dimension = in_dist[[i]], edge_paths) %>%
        mutate(net_dist = sapply(edge_paths,
                                 function(x) sum(edge_info_df$weight[x],
                                                 na.rm = TRUE)),
               local_significance = sapply(edge_paths,
                                           function(x) sum(edge_info_df$ls[x],
                                                           na.rm = TRUE)),
               greenness = sapply(edge_paths,
                                  function(x) sum(edge_info_df$greenness[x],
                                                  na.rm = TRUE))) %>%
        group_by(Dimension) %>%
        summarise(avg_net_dist = mean(net_dist, na.rm = TRUE),
                  min_net_dist = min(net_dist, na.rm = TRUE),
                  n = n(),
                  avg_greenness = mean(greenness, na.rm = TRUE),
                  avg_local_significance = mean(local_significance, na.rm = TRUE),
                  max_local_significance = max(local_significance, na.rm = TRUE)) %>%
        mutate(address = np_build_network$address[i]) %>%
        bind_rows(tmp_out, .)
    }

    tidyr::pivot_wider(tmp_out,
                       id_cols = address,
                       names_from = Dimension,
                       values_from = -c(Dimension, address)) %>%
      right_join(buildings) %>%
      write_sf(paste0(output_folder,
                      "/neighborhood_data_",
                      lor_sf$PLR_NAME[j],
                      "_", d, "m",
                      ".gpkg"),
               append = FALSE)
  }
}
