################################################################################
# MODULE 4 - SCENARIO PLOTTING (REVISED)
# AUTHOR: BENJAMIN LABOHM, BERLIN, 2022
################################################################################

# PACKAGES ---------------------------------------------------------------------
library(dplyr)
library(sf)
library(sfnetworks)
library(ggplot2)
library(tidygraph)
library(stringr)

# SOURCE FUNCTIONS -------------------------------------------------------------
functions_path <- file.path(getwd(), "tool", "Module 3 - index building", "functions")
list.files(functions_path, pattern = "3.*\\.R", full.names = TRUE) %>%
  purrr::walk(source)

# DIRECTORIES AND INPUTS -------------------------------------------------------
wd <- "C:/Users/labohben/Desktop/DE008/"
nodes_path <- file.path(wd, "nodes.gpkg")
edges_path <- file.path(wd, "edges.gpkg")
lvp_outline_path <- file.path(wd, "lvp_outline.gpkg")
ua2018_v012_path <- file.path(wd, "DE008L2_LEIPZIG_UA2018_v012.gpkg")
ua2018_v013_path <- file.path(wd, "DE008L2_LEIPZIG_UA2018_v013.gpkg")
buildings_path <- file.path(wd, "buildings.gpkg")

id <- "23473-DE008L2"
buffer_distance <- 2000
out_dir <- file.path(wd, "scenarios")
dir.create(out_dir, showWarnings = FALSE)

# BUFFER CREATION --------------------------------------------------------------
get_buffer_wkt <- function(nodes_path, identifier, buffer_dist) {
  query <- sprintf("SELECT * FROM nodes WHERE identifier IS '%s'", identifier)
  entries <- read_sf(nodes_path, query = query)
  st_buffer(entries, buffer_dist) %>%
    st_union() %>%
    st_geometry() %>%
    st_as_text()
}

wkt_filter <- get_buffer_wkt(nodes_path, id, buffer_distance)

# PARK ENTRIES -----------------------------------------------------------------
gs_layer <- st_layers(ua2018_v012_path)$name[1]
gs_query <- sprintf("SELECT area, geom FROM %s WHERE identifier LIKE '%s'", gs_layer, id)
gs <- read_sf(ua2018_v012_path, query = gs_query)

# SYNTHETIC GREEN SPACE POINTS (2M SAMPLING) -----------------------------------
lvp_outline <- read_sf(lvp_outline_path) %>%
  st_union() %>%
  st_cast("MULTILINESTRING") %>%
  st_as_sf()

sample_n <- round(as.numeric(st_length(lvp_outline) / 5))

new_gse <- lvp_outline %>%
  st_sample(size = sample_n, type = "regular") %>%
  st_cast("POINT") %>%
  st_as_sf() %>%
  mutate(
    area = gs$area,
    identifier = id,
    ID = NA,
    population = NA,
    geom = geometry
  ) %>%
  select(-geometry)

# BLEND GREEN SPACE ENTRIES TO NETWORK -----------------------------------------
net <- read_sf(edges_path, wkt_filter = wkt_filter) %>%
  as_sfnetwork() %>%
  convert(to_undirected) %>%
  st_network_blend(new_gse) %>%
  activate("edges") %>%
  st_as_sf()

edges_new <- file.path(out_dir, "edges.gpkg")
nodes_new <- file.path(out_dir, "nodes.gpkg")

net %>%
  mutate(edge_id = row_number()) %>%
  select(edge_id) %>%
  write_sf(edges_new)

read_sf(nodes_path, wkt_filter = wkt_filter) %>%
  write_sf(nodes_new)

# BASE INDICES -----------------------------------------------------------------
index_dir <- file.path(out_dir, "indices")
dir.create(index_dir)

gs_ids <- read_sf(nodes_new, wkt_filter = wkt_filter) %>%
  filter(!is.na(identifier)) %>%
  pull(identifier) %>%
  unique()

calcIndices(green_space_IDs = gs_ids, in_directory = out_dir, out_directory = index_dir)

gatherDI(building_polygons = buildings_path, index_dir = index_dir, output_dir = file.path(out_dir, "di.gpkg"))
gatherLS(edges = edges_path, index_dir = index_dir, output_dir = file.path(out_dir, "ls.gpkg"))

################################################################################
# SCENARIO 1: UNLIMITED ACCESS -------------------------------------------------
index_dir1 <- file.path(out_dir, "indices1")
dir.create(index_dir1)

net <- read_sf(edges_new)
be1 <- read_sf(nodes_new) %>% filter(population > 0)

out1 <- add_params(build_entries = be1, gs_entries = new_gse, network = net)

file.copy(
  from = list.files(index_dir, pattern = paste(gs_ids[!grepl(id, gs_ids)], collapse = "|"), full.names = TRUE),
  to = index_dir1
)

write_output(out1, network = net, out_dir = index_dir1, ID = id)
gatherDI(building_polygons = buildings_path, index_dir = index_dir1, output_dir = file.path(out_dir, "di1.gpkg"))
gatherLS(edges = edges_new, index_dir = index_dir1, output_dir = file.path(out_dir, "ls1.gpkg"))

################################################################################
# SCENARIO 2: GREEN SPACE DEVELOPMENT ------------------------------------------
index_dir2 <- file.path(out_dir, "indices2")
dir.create(index_dir2)

target_ids <- c("23502-DE008L2", "23493-DE008L2", "23485-DE008L2", "23508-DE008L2", "23509-DE008L2")
target_query <- paste0("SELECT * FROM nodes WHERE ", paste(sprintf("identifier = '%s'", target_ids), collapse = " OR "))
target_gs <- read_sf(nodes_new, query = target_query)

ua_layer <- st_layers(ua2018_v013_path)$name[1]
pop_query <- sprintf("SELECT Pop2018, area FROM %s WHERE code_2018 = 11100", ua_layer)

hd_pop <- read_sf(ua2018_v013_path, query = pop_query, wkt_filter = wkt_filter) %>%
  mutate(pop_per_m = Pop2018 / area)

pop_per_m_95 <- quantile(hd_pop$pop_per_m, 0.95)

new_building_entries <- target_gs %>%
  group_by(identifier) %>%
  mutate(
    population = round(area * pop_per_m_95 / n()),
    ID = identifier,
    area = NA,
    identifier = NA
  )

net <- read_sf(edges_new)
be2 <- read_sf(nodes_new) %>% filter(population > 0) %>% bind_rows(new_building_entries)

gs_ids2 <- setdiff(gs_ids, target_ids)

for (i in gs_ids2) {
  gse <- read_sf(nodes_new, query = sprintf("SELECT * FROM nodes WHERE identifier IS '%s'", i))
  out <- add_params(build_entries = be2, gs_entries = gse, network = net)
  write_output(out, network = net, out_dir = index_dir2, ID = i)
}

gatherDI(building_polygons = buildings_path, index_dir = index_dir2, output_dir = file.path(out_dir, "di2.gpkg"))
gatherLS(edges = edges_new, index_dir = index_dir2, output_dir = file.path(out_dir, "ls2.gpkg"))

################################################################################
# SCENARIO 3: POPULATION INCREASE ----------------------------------------------
index_dir3 <- file.path(out_dir, "indices3")
dir.create(index_dir3)

ua_pop <- read_sf(ua2018_v013_path, layer = ua_layer, wkt_filter = wkt_filter) %>%
  filter(grepl("^11", code_2018)) %>%
  st_drop_geometry()

hd_pop <- ua_pop %>%
  mutate(pop_per_m = Pop2018 / area) %>%
  group_by(code_2018) %>%
  summarise(pop_high = quantile(pop_per_m, 0.95))

be3 <- read_sf(nodes_new) %>%
  filter(population > 0) %>%
  select(-area) %>%
  mutate(identifier = str_extract(ID, ".*L2")) %>%
  left_join(ua_pop, by = "identifier") %>%
  left_join(hd_pop, by = "code_2018") %>%
  mutate(
    Pop2018_new = area * pop_high,
    population_new = round(population / Pop2018 * Pop2018_new),
    population_new = ifelse(population_new < population, round(population * 1.2), population_new)
  ) %>%
  transmute(identifier = NA, area = NA, population = population_new, ID, geom)

write_sf(be3, file.path(wd, "scen3_be.gpkg"))

for (i in gs_ids) {
  gse <- read_sf(nodes_new, query = sprintf("SELECT * FROM nodes WHERE identifier IS '%s'", i))
  out <- add_params(build_entries = be3, gs_entries = gse, network = net)
  write_output(out, network = net, out_dir = index_dir3, ID = i)
}

gatherDI(building_polygons = buildings_path, index_dir = index_dir3, output_dir = file.path(out_dir, "di3.gpkg"))
gatherLS(edges = edges_new, index_dir = index_dir3, output_dir = file.path(out_dir, "ls3.gpkg"))

################################################################################
# SCENARIO 4: POP-INCREASE + GREEN SPACE DEV ----------------------------------
index_dir4 <- file.path(out_dir, "indices4")
dir.create(index_dir4)

be4 <- bind_rows(be3, new_building_entries)

gse4 <- read_sf(nodes_new, query = "SELECT * FROM nodes WHERE identifier IS NOT NULL") %>%
  filter(!(identifier %in% c(id, target_ids)), area > 0) %>%
  bind_rows(new_gse)

gs_ids4 <- gs_ids2

for (i in gs_ids4) {
  gse <- filter(gse4, identifier == i)
  out <- add_params(build_entries = be4, gs_entries = gse, network = net)
  write_output(out, network = net, out_dir = index_dir4, ID = i)
}

gatherDI(building_polygons = buildings_path, index_dir = index_dir4, output_dir = file.path(out_dir, "di4.gpkg"))
gatherLS(edges = edges_new, index_dir = index_dir4, output_dir = file.path(out_dir, "ls4.gpkg"))
