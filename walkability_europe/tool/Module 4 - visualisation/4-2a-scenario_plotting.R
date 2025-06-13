################################################################################
# MODULE 4 - SCENARIO PLOTTING 
# AUTHOR: BENJAMIN LABOHM, BERLIN, 2022
################################################################################


library(ggplot2)
library(sf)
library(dplyr)
library(RColorBrewer)
library(ggspatial)
library(cowplot)

# Define paths -------------------------------------------------------------

DRIVE <- "D:/"
github <- paste0(DRIVE, "MA/")
wd <- paste0(DRIVE, "output/DE008/")

edges <- paste0(wd, "edges.gpkg")
build_poly <- paste0(wd, "buildings.gpkg")
gs_dir <- paste0(wd, "DE008L2_LEIPZIG_UA2018_v012.gpkg")

# Read and prepare spatial data --------------------------------------------

id <- "23473-DE008L2"
lvp_query <- paste0(
  "SELECT area, geom FROM ", st_layers(gs_dir)$name[1],
  " WHERE identifier LIKE '", id, "'"
)
lvp <- read_sf(gs_dir, query = lvp_query)
lvp_label <- st_point_on_surface(lvp) %>%
  mutate(name = "Lene Voigt Park")

gs_query <- paste0(
  "SELECT area, geom, identifier FROM ", st_layers(gs_dir)$name[1],
  " WHERE code_2018 IN (14100, 31000)"
)

gs <- read_sf(gs_dir, query = gs_query)

ls_values <- read_sf(paste0(wd, "scenarios/ls_values.gpkg"))
di_values <- read_sf(paste0(wd, "scenarios/di_values.gpkg"))

# Bounding box and filters -------------------------------------------------

bbox <- ls_values %>% st_bbox()

xmin <- bbox["xmin"] + 1100
xmax <- bbox["xmax"] - 1100
ymin <- bbox["ymin"] + 1100
ymax <- bbox["ymax"] - 1100

bbox_filter <- bbox %>%
  st_as_sfc() %>%
  st_as_text()

# Utility function for fixing German umlauts -------------------------------

umlaute <- function(variable) {
  variable %>%
    gsub("Ã¼", "ü", .) %>%
    gsub("ÃŸ", "ß", .) %>%
    gsub("Ã¶r|Ã¶", "ö", .) %>%
    gsub("Ã¤", "ä", .)
}

# Street labels -------------------------------------------------------------

street_labs <- read_sf(paste0(wd, "lvp_osm.gpkg")) %>%
  st_transform(3035) %>%
  filter(!is.na(name), highway != "highway") %>%
  st_filter(st_as_sfc(bbox)) %>%
  mutate(name = umlaute(name)) %>%
  filter(name %in% c("Riebeckstraße", "Josephinenstraße")) %>%
  group_by(name) %>%
  summarise() %>%
  st_union(by_feature = TRUE) %>%
  st_point_on_surface() %>%
  bind_rows(lvp_label) %>%
  mutate(lab = LETTERS[1:nrow(.)])

# Read building and network data --------------------------------------------

build_sf <- read_sf(build_poly, wkt_filter = bbox_filter) %>%
  select(geom, population) %>%
  mutate(population = pmin(population, 50))

net_sf <- read_sf(edges, wkt_filter = bbox_filter) %>%
  select(geom)

# Base plot definition -----------------------------------------------------

base_plot <- ggplot() +
  geom_sf(data = st_buffer(st_as_sfc(bbox), 1000), fill = "gray93") +
  geom_sf(data = build_sf, fill = "gray99", color = "gray99") +
  geom_sf(data = lvp, fill = "darkolivegreen4", alpha = 0.2, color = NA) +
  geom_sf(data = net_sf, color = "gray80", size = 1) +
  geom_sf(data = gs, fill = "darkolivegreen1", alpha = 0.2, color = NA)

base_plot

# Leipzig overview plot -----------------------------------------------------

lpz <- paste0(wd, "cities.gpkg") %>%
  read_sf(query = "SELECT * FROM cities WHERE URAU_CODE = 'DE008'") %>%
  st_transform(3035)

lpz_box <- st_bbox(lpz)

ua <- paste0(wd, "DE008L2_LEIPZIG_UA2018_v013.gpkg")

water <- read_sf(
  ua,
  query = paste("SELECT * FROM", st_layers(ua)$name[1], "WHERE code_2018 = '50000'"),
  wkt_filter = st_as_text(lpz$geom)
)

overview_plot <- ggplot(lpz) +
  geom_sf(data = water, fill = "lightblue", color = "lightblue") +
  geom_sf(fill = NA) +
  geom_sf(data = st_point_on_surface(lvp), color = "red", size = 3) +
  coord_sf(
    xlim = c(lpz_box["xmin"], lpz_box["xmax"]),
    ylim = c(lpz_box["ymin"], lpz_box["ymax"])
  ) +
  geom_sf_text(data = lvp, aes(label = "Lene Voigt Park"), size = 3, nudge_y = 2000) +
  ggtitle("Leipzig") +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(color = "black"),
    plot.title.position = "panel"
  )

overview_plot

# LS plot -------------------------------------------------------------------

ls_plot <- base_plot +
  ls_values %>%
  filter(!is.na(ls)) %>%
  mutate(ls = log(ls)) %>%
  arrange(ls) %>%
  geom_sf(aes(color = ls), size = 2) +
  scale_color_distiller(palette = "RdBu") +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  labs(title = "Local Significance (LS)", color = "LS") +
  annotation_scale(style = "ticks") +
  geom_sf_label(data = street_labs, aes(label = lab), color = "gray10") +
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())

ls_plot

# DI plot -------------------------------------------------------------------

di_plot <- base_plot +
  di_values %>%
  filter(!is.na(di)) %>%
  geom_sf(aes(fill = di, color = di)) +
  scale_fill_distiller(palette = "RdYlBu", direction = 1, breaks = c(0, .2, .4, .6, .8)) +
  scale_color_distiller(palette = "RdYlBu", direction = 1, breaks = c(0, .2, .4, .6, .8)) +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  labs(title = "Detour Index (DI)", fill = "DI", color = "DI") +
  annotation_scale(style = "ticks") +
  annotation_custom(
    grob = ggplotGrob(overview_plot),
    xmin = xmin - 100,
    xmax = xmin + ((ymax - ymin) / 3) * 0.88 - 100,
    ymin = ymin,
    ymax = ymin + (ymax - ymin) / 3
  ) +
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())

di_plot

# Save LS and DI plots ------------------------------------------------------

plot_grid(ls_plot, di_plot, nrow = 2) %>%
  ggsave(
    filename = paste0(github, "/plots/3-1_ls_di_plot.pdf"),
    plot = .,
    width = 8.27, height = 11.69
  )

# Helper function: logarithmic transformation with sign --------------------

mk_log <- function(numbers) {
  tibble(numbers) %>%
    mutate(num_log = log(abs(numbers))) %>%
    mutate(num_log = case_when(
      numbers == 0 ~ 0,
      numbers > 0 ~ num_log,
      numbers < 0 ~ -num_log,
      TRUE ~ num_log
    )) %>%
    pull(num_log)
}

# More scenario plots -------------------------------------------------------

# (Here you can add your alternative plots — ls1_plot, di1_plot, ls2_plot, etc. — formatted similarly)

# Example for Alternative 1: Unlimited Access (LS)

ls1_plot <- base_plot +
  geom_sf(data = build_sf, aes(fill = population)) +
  scale_fill_distiller(palette = "Blues") +
  ls_values %>%
  filter(!is.na(d_ls1), d_ls1 != 0) %>%
  arrange(d_ls1) %>%
  mutate(d_ls1 = mk_log(d_ls1)) %>%
  geom_sf(aes(color = d_ls1), size = 2) +
  scale_color_distiller(palette = "RdBu") +
  coord_sf(xlim = c(xmin + 650, xmax - 400), ylim = c(ymin + 400, ymax - 300)) +
  labs(title = "a) Alternative 1: Unlimited Access (LS)", color = expression(Delta ~ "LS")) +
  annotation_scale(style = "ticks") +
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())

# Similar approach for other plots...

# Save plots ----------------------------------------------------------------

ggsave(filename = paste0(github, "/plots/3-1a_ls.pdf"), plot = ls_plot, width = 11.69, height = 8.27)
ggsave(filename = paste0(github, "/plots/3-1b_di.pdf"), plot = di_plot, width = 11.69, height = 8.27)
ggsave(filename = paste0(github, "/plots/6-2a_ls1.pdf"), plot = ls1_plot, width = 
