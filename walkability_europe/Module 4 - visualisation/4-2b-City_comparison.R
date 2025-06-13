# Module 4: Scenario plotting --------------------------------------------------

library(ggplot2)
library(sf)
library(dplyr)
library(RColorBrewer)
library(ggspatial)
library(cowplot)

# Paths and IDs ----------------------------------------------------------------

DRIVE <- "D:/"
github <- paste0(DRIVE, "MA/")
wd <- paste0(DRIVE, "MA/DE008/")

edges <- paste0(wd, "edges.gpkg")
build_poly <- paste0(wd, "buildings.gpkg")
gs_dir <- paste0(wd, "DE008L2_LEIPZIG_UA2018_v012.gpkg")
id <- "23473-DE008L2"

# Queries and data loading -----------------------------------------------------

lvp_q <- paste0(
  "SELECT area, geom FROM ", st_layers(gs_dir)$name[1],
  " WHERE identifier LIKE '", id, "'"
)
lvp <- read_sf(gs_dir, query = lvp_q)

lvp_label <- st_point_on_surface(lvp) %>% 
  mutate(name = "Lene Voigt Park")

gs_q <- paste0(
  "SELECT area, geom, identifier FROM ", st_layers(gs_dir)$name[1],
  " WHERE code_2018 = 14100 OR code_2018 = 31000"
)

gs <- read_sf(gs_dir, query = gs_q)

ls_values <- read_sf(paste0(wd, "scenarios/ls_values.gpkg"))
di_values <- read_sf(paste0(wd, "scenarios/di_values.gpkg"))

# Bounding box & filters -------------------------------------------------------

bbox <- ls_values %>% st_bbox()
bbox_filter <- bbox %>% st_as_sfc() %>% st_as_text()

xmin <- bbox[1] + 1100
xmax <- bbox[3] - 1100
ymin <- bbox[2] + 1100
ymax <- bbox[4] - 1100

# Function to fix German umlauts -----------------------------------------------

umlaute <- function(variable) {
  variable %>%
    gsub("Ã¼", "ü", .) %>%
    gsub("ÃŸ", "ß", .) %>%
    gsub("Ã¶r|Ã¶", "ö", .) %>%
    gsub("Ã¤", "ä", .)
}

# Street labels ----------------------------------------------------------------

street_labs <- read_sf(paste0(wd, "lvp_osm.gpkg")) %>%
  st_transform(3035) %>%
  filter(!is.na(name), highway != "highway") %>%
  st_filter(st_as_sfc(bbox)) %>%
  mutate(name = umlaute(name)) %>%
  filter(name %in% c("Riebeckstraße", "Josephinenstraße")) %>%
  group_by(name) %>%
  summarise() %>%
  st_union(by_feature = TRUE) %>%
  st_centroid() %>%
  bind_rows(lvp_label) %>%
  mutate(lab = LETTERS[1:nrow(.)])

# Spatial data -----------------------------------------------------------------

build_sf <- read_sf(build_poly, wkt_filter = bbox_filter) %>% select(geom)
net_sf <- read_sf(edges, wkt_filter = bbox_filter) %>% select(geom)

# Base plot setup --------------------------------------------------------------

base_plot <- ggplot() +
  geom_sf(data = st_buffer(st_as_sfc(bbox), 1000), fill = "gray93") +
  geom_sf(data = build_sf, fill = "gray99", color = "gray99") +
  geom_sf(data = lvp, fill = "darkolivegreen4", color = NA, alpha = 0.2) +
  geom_sf(data = net_sf, color = "gray80", size = 1) +
  geom_sf(data = gs, fill = "darkolivegreen1", color = NA, alpha = 0.2)

# Overview plot ----------------------------------------------------------------

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
  geom_sf(data = water, color = "lightblue", fill = "lightblue") +
  geom_sf(fill = NA) +
  geom_sf(data = st_point_on_surface(lvp), color = "red", size = 3) +
  coord_sf(xlim = c(lpz_box[1], lpz_box[3]), ylim = c(lpz_box[2], lpz_box[4])) +
  geom_sf_text(data = lvp, aes(label = "Lene Voigt Park"), size = 3, nudge_y = 2000) +
  ggtitle("Leipzig") +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(color = "black"),
    plot.title.position = "panel"
  )

# Plotting Local Significance (LS) ---------------------------------------------

ls_plot <- base_plot +
  ls_values %>%
  select(ls) %>%
  filter(!is.na(ls)) %>%
  mutate(ls = log(ls)) %>%
  arrange(ls) %>%
  geom_sf(aes(color = ls), size = 1.2) +
  scale_color_distiller(palette = "RdBu") +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  labs(title = "Local Significance (LS)", color = "LS") +
  annotation_scale(aes(style = "ticks")) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.title = element_blank()
  )

# Plotting Detour Index (DI) ---------------------------------------------------

di_plot <- base_plot +
  di_values %>%
  select(di) %>%
  filter(!is.na(di)) %>%
  geom_sf(aes(fill = di, color = di)) +
  scale_fill_distiller(palette = "RdYlBu", direction = 1, breaks = seq(0, 0.8, 0.2)) +
  scale_color_distiller(palette = "RdYlBu", direction = 1, breaks = seq(0, 0.8, 0.2)) +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  labs(title = "Detour Index (DI)", fill = "DI", color = "DI") +
  annotation_scale(aes(style = "ticks")) +
  annotation_custom(
    grob = ggplotGrob(overview_plot),
    xmin = xmin - 100,
    xmax = xmin + ((ymax - ymin) / 3) * 0.88 - 100,
    ymin = ymin,
    ymax = ymin + (ymax - ymin) / 3
  ) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

# Save combined plot -----------------------------------------------------------

plot_grid(ls_plot, di_plot, nrow = 2) %>%
  ggsave(
    plot = .,
    filename = paste0(github, "3-1_ls_di_plot_no_labs.pdf"),
    width = 8.27,
    height = 11.69
  )

# Helper: Log transform preserving sign ----------------------------------------

mk_log <- function(numbers) {
  tibble(numbers) %>%
    mutate(num_log = log(abs(numbers))) %>%
    mutate(
      num_log = case_when(
        numbers == 0 ~ 0,
        numbers > 0 ~ num_log,
        numbers < 0 ~ -num_log,
        TRUE ~ num_log
      )
    ) %>%
    pull(num_log)
}

# Prepare data for scenario plots -------------------------------------------------

ls1_data <- ls_values %>%
  select(d_ls1) %>%
  filter(!is.na(d_ls1), d_ls1 != 0) %>%
  arrange(d_ls1) %>%
  mutate(d_ls1 = mk_log(d_ls1))

di1_data <- di_values %>%
  select(d_di1) %>%
  mutate(d_di1 = ifelse(abs(d_di1) <= 0.005, 0, d_di1)) %>%
  filter(!is.na(d_di1), d_di1 != 0)

target_ids <- c(
  "23502-DE008L2", "23493-DE008L2", "23485-DE008L2",
  "23508-DE008L2", "23509-DE008L2"
)

ls2_data <- ls_values %>%
  select(d_ls2) %>%
  filter(!is.na(d_ls2), d_ls2 != 0) %>%
  arrange(d_ls2) %>%
  mutate(d_ls2 = mk_log(d_ls2))

di2_data <- di_values %>%
  select(d_di2) %>%
  mutate(d_di2 = ifelse(abs(d_di2) <= 0.005, 0, d_di2)) %>%
  filter(!is.na(d_di2), d_di2 != 0)

build <- read_sf(build_poly) %>% select(ID)
pop_dat <- read_sf(paste0(wd, "scen3_be.gpkg")) %>%
  st_drop_geometry() %>%
  mutate(population = ifelse(population > 50, 50, population)) %>%
  left_join(build) %>%
  st_as_sf()

ls3_data <- ls_values %>%
  select(d_ls3) %>%
  filter(!is.na(d_ls3), d_ls3 != 0) %>%
  arrange(d_ls3) %>%
  mutate(d_ls3 = mk_log(d_ls3))

ls4_data <- ls_values %>%
  select(d_ls4) %>%
  filter(!is.na(d_ls4), d_ls4 != 0) %>%
  arrange(d_ls4) %>%
  mutate(d_ls4 = mk_log(d_ls4))

di4_data <- di_values %>%
  select(d_di4) %>%
  mutate(d_di4 = ifelse(abs(d_di4) <= 0.005, 0, d_di4)) %>%
  filter(!is.na(d_di4), d_di4 != 0)

# Plot parameters --------------------------------------------------------------

p_theme <- theme(
  axis.title = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  legend.position = "none"
)

p_scale <- annotation_scale(aes(style = "ticks"))

ls_bp <- rev(brewer.pal(11, "RdYlBu"))
ls_color <- scale_color_stepsn(
  colours = ls_bp,
  breaks = c(-10, -5, 0, 5, 10, 15),
  limits = c(-10, 15),
  oob = scales::squish
)

line_size <- 1.2

di_bp <- brewer.pal(5, "RdBu")
di_color <- scale_color_steps2(
  low = di_bp[1],
  mid = di_bp[3],
  high = di_bp[5],
  midpoint = 0,
  aesthetics = c("color", "fill"),
  limits = c(-0.3, 0.3)
)

# Scenario plots ---------------------------------------------------------------

ls1_plot <- base_plot +
  geom_sf(data = ls1_data, aes(color = d_ls1), size = line_size) +
  ls_color +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  labs(title = "Scenario 1: Local Significance (log-scale)") +
  p_theme +
  p_scale

di1_plot <- base_plot +
  geom_sf(data = di1_data, aes(fill = d_di1, color = d_di1)) +
  di_color +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  labs(title = "Scenario 1: Detour Index") +
  p_theme +
  p_scale

# Combine plots ----------------------------------------------------------------

final_plot <- plot_grid(ls1_plot, di1_plot, nrow = 2)

ggsave(
  filename = paste0(github, "scenario1_plot.pdf"),
  plot = final_plot,
  width = 8.27,
  height = 11.69
)
