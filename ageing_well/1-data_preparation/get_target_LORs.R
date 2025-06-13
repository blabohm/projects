library(sf)
library(dplyr)

lor_path_bzr <- "D:/LandOeko/AgeingWell/data/lor_shp_2021/lor_bzr.shp"
lor_path_plr <- "D:/LandOeko/AgeingWell/data/lor_shp_2021/lor_plr.shp"

# Filter Bezirke
lor_bzr_raw <- read_sf(lor_path_bzr)

target_lor_bzr_ids <- "^10.*|^12.*|^05.*"

lor_bzr_filtered <- filter(lor_bzr_raw, 
                       grepl(target_lor_bzr_ids,
                             BZR_ID, ignore.case = TRUE))
lor_top_level <- lor_filtered %>%
  transmute(BZR_ID = substr(BZR_ID, 1, 2)) %>%
  group_by(BZR_ID) %>%
  summarise(geometry = st_union(geometry)) %>%
  mutate(BZR_NAME = c("Spandau", "Marzahn-Hellersdorf", "Reinickendorf"))

write_sf(lor_top_level, "D:/LandOeko/AgeingWell/data/lor_target_bzr.gpkg")

# Filter Planungsräume
# Springpfuhl, Tegel Süd, Kurstraße und Eiswerder
# Marzahn- Hellersdorf: Auersbergstraße (10100314)
# Spandau: Ackerstr. (05100315)
# Reinickendorf: Alt-Tegel (1200515)

lor_plr_raw <- read_sf(lor_path_plr)

target_lor_plr_ids <- paste(
  05100313, 05100314, 05100315,
  10100312, 10100314, 
  12200413, 12200515,
  sep = "|")

#target_lor_names <- "Springpfuhl|Tegel Süd|Kurstraße|Eiswerder"
lor_plr_filtered <- filter(lor_plr_raw, 
                           grepl(target_lor_plr_ids, 
                                 PLR_ID, 
                                 ignore.case = TRUE))

write_sf(lor_plr_filtered, "D:/LandOeko/AgeingWell/data/lor_target_plr.gpkg",
         append = FALSE)

