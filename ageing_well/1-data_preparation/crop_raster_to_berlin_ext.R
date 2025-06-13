
berlin_uc <-
  sf::read_sf("D:/LandOeko/AgeingWell/data/DE001L1_BERLIN_UA2018_v013.gpkg",
              layer = "DE001L1_BERLIN_UA2018_UrbanCore") %>%
  st_transform(3035)


tcd18_berlin <-
  crop(rast("Z:/ch/data/TCD_2018_010m_eu_03035_v020/DATA/TCD_2018_010m_eu_03035_V2_0.tif"),
       st_bbox(berlin_uc))

writeRaster(tcd18_berlin * 1,
            "D:/LandOeko/AgeingWell/data/tcd2018_berlin.tif",
            overwrite = TRUE)
