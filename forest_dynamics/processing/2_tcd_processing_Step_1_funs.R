
# Harmonized forest area and condition processing on 1km resolution
# 1. Aggregate forest condition inside stable forest
# 2. Calculate forest area
# 3. Calculate sensitivity layers
# Author: Benjamin Labohm
# Year: 2024


# TCD change in stable forest aggregated to 1000 m ------------------------

tcd_change_stable_forest <- function(data_vector,
                                     data_dir = data_vector$data_dir,
                                     github_dir = data_vector$github_dir)
{

  # FOR PARALLEL PROCESSING:
  # Load packages into core
  req_pkg = c("dplyr", "terra", "parallel", "sf")
  lapply(req_pkg, require, character.only = TRUE)
  # Load function
  source(paste0(github_dir,
                "processing/2_tcd_processing_Step_1_and_2_general_funs.R"))

  # Data input
  # Counting
  i <- data_vector$n

  # Input TCD data (*1 to remove description)
  tcd12raw <- rast(data_vector$tcd12) * 1
  tcd18raw <- rast(data_vector$tcd18) * 1

  # Processing
  # Retrieve forested pixels
  tcd12forest <- replaceNonForest(tcd12raw)
  tcd18forest <- replaceNonForest(tcd18raw)

  # aggregate tcd 2018 to same resolution as tcd 2012
  tcd18forest <- aggregate(tcd18forest, fact = 2, fun = mean, na.rm = TRUE)

  # Mask both years with each other to retrieve stable forest
  # (to prevent bias by regrowth or forest loss)
  tcd12forest <- mask(tcd12forest, tcd18forest)
  tcd18forest <- mask(tcd18forest, tcd12forest)

  # Aggregate
  tcd12agg <- aggregate(tcd12forest, fact = 50, fun = mean, na.rm = TRUE)
  tcd18agg <- aggregate(tcd18forest, fact = 50, fun = mean, na.rm = TRUE)

  # Output
  raster_out(tcd12agg,
             paste0(data_dir, "TCD12_stable_forest_tiles/"),
             "tcd_2012_", i)
  raster_out(tcd18agg,
             paste0(data_dir, "TCD18_stable_forest_tiles/"),
             "tcd_2018_", i)

  # Clean up ram
  clean_up(environment())
}


# TCA change aggregated to 1000 m -----------------------------------------

tca_change <- function(data_vector,
                       data_dir = data_vector$data_dir,
                       github_dir = data_vector$github_dir)
{

  # For parallel processing
  # Load packages into core
  req_pkg = c("dplyr", "terra", "parallel", "sf")
  lapply(req_pkg, require, character.only = TRUE)
  # Load functions
  source(paste0(github_dir,
                "processing/2_tcd_processing_Step_1_and_2_general_funs.R"))

  # Processing
  i <- data_vector$n
  tca12 <- aggregate(rast(data_vector$tcd12) * 1,
                     fact = 50, fun = rel_rst) * 400
  tca18 <- aggregate(rast(data_vector$tcd18) * 1,
                     fact = 100, fun = rel_rst) * 100

  # Output
  raster_out(tca12, paste0(data_dir, "TCA12_tiles/"), "tca_2012_", i) # values 0-100
  raster_out(tca18, paste0(data_dir, "TCA18_tiles/"), "tca_2018_", i) # values 0-100

  # Clean up ram
  clean_up(environment())
}


# Generate sensitivity layers ---------------------------------------------

sensitivity_lyrs <- function(data_vector,
                             data_dir = data_vector$data_dir,
                             github_dir = data_vector$github_dir)
{

  # For parallel processing
  # Load packages into core
  req_pkg = c("dplyr", "terra", "parallel", "sf")
  lapply(req_pkg, require, character.only = TRUE)

  # Load functions
  source(paste0(github_dir,
                "processing/2_tcd_processing_Step_1_and_2_general_funs.R"))

  # Processing
  i <- data_vector$n

  na12 <- aggregate(rast(data_vector$tcd12) * 1,
                    fact = 50,
                    fun = function(x) {length(x[x > 100])}) * 400
  threshold12 <- aggregate(rast(data_vector$tcd12) * 1,
                           fact = 50,
                           fun = function(x) {length(x[x < 30])}) * 400
  na18 <- aggregate(rast(data_vector$tcd18) * 1,
                    fact = 100,
                    fun = function(x) {length(x[x > 100])}) * 100
  threshold18 <- aggregate(rast(data_vector$tcd18) * 1,
                           fact = 100,
                           fun = function(x) {length(x[x < 30])}) * 100

  # Output
  raster_out(na12,
             paste0(data_dir, "na12_tiles/"), "na_2012_", i, dtype = "INT2U") # values 0-100
  raster_out(threshold12,
             paste0(data_dir, "threshold12_tiles/"), "threshold_2012_",
             i, dtype = "INT2U") # values 0-100
  raster_out(na18,
             paste0(data_dir, "na18_tiles/"), "na_2018_", i, dtype = "INT2U") # values 0-100
  raster_out(threshold18,
             paste0(data_dir, "threshold18_tiles/"), "threshold_2018_",
             i, dtype = "INT2U") # values 0-100

  # Clean up ram
  clean_up(environment())
}


# End

