################################################################################
# Needs description
################################################################################
# Load required packages
req_pkg = c("dplyr", "terra", "parallel", "sf", "stringr")
lapply(req_pkg, require, character.only = TRUE)


################################################################################
# Get file names of TCD tiles from 2012 and 2018 in data directory 'data_dir'
mkDatDf <- function(data_dir)
{
  dplyr::tibble(tcd12 = list.files(paste0(data_dir, "tcd_2012_tiles/"),
                            full.names = TRUE, pattern = ".tif$"),
         tcd18 = list.files(paste0(data_dir, "tcd_2018_tiles/"),
                            full.names = TRUE, pattern = ".tif$"),
         n = 1:length(tcd18))
}


################################################################################
# Wrapper for executing a function 'fun' on a data list 'x' with additional
# parameters '...' on a set percentage of computation core 'perc_cores'
par_exec <- function(x,
                     fun,
                     ...,
                     perc_cores = .25,
                     n_cores = round(parallel::detectCores() * perc_cores))
{
  cl <- parallel::makeCluster(n_cores)
  parallel::parLapply(cl = cl, X = x, fun = fun, ... = ...)
  parallel::stopCluster(cl)
}


################################################################################
# Empty environment 'e' and do garbage collection (to prevent R from RAM
# hogging)
clean_up <- function(e)
{
  to_del <- ls(envir = e)
  rm(list = to_del, envir = e)
  gc(verbose = FALSE)
}


################################################################################
# Find tiles that have not been computed in a previous process.
# (In case the process crashed or was stopped)
find_missing <- function(directory,
                         n,
                         num_sep = "_")
{
  if (!dir.exists(directory)) return(1:n)
  vec <- paste0(num_sep, 1:n)
  files <- list.files(directory,
                      pattern = ".tif$", full.names = TRUE)
  if (length(files) == 0) return(message("no files"))
  string <- paste0(num_sep, "\\d{1,", nchar(n), "}")
  which_pos <- stringr::str_extract_all(files, string) %>%
    sapply(dplyr::last) %>%
    paste0(collapse = "$|") %>%
    grepl(., vec)
  missing_n <- vec[!which_pos] %>%
    gsub(num_sep, "", .) %>%
    as.numeric()
  if (length(missing_n) == 0) return(message("no files"))
  missing_n
}


################################################################################
# Replace non-forest pixels in 'TCDraw' data with NA for later masking and
# NA removal. In order to retrieve stable forest pixels for TCD calculation.
replaceNonForest <- function(TCDraw)
{
  TCDforest <- TCDraw
  TCDforest[TCDforest < 30 | TCDforest > 100] <- NA
  TCDforest
}


################################################################################
# Wrapper around terra::writeRaster with auto-creation of directory and
# generation of file name. Default datatype is "INT1U"
raster_out <- function(raster,
                       directory,
                       string,
                       i,
                       dtype = "INT1U")
{
  if (!dir.exists(directory)) dir.create(directory)
  terra::writeRaster(raster, paste0(directory, string, i, ".tif"),
                     datatype = dtype, overwrite = TRUE)
}


################################################################################
# count number of forested cells in 'x' during aggregation
rel_rst <- function(x)
{
  length(x[x < 101 & x > 29 & !is.na(x)])  / 1e4
}


################################################################################
# END
################################################################################

