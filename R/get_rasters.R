# Function to load rasters for a specific region

#' Load raster data
#'
#' This function loads raster data (predictor variables) necessary for agri modeling.
#' File paths are hardcoded.
#'
#' @param region Region for which to get the rasters.
#'
#' @return RasterStack
#' @export
#'
#' @import Rahat
#' @importFrom raster stack
#'
#' @examples get_rasters("Korea_region")

get_rasters <- function(region)
{

  region_rasters <- "Projects/Land_use/Data/Predictors/Normalized_IMAGE_regions/" %>%
    paste0(region) %>%
    Rahat::milkunize() %>%
    list.files(pattern = ".tif$", full.names = TRUE) %>%
    raster::stack()

  return(region_rasters)
}
