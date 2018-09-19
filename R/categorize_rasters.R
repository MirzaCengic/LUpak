# Function that creates dummy variables for categorical raster data
# In this case only the previous land use and protected areas predictors will be dummied

# source("/vol/milkun1/Mirza_Cengic/layerize2.R")

#' Categorize rasters
#'
#' This function creates dummy variables for categorical variable levels.
#' @param raster_data Raster stack to be dummified.
#' @param data Modeling data. Output of \code{format_data()} function.
#'
#' @return RasterStack
#' @export
#'
#' @examples None
#' @importFrom raster layerize dropLayer stack
#'
 categorize_rasters <- function(raster_data, data)
{
  # Create dummy variables for previous land cover
  dummy_variables_LC <- raster::layerize(raster_data$Previous_land_cover_catg)
  # Replace names for LC
  names(dummy_variables_LC) <- c("Previous_land_cover_catg_1",
                                 "Previous_land_cover_catg_2",
                                 "Previous_land_cover_catg_3",
                                 "Previous_land_cover_catg_4",
                                 "Previous_land_cover_catg_5")
  # Create dummy variables for protected areas
  dummy_variables_PA <- raster::layerize(raster_data$Protected_areas_catg)
  # Replace names
  names(dummy_variables_PA) <- c("Protected_areas_catg_0", "Protected_areas_catg_1")
  # Drop previous land cover layer - to be replaced with 4 level dummy variable
  raster_data <- raster::dropLayer(raster_data, c("Previous_land_cover_catg", "Protected_areas_catg"))
  raster_data <- raster::stack(raster_data, dummy_variables_LC, dummy_variables_PA)
  return(raster_data)
}
