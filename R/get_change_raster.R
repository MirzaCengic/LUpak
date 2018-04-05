# Set notin function
"%notin%" <- Negate("%in%")

# Main get change function. It has been thorougly optimized.

#' Get difference between two rasters
#'
#' Function will take a RasterStack or RasterBrick object with two layers and return a raster where the cells that have changed
#' between the two layers have value of 1. If the raster data has temporal aspect,
#' then the \code{raster_input[[1]]} is \emph{t1 timestep}, while \code{raster_input[[2]]} is \emph{t2 timestep}.
#' Since the two raster objects should match, the passed variable has to be RasterStack or RasterBrick
#' (RasterStack will be converted to RasterBrick, therefore it might be more economical to work with RasterBrick as an input here).
#' Values for the change is observed can be clamped. For example, when applying the function to the ESA CCI LC data,
#' if you only want to extract changes of agriculture, then \code{category} argument can have values of agricultural classes
#' (eg. \code{category = 10:40}).
#'
#' @param raster_input RasterStack or RasterBrick of t1 and t2 timestep.
#' @param filename Output file. Full path with .tif extension. If missing, temporary file will be created (file sizes may get large).
#' @param category Land cover category to extract.
#' @param return_raster Logical. The function stores the raster in the \code{filename} argument path as a side-effect.
#' If \code{return_raster = TRUE} (default), then the function returns the stored raster.
#'
#' @return Binary raster with 1 values as changes. Only if \code{return_raster = TRUE}. Otherwise, the function side-effect is to save the file locally.
#' @export
#'
#' @examples None yet.
#' @importFrom raster nlayers brick stack raster subset blockSize getValuesBlock writeStart writeStop writeValues



get_change_raster <- function(raster_input, filename, category, return_raster = TRUE)
{
  if (missing(filename))
  {
    filename <- tempfile(fileext = ".tif")
     warning("Argument filename is missing. Temporary raster file will be created.")
  }

  if (nlayers(raster_input) > 2)
  {
    stop("There are more than 2 timesteps in the input raster. Pass data with only t1 and t2 component.")
  }

  # change input into raster brick
  if (inherits(raster_input, "RasterStack"))
  {
    # message("Input RasterStack will be converted to RasterBrick")
    raster_input <- brick(raster_input)
  }

  if (!inherits(raster_input, "RasterBrick"))
  {
    stop("Input has to be RasterBrick or RasterStack")
  }

  raster_to_write <- writeStart(raster::subset(raster_input, 1), filename, format = "GTiff", overwrite = TRUE)
  block_size <- blockSize(raster_input)

  for (i in 1:block_size$n) {
    vals <- getValuesBlock(raster_input, row = block_size$row[i], nrows = block_size$nrows[i])
    # Check which values aren't crops

    # Exclude values from category var
    no_crop_index <- which(vals[, 2] %notin% category)
    # Convert to NA
    vals[no_crop_index, ] <- NA
    # binarize changes

    # Slower implementation
    # is_change <- ifelse(vals[, 1] == vals[, 2], 0, 1)

    is_change <- rep(1, nrow(vals))
    is_change[no_crop_index] <- NA
    is_change[vals[, 1] == vals[, 2]] <- 0

    # Assign values
    raster_to_write <- writeValues(raster_to_write, v = is_change, block_size$row[i])
  }
  raster_to_write <- writeStop(raster_to_write)

  if (isTRUE(return_raster))
  {

    change_raster <- raster(filename)
    return(change_raster)
  }
}
