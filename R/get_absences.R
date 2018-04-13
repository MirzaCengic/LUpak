##%######################################################%##
#                                                          #
####     Reclassify land cover categories function      ####
#                                                          #
##%######################################################%##

# Script contains two helper functions (rcl_excluded_vals and rcl_na) and a main get_absences function.


#' Reclassify excluded values
#'
#' This is a helper function for absences creation that takes a raster, and reclassifies it to 0-1 raster.
#' Reclassified raster should be ESA CCI LC data. For the \code{values_included} argument,
#' numerical vector of all land cover values in the raster (they are turned to value 1).
#' For the \code{values_excluded} argument, numerical vector of land cover categories to be excluded
#' from absence creation (usually the modeled category and urban/bare areas)
#' should be provided (they are turned to value 0).
#' Argument \code{filename} is the name of the temporary file created.
#'
#' @param x Input raster to be classified.
#' @param vals_included Values of all possible categories (without the excluded values).
#' @param vals_excluded Values to be excluded from absence creation. Usually modeled category or urban.
#' @param filename Temporary file to write.
#'
#' @return Raster object.
#' @export
#'
#' @examples None.
#' @importFrom raster raster blockSize writeStart getValues writeValues writeStop
rcl_excluded_vals <- function(x, vals_included, vals_excluded, filename)
{
  out <- raster::raster(x)
  bs <- raster::blockSize(out)
  out <- raster::writeStart(out, filename, overwrite=TRUE)
  for (i in 1:bs$n)
  {
    vals <- raster::getValues(x, row=bs$row[i], nrows=bs$nrows[i] )

    ####
    vals[vals %in% vals_included] <- 0
    vals[vals %in% vals_excluded] <- 1



    ####
    out <- raster::writeValues(out, vals, bs$row[i])
  }
  out <- raster::writeStop(out)
  return(out)
}

#' Reclassify to NA
#'
#' Helper function that reclassifies two summed change rasters.
#'
#' @param x Input raster.
#' @param filename Temporary filename.
#'
#' @return Raster object.
#' @export
#'
#' @examples None
#' @importFrom  raster raster blockSize writeStart getValues writeValues writeStop
rcl_na <- function(x, filename)
{

  out <- raster::raster(x)
  bs <- raster::blockSize(out)
  out <- raster::writeStart(out, filename, overwrite=TRUE)

  for (i in 1:bs$n)
  {
    vals <- raster::getValues(x, row=bs$row[i], nrows=bs$nrows[i] )
    ####
    vals[vals %in% 1:2] <- NA
    ####
    out <- raster::writeValues(out, vals, bs$row[i])
  }
  out <- raster::writeStop(out)
  return(out)
}

#### Main get absences function ####


#' Get absences
#'
#' Take land cover rasters from two timesteps (old and new).
#' @param raster_old Raster from the previous timestep.
#' @param raster_new Raster from the future timestep.
#' @param category Modeled land cover category. Currently only agriculture is supported. Numeric. One of c(10, 30, 40).
#' @param abs_number Number of absences to create. Default is 10 000.
#' @param exclude_urban Should the urban areas be excluded from the absences calculation. If \code{exclude_urban = TRUE},
#' absences will not be created in urban areas (class 190) in addition to modeled category. Default is FALSE.
#' @param sp Logical. If \code{sp = TRUE}, spatial object of class sp will be returned. Otherwise a dataframe (default).
#'
#' @return Dataframe or sp object (depending on the \code{sp} argument).
#' @export
#'
#' @examples None.
#' @importFrom fs file_temp file_delete
#' @importFrom raster sampleRandom
get_absences <- function(raster_old, raster_new, category, abs_number,
                         exclude_urban = FALSE, sp = FALSE)
{

  # Category 10, 11, 12 or 666 (use 10, 11, 12 for separate modeling, 666 just have when all rainfed categories are used)
  if(category == 10)
  {
    values_excluded <- 10:20
  } else if(category == 30)
  {
    values_excluded <- 30
  } else if(category == 40)
  {
    values_excluded <- 40
  } else {stop("Enter correct crop category to model")}
  # Values for all of the possible categories
  all_categories <- 1:221

  # Add fork for cases when the urban areas should be excluded from the absence creation
  if (exclude_urban)
  {
    urban <- 190
    values_excluded <- c(values_excluded, urban, 210:220)
    values_included <- all_categories[-values_excluded]
  } else {
    values_excluded <- c(values_excluded, 210:220)
    values_included <- all_categories[-values_excluded]
  }

  # Reclassify previous timestep raster
  tmppath_prev <- as.character(fs::file_temp(tmp_dir = "/scratch", ext = ".tif"))
  #
  cat("Reclassifying crops for previous timestep", "\n")
  raster_layer_prev <- rcl_excluded_vals(raster_old, values_included, values_excluded, tmppath_prev)
  # Reclassify previous timestep raster
  tmppath_fut <- as.character(fs::file_temp(tmp_dir = "/scratch", ext = ".tif"))
  #
  cat("Reclassifying crops for future timestep", "\n")
  raster_layer_fut <- rcl_excluded_vals(raster_new, values_included, values_excluded, tmppath_fut)
  ##########
  LC_sum <- raster_layer_prev + raster_layer_fut


  tmppath_combined <- as.character(fs::file_temp(tmp_dir = "/scratch", ext = ".tif"))

  # reclassify summed raster file
  both <- rcl_na(LC_sum, tmppath_combined)
  cat("Creating absences", "\n")

  # Function fork - sp argument (decide what type of object does the function returns)
  if (sp)
  {
    absences <- raster::sampleRandom(both, abs_number, na.rm = TRUE, sp = TRUE)
    names(absences) <- "PA"
  } else {
    absences <- raster::sampleRandom(both, abs_number, na.rm = TRUE, xy = TRUE)
    absences <- absences[, c("x", "y")]
  }

  # Remove temporary files
  fs::file_delete(c(tmppath_combined,
                    tmppath_fut,
                    tmppath_prev))

  return(absences)

}
