# Change output name, input rasters, output shape name

#' Extract changed cells from two rasters
#' Get changes from two rasters from different periods. The function is suitable for parallel processing of very large rasters on computers with many cores.
#'
#' @param x t1 raster
#' @param y t2 raster
#' @param size tilesize
#' @param outpath Folder in which the temporary raster files will be created. If keep_temp argument is set to FALSE (default), files will be deleted after the process is sucessfully executed. Default path is R temporary directory (see tempdir()).
#' @param spatial Logical. When TRUE function returns SpatialPointsDataFrame. Default is FALSE.
#' @param category Land cover category to extract.
#' @param number_of_cores Number of cores to run. Default is 1.
#' @param outfile Output file name. Optional.
#'
#' @return None.
#' @export
#'
#' @examples Dunno
#' @importFrom GSIF getSpatialTiles
#' @importFrom rgdal GDALinfo readGDAL
#' @importFrom fs path_temp dir_create
#' @importFrom raster raster getValues setValues
#' @importFrom parallel makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach
#'
get_change_raster_GDAL <- function(x, y, size, outpath, outfile, values_of_interest, spatial = FALSE, number_of_cores = 2)
{
  stopifnot(!missing(outfile), !missing(values_of_interest))

  ###########################
  # Get raster path
  if (inherits(x, "Raster"))
  {
    x <- x@file@name
    y <- y@file@name
  }


  x_info <- rgdal::GDALinfo(x)

  if (missing(size))
  {
    size <- x_info["res.x"] * 1000
  }
  print(size)
  tiles_x <- GSIF::getSpatialTiles(x_info, block.x = size, return.SpatialPolygons = FALSE)

  if (missing(outpath))
  {
    outpath <- fs::path_temp("Mosaic_tempdir")
  }

  fs::dir_create(outpath)
  print("Par")


  cat(paste0("Running in parallel with ", number_of_cores, " cores."), "\n")
  cl <- parallel::makeCluster(number_of_cores)
  doParallel::registerDoParallel(cl)
  # Define custom function (opposite of %in%)
  "%notin%" <- Negate("%in%")

  # Foreach loop
  foreach::foreach(i = 1:nrow(tiles_x)) %dopar%
  {
    # Get single tile (layer x - t1)
    x_load <- rgdal::readGDAL(x, offset = unlist(tiles_x[i, c("offset.y", "offset.x")]),
                              region.dim = unlist(tiles_x[i, c("region.dim.y", "region.dim.x")]),
                              output.dim = unlist(tiles_x[i, c("region.dim.y", "region.dim.x")]),
                              silent = TRUE)
    # Get single tile (layer y - t2)
    y_load <- rgdal::readGDAL(y, offset = unlist(tiles_x[i, c("offset.y", "offset.x")]),
                              region.dim = unlist(tiles_x[i, c("region.dim.y", "region.dim.x")]),
                              output.dim = unlist(tiles_x[i, c("region.dim.y", "region.dim.x")]),
                              silent = TRUE)

    #### Load tiles as rasters
    x_ras <- raster::raster(x_load)
    y_ras <- raster::raster(y_load)
    #### Filter out the crop category values from t2 raster
    vals <- raster::getValues(y_ras)

    vals[vals %notin% values_of_interest] <- NA

    y_ras <- raster::setValues(y_ras, vals)
    #### Write raster !!CHANGE PATH!!
    diff_raster <- x_ras - y_ras
    print("vals")
    diff_raster_vals <- raster::getValues(diff_raster)
    diff_raster_vals[diff_raster_vals != 0] <- 1
    diff_raster_vals[diff_raster_vals == 0] <- NA
    diff_raster <- raster::setValues(diff_raster, diff_raster_vals)

    # Modify the filename below; modify also so it creates temp folder for the mosaics and deletes files after it's done
    # Use dir.create and check if output file exists then burn everything


    outmosaic <- paste0(outpath , "/", "tmpmosaic_", i,".tif")

    raster::writeRaster(diff_raster, outmosaic, format = "GTiff", overwrite = TRUE, options = c("COMPRESS=LZW"))
  }
  parallel::stopCluster(cl)
  # Delete temporary files if final output file exists
  # and if keep_temp argument is true (check if this actually works)

  out_folder_path <- paste0(outpath, "/*.tif")


  # if (file.exists(outfile))
  # {
  #   unlink(out_folder_path)
  # }


  # Check the bigtiff argument in gdal function
  x_rcl <- GDAL_mosaic_tiles(outfile, folder_path = out_folder_path, large_tif = TRUE, return_raster = TRUE)

  return(x_rcl)

}


# x_rcl_points <- raster::rasterToPoints(x_rcl, fun = function(x){x == 1}, sp = spatial) # FINISH!

