#' Rarify points
#'
#' Take a set of spatial points data, and sample them according the the raster grid.
#' Points will be sampled so a specified number of points per grid cell remains.
#' Number of points is defined by \code{points_number} argument (still experimental if points_number > 1).
#'
#' @param point_data Point data to rarify. Should be of \emph{class sp}.
#' @param raster_data Raster grid used to sample points. \emph{Raster*} object.
#' @param points_number Number of points to sample per grid cell. Default is 1. \strong{Using points_number > 1 is still experimental.}
#' @param return Type of object to return. One of \code{c("sf", "sp")}. Default is \emph{"sf"}.
#'
#' @return Spatial point data. Either sp or sf class.
#' @export
#'
#' @examples
#' library(LUpak)
#'
#' bioclim_mask <- raster(milkunize("Temp/Korea_mask_crop.tif"))
#' korea_points <-  shapefile(milkunize("Temp/Korea_points_crop.shp"))
#'
#' my_crops_rarified <- rarify_points(korea_points, bioclim_mask)
#' @importFrom magrittr "%>%"
#' @importFrom raster crs
#' @importFrom sp spTransform proj4string over
#' @importFrom data.table data.table
#' @importFrom sf st_as_sf
rarify_points <- function(point_data, raster_data, points_number = 1, return = "sf")
{
  # argchecks
  stopifnot(return %in% c("sf", "sp"))

  if (missing(raster_data) | missing(point_data))
  {
    stop("Input data missing.")
  }

  # Convert raster to a spatial grid
  raster_grid <- as(raster_data, "SpatialGrid")
  # Check coordinate system
  if (!identical(raster::crs(raster_data, asText = TRUE), sp::proj4string(point_data)))
  {
    point_data <- sp::spTransform(point_data, raster::crs(raster_data, asText = TRUE))
    warning("Point data and raster data CRS is not identical. Point data CRS will be modified.")
  }
  # Intersect point and raster data
  point_data$grid <- sp::over(point_data, raster_grid)

  point_data_sf <- point_data %>%
                st_as_sf() %>%
    data.table::data.table() %>%
                unique(by = "grid") %>%
            sf::st_as_sf()


  if (return == "sf")
  {
    return(point_data_sf)
  } else {
    point_data_sp <- as(point_data_sf, "Spatial")
    return(point_data_sp)
  }
}
