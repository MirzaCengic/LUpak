# Function to load presence absence data

# region - IMAGE region
# category - crop category
# type - data for model fitting or model evaluation (character)
#' Load response variable
#'
#' Load response variable (presences/absences) for specific region, category and assessment method.
#' @param region Which region.
#' @param category Which category.
#' @param type Hind-casted or cross-validated type of data. One of \code{c("Fit", "Eval")}
#' @param path Folder path
#'
#' @return Dataframe
#' @export
#'
#' @examples None so far
#' @import Rahat
#' @import stringr
#' @importFrom dplyr transmute mutate select bind_rows
#' @importFrom sf st_as_sf
load_PA <- function(region, category, type, path)
{

  stopifnot(type %in% c("Fit", "Eval"))
  if(missing(path))
  {
    path <- "LU_data/Changes/"
  }
  # Get path for change data - str_subset controls the region, list.files the category
  region_files <- path %>%
    paste0(type) %>%
    Rahat::milkunize("m5") %>%
    list.dirs(recursive = TRUE, full.names = TRUE) %>%
    stringr::str_subset(paste0(region, "$")) %>%
    list.files(pattern = category, full.names = TRUE)

  # Load csvs with presence and absence coordinates
  region_abs <- read.csv(region_files[1])
  region_pres <- read.csv(region_files[2])

  # clean dataset
  region_abs <- region_abs %>%
    dplyr::transmute(x = x,
                     y = y,
                     PA = rep(0, nrow(.)))

  region_pres <- region_pres %>%
    dplyr::transmute(
      x = X,
      y = Y,
      PA = rep(1, nrow(.)))

  # Load data for model evaluation ####
  region_PAs <- dplyr::bind_rows(region_pres, region_abs)
  # Convert to sf
  region_PAs_sf <- sf::st_as_sf(region_PAs, coords = c("x", "y"), crs = 4326)
  # Extract values from raster ####
  # Convert sf to sp
  region_PAs_sp <- as(region_PAs_sf, "Spatial")

  return(region_PAs_sp)
}

# ####
# path <- "LU_data/Changes_values/"
# region <- "Korea_region"
# type <- "Fit"
# category <- "10"

# region_files <- path %>%
# paste0(type) %>%
#   Rahat::milkunize("m5") %>%
#   list.dirs(recursive = TRUE, full.names = TRUE) %>%
#   stringr::str_subset(region) %>%
#     list.files(pattern = category, full.names = TRUE)

# Function that loads newly created absences
#' Load data2
#'
#' Second version of the function, hard coded file paths should be removed.
#' @param region Which region.
#' @param category Which category.
#' @param type Fit or eval.
#'
#' @return Dataframe
#' @export
#'
#' @examples None.
#' @import Rahat
#' @import stringr
#' @import dplyr
#' @import sf

load_PA2 <- function(region, category, type)
{
  # Presence data
  region_pres <- "LU_data/Changes_four_categ/" %>%
    paste0(type) %>%
    Rahat::milkunize("m5") %>%
    list.dirs(recursive = TRUE, full.names = TRUE) %>%
    stringr::str_subset(paste0(region, "$")) %>%
    list.files(pattern = category, full.names = TRUE) %>%
    str_subset("Presences") %>%
    read.csv() %>%
    dplyr::transmute(x = X,
                     y = Y,
                     PA = rep(1, nrow(.)))

  # Absence data
  region_abs <- "LU_data/Changes_test/" %>%
    paste0(type) %>%
    Rahat::milkunize("m5") %>%
    list.dirs(recursive = TRUE, full.names = TRUE) %>%
    stringr::str_subset(paste0(region, "$")) %>%
    list.files(pattern = category, full.names = TRUE) %>%
    str_subset("Absences") %>%
    read.csv() %>%
    dplyr::transmute(x = x,
                     y = y,
                     PA = rep(0, nrow(.)))

  # Load data for model evaluation ####
  region_PAs <- bind_rows(region_pres, region_abs)
  # Convert to sf
  region_PAs_sf <- sf::st_as_sf(region_PAs, coords = c("x", "y"), crs = 4326)
  # Extract values from raster ####
  # Convert sf to sp
  region_PAs_sp <- as(region_PAs_sf, "Spatial")

  return(region_PAs_sp)
}
