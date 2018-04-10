# Function for preparing data for modeling

# Explanatory_variables - raster data, output from get_rasters()
# Response_variable - data for fitting, output from load_PA()
# Evaluation data - data for evaluation, output from load_PA(). Necessary if cross_validate = FALSE
# VIF_select - do VIF selection of predictiors (only works now for cross_validate = TRUE)
# cross_validate - assess models on cross validated or hind casted dataset

#' Format data
#'
#' Prepares data for modeling. Gives list with training and evaluation data needed for the model.
#'
#' @param explanatory_variables Raster data, output from get_rasters()
#' @param response_variable Data for fitting, output from load_PA()
#' @param evaluation_data Data for evaluation, output from load_PA(). Necessary if cross_validate = FALSE
#' @param VIF_select Do VIF selection of predictiors (only works now for cross_validate = TRUE). Not necessary, since VIF selection is now done a priori.
#' @param cross_validate Assess models on cross validated or hind casted dataset. Default is FALSE.
#' @param threshold Thershold for VIF calculation. Not needed anymore (see VIF_select arg).
#' @param explanatory_variables_eval Raster stack of explanatory variables used for hind casting. Necessary only if cross_validate = FALSE.
#'
#' @return List of 2. Training data and evaluation data.
#' @export
#'
#' @examples None.
#' @importFrom raster extract
#' @importFrom tidyr drop_na
#' @importFrom stringr str_detect str_replace
#' @importFrom caret createDataPartition
format_data <- function(explanatory_variables, response_variable, evaluation_data, explanatory_variables_eval,
                        VIF_select = FALSE, cross_validate = FALSE, threshold = 10)
{

  if (VIF_select)
  {
    cat(paste0("Performing VIF selection with VIF threshold VIF=", threshold, "..."), "\n")
    explanatory_variables <- vif_select_vars(explanatory_variables, thresh = threshold)
  }
  if (cross_validate == FALSE & missing(explanatory_variables_eval))
  {
    stop("Please add rasters for evaluation if cross_validate = FALSE")
  }

  # Extract values from a raster file for modeling
  region_values_raw <- raster::extract(explanatory_variables, response_variable, sp = TRUE)
  region_values <- region_values_raw@data

  # Remove points with NA data
  region_data_formatted <- region_values %>%
    tidyr::drop_na()

  # Make categorical variables as factor
  categorical_rasters_index <- region_data_formatted %>%
    names() %>%
    stringr::str_detect("catg") %>%
    which()

  region_data_formatted[[categorical_rasters_index[1]]] <- as.factor(region_data_formatted[[categorical_rasters_index[1]]])
  region_data_formatted[[categorical_rasters_index[2]]] <- as.factor(region_data_formatted[[categorical_rasters_index[2]]])


  # Prepare data for modeling ####
  # Create data partition - in this case cross validated dataset (66% of the data goes to model training)

  # Create empty list that will in the end return training and evaluation data
  region_data_for_modeling <- list()

  if (cross_validate)
  {
    print("CV")

    region_data_partition <- caret::createDataPartition(region_data_formatted$PA, p = 0.66, list = FALSE)

    # Subset data for training and testing
    region_train_data <- region_data_formatted[region_data_partition,]
    region_test_data <- region_data_formatted[-region_data_partition,]

    # Put cross validation data into empty list
    region_data_for_modeling[["training_data"]] <- region_train_data
    region_data_for_modeling[["evaluation_data"]] <- region_test_data
    return(region_data_for_modeling)

  } else {

    print("hindcast")
    region_values_evaluation_raw <- raster::extract(explanatory_variables_eval, evaluation_data, sp = TRUE)
    region_evaluation_values <- region_values_evaluation_raw@data

    # Remove points with NA data
    region_data_evaluation_formatted <- region_evaluation_values %>%
      drop_na()

    # Make categorical variables as factor
    categorical_rasters_index <- region_data_evaluation_formatted %>%
      names() %>%
      str_detect("catg") %>%
      which()

    region_data_evaluation_formatted[[categorical_rasters_index[1]]] <- as.factor(region_data_evaluation_formatted[[categorical_rasters_index[1]]])
    region_data_evaluation_formatted[[categorical_rasters_index[2]]] <- as.factor(region_data_evaluation_formatted[[categorical_rasters_index[2]]])

  }
  # Put hind casting data into empty list
  region_data_for_modeling[["training_data"]] <- region_data_formatted
  names(region_data_for_modeling[["training_data"]]) <- stringr::str_replace(names(region_data_for_modeling[["training_data"]]), "_fit_catg", "_catg")

  region_data_for_modeling[["evaluation_data"]] <- region_data_evaluation_formatted
  names(region_data_for_modeling[["evaluation_data"]]) <- stringr::str_replace(names(region_data_for_modeling[["evaluation_data"]]), "_eval_catg", "_catg")

  return(region_data_for_modeling)
}
