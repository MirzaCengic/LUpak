##%######################################################%##
#                                                          #
####                   Evaluate model                   ####
#                                                          #
##%######################################################%##

# Function to calculate model evaluation metrics from fitted land use conversion models
# Mirza Cengic | mirzaceng@gmail.com

#' Evaluate model
#' Get evaluation metrics (currently AUC and TSS) for agricultural conversion models.
#'
#' @param fitted_model glm model. Output of fit_model() function (or glm() function in that matter). Any model of class "glm".
#' @param data - data for modeling. Output of format_data() function. List with "evaluation_data" slot.
#'
#' @return dataframe with TSS and AUC values.
#' @export
#'
#' @examples None.
#' @importFrom dplyr transmute select pull
#' @importFrom PresenceAbsence optimal.thresholds
#' @importFrom tidyr drop_na spread
#' @importFrom dismo evaluate
evaluate_model <- function(fitted_model, data)
{
  # In the case that the evaluation data is not in the list form, change this line. Evaluation data should have a column "PA" with zeros and ones, and other columns are the predictors.
  model_evaluation_data <- data[["evaluation_data"]]

  # Predict model with evaluation data
  glm_model_prediction <- predict(fitted_model, newdata = model_evaluation_data, type = "response")

  # Calculate binarization threshold (for TSS calculation) ####
  # Get dataframe with observed values (1/0) and predicted suitability values
  model_obs_pred <- model_evaluation_data %>%
    dplyr::transmute(
      ID = row.names(.),
      Observed = PA,
      Predicted = glm_model_prediction)

  # Prepare dataframe for dismo::evaluate function
  predicted <- model_evaluation_data %>%
    dplyr::transmute(
      ID = row.names(.),
      Observed = ifelse(PA == 1, "P", "A"),
      Predicted = glm_model_prediction) %>%
    tidyr::spread(Observed, Predicted) %>%
    dplyr::select(-ID)
  # Prepare dataframe for PresenceAbsence threshold function
  model_obs_pred <- model_evaluation_data %>%
    dplyr::transmute(
      ID = row.names(.),
      Observed = PA,
      Predicted = glm_model_prediction)

  # Calculate optimal threshold by maximizing the sum of sensitivity and specificity (max TSS)
  best_thres <- PresenceAbsence::optimal.thresholds(model_obs_pred, opt.methods = 3)$Predicted

  # Get vector of predicted values at presence and absence locations
  presence_values <- predicted %>%
    dplyr::select(P) %>%
    tidyr::drop_na() %>%
    dplyr::pull()
  absence_values <- predicted %>%
    dplyr::select(A) %>%
    tidyr::drop_na() %>%
    dplyr::pull()

  model_evaluation <- dismo::evaluate(p = presence_values, a = absence_values, model = fitted_model, tr = best_thres)

  # Calculate TSS (TSS = sensitivity + specificity - 1) ####
  # Extract values from slots
  TSS <- model_evaluation@TPR + model_evaluation@TNR - 1
  AUC <- model_evaluation@auc
  # Create data for final output
  model_evaluation <- data.frame("AUC" = AUC, "TSS" = TSS, row.names = NULL)

  return(model_evaluation)
}


#' Get model evaluations
#'
#' This function wraps around \code{evaluate_model()} and returns a list that
#' contains \code{model_evaluation} and \code{model_coefficients} dataframes.
#'
#' @param fitted_model - glm model. Output of fit_model() function (or glm() function in that matter). Any model of class "glm".
#' @param data - data for modeling. Output of format_data() function. List with "evaluation_data" slot.
#' @param ID - model_id for the current model. Stored in a variable. Region + category
#'
#' @return List with two dataframes.
#' @export
#'
#' @examples None.
#'
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr transmute mutate
#' @importFrom broom tidy
get_evaluations <- function(fitted_model, data, ID)
{

  model_eval <- fitted_model %>%
    evaluate_model(data) %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    dplyr::transmute(
      Var = rowname,
      Value = V1,
      Model_ID = ID)

  # Get model coefficients
  model_coeff <- fitted_model %>%
    broom::tidy() %>%
    dplyr::mutate(Model_ID = ID)

  model_assessm <- list()
  model_assessm[["model_evaluation"]] <- model_eval
  model_assessm[["model_coefficients"]] <- model_coeff

  return(model_assessm)
  }

