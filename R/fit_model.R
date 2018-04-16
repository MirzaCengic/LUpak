#' Fit model
#'
#' Fits model for agricultural conversion with implemented weighing scheme.
#' #'
#'
#' @param data Data for modeling. Output of \code{format_data()} function.
#' @param aic_step Perform aic stepwise model selection. Default is \code{TRUE}.
#' @param weights Should the weighting scheme be implemented? Default is \code{FALSE}.
#'
#' @return Model of "glm" class.
#' @export
#'
#' @examples None.
fit_model <- function(data, aic_step = TRUE, weights = FALSE)
{
  model_fitting_data <- data[["training_data"]]
  # Fit model

  # Set presence absence weight - equal weighting scheme
  if (weights)
  {
    P_proportion <- round(sum(model_fitting_data$PA == 1) / length(model_fitting_data$PA) * 10)
    A_proportion <- round(sum(model_fitting_data$PA == 0) / length(model_fitting_data$PA) * 10)
    weights <- ifelse(model_fitting_data$PA == 1, P_proportion, A_proportion)
    #
    message(paste0("Implementing weights with proportions of presence:", unique(P_proportion),
                   ", and proportion for absence: ", unique(A_proportion)))
    glm_model <- glm(PA ~ . , data = model_fitting_data, family = binomial,
                     weights = weights, control = glm.control(epsilon = 1e-08, maxit = 50, trace = FALSE))
  } else {
    message("Fitting the models without weighing...")
    glm_model <- glm(PA ~ . , data = model_fitting_data, family = binomial,
                     control = glm.control(epsilon = 1e-08, maxit = 50, trace = FALSE))
  }

  #
  if (aic_step)
  {
    glm_model <- step(glm_model)
    return(glm_model)
  } else {
    return(glm_model)
  }
}
