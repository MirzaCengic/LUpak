# Function to fit land use conversion model

# data - modeling data. Output of format_data() function.

#' Fit model
#'
#' Fits model for agricultural conversion...
#'
#' @param data Data for modeling. Output of \code{format_data()} function.
#' @param aic_step Perform aic stepwise model selection. Default is \code{TRUE}.
#'
#' @return Model of "glm" class.
#' @export
#'
#' @examples None.
fit_model <- function(data, aic_step = TRUE)
{
  model_fitting_data <- data[["training_data"]]
  # Fit model
  warning("Fitting model... PA weighting still needs to be implemented!")

  # Set presence absence weight - equal weighting scheme
  P_proportion <- sum(model_fitting_data$PA == 1) / length(model_fitting_data$PA)
  A_proportion <- sum(model_fitting_data$PA == 0) / length(model_fitting_data$PA)
  weights <- ifelse(model_fitting_data$PA == 1, P_proportion, A_proportion)
  #
  glm_model <- glm(PA ~ . , data = model_fitting_data, family = binomial,
                   weights = weights, control = glm.control(epsilon = 1e-08, maxit = 50, trace = FALSE))
  #
  if (aic_step)
  {
    glm_model <- step(glm_model)
    return(glm_model)
  } else {
    return(glm_model)

  }

}


# model_fitting_data <- data[["training_data"]]


# table(model_fitting_data$PA)[1] / length(model_fitting_data$PA)

# length(which(model_fitting_data$PA == 1))

# length(which(model_fitting_data$PA == 0))


# biomod_options <- BIOMOD_ModelingOptions(GLM = list(type = "simple",
#                                                     interaction.level = 0,
#                                                     myFormula = NULL,
#                                                     test = "AIC",
#                                                     family = binomial(link = "logit"),
#                                                     mustart = 0.5,
#                                                     control = glm.control(epsilon = 1e-08, maxit = 50, trace = FALSE)))
