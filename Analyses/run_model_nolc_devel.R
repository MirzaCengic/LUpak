##%######################################################%##
#                                                          #
####             Main script to run models              ####
#                                                          #
##%######################################################%##
# Load packages


# Script setup ------------------------------------------------------------

pacman::p_load(raster, sp, sf, caret, Rahat,
               tidyr, tictoc, PresenceAbsence, tidyverse, LUpak, fs)

# region_name <- "Korea_region"
# category_no <- "10"

# Set in which folder is the input data
changes_path <- "Projects/Land_use/Data/Agri_changes/" %>%
  milkunize2("archive")

# Main function with error catching  --------------------------------------

# Function returns different error levels for regions
# Set variable values ####
# Set model ID


# run_model <- function(region_name, category_no, path_backup)
# {
model_id <- tolower(str_c(region_name, "_", category_no))

cat(paste0("\n======================================\n",
           "#### Model run ", model_id, " ####\n",
           "======================================\n\n"))
## Set processing folder and folder for evaluation output for each region
base_dir_path <- milkunize2("Projects/Land_use/Output/Model_runs_VIF/", "archive")

proc_folder <- str_c(base_dir_path, region_name)
eval_folder <- str_c(proc_folder, "/Eval")

dir.create(proc_folder, mode = "777", recursive = TRUE)
dir.create(eval_folder, mode = "777", recursive = TRUE)

raster_outname <- paste0(proc_folder, "/", model_id, ".tif")

print(raster_outname)

if (file.exists(raster_outname))
{
  print("Raster file finished")

} else {

  output_file <- paste0(milkunize2("Projects/Land_use/R/Output/VIF/Below_threshold_files/"), region_name, "_variables.csv")

  # Here function returns VIF error if file with VIF selected variables exist, run script, else fail.

  cat("Reading data...", "\n")
  vif_rasters <- read.csv(output_file, stringsAsFactors = FALSE)


  # Load data for model fitting ####
  # Load raster data
  raster_data_raw <- get_rasters(region = region_name)
  raster_data <- raster_data_raw
  if (Sys.info()["sysname"][[1]] != "Windows")
  {
    temp_dir <- "/scratch/R_lu_tmpdir"
    dir.create(temp_dir, mode = "777", recursive = TRUE)
    rasterOptions(tmpdir = temp_dir)
  }

  # Force raster processing to byblock by lowering the maxmemory parameter
  rasterOptions(maxmemory = ncell(raster_data) - 1)

  # Use previous land cover layers for fit and evaluation data
  # raster_to_subset_fit <- c(vif_rasters$Var, "Protected_areas_catg")
  # raster_to_subset_eval <- c(vif_rasters$Var, "Protected_areas_catg")

  # Subset explanatory variables to VIF ones + categorical protected areas predictor
  raster_data <- raster::subset(raster_data, c(vif_rasters$Var, "Protected_areas_catg"))
  # raster_data_eval <- raster::subset(raster_data, raster_to_subset_eval)

  # Load presences/absences (argument "type" defines if the modeling data is for model fitting or evaluation)
  PA_data <- load_PA(region = region_name, category = category_no, type = "Fit", path = changes_path)
  PA_data_eval <- load_PA(region = region_name, category = category_no, type = "Eval", path = changes_path)


  ###########################
  #### Hind casted model ####
  ###########################
  # cat("", "\n")
  tic("Preparing data - hind casted model...")
  modeling_data <- format_data(explanatory_variables = raster_data, response_variable = PA_data, evaluation_data = PA_data_eval,
                               VIF_select = FALSE, cross_validate = FALSE, explanatory_variables_eval = raster_data)
  toc()
  # Fit model with stepwise AIC selection, without presence/absence weighting
  my_model <- fit_model(modeling_data, weights = FALSE, aic_step = T)

  # Write model outputs -----------------------------------------------------
  # Calculate variable importances
  var_imp_raw <- variable_importance(data = modeling_data$evaluation_data, model = my_model,
                                     clean = TRUE)
  var_imp <- var_imp_raw %>%
    transmute(
      Variable,
      Importance = Iter_1,
      Importance_scaled_100 = (Iter_1 / sum(Iter_1)) * 100,
      Model_ID = model_id)

  # Calculate model assessment
  # evaluate_model(my_model, modeling_data)
  model_assessment <- get_evaluations(fitted_model = my_model, data = modeling_data, ID = model_id)
  model_coefficients <- clean_coeffs(model = my_model, r_data = raster_data_raw, v_rasters = vif_rasters, m_assessment = model_assessment)

  ############ !! ################


  ###########################################
  # Set file paths
  variable_imp_filename <- paste0(eval_folder, "/Variable_importance_", model_id, ".csv")
  model_assessmet_filename <- paste0(eval_folder, "/Model_assessment_", model_id, ".csv")
  model_coeffs_filename <- paste0(eval_folder, "/Model_coefficients_", model_id, ".csv")
  # Save
  write.csv(var_imp, variable_imp_filename, row.names = FALSE)
  write.csv(model_assessment$model_evaluation, model_assessmet_filename, row.names = FALSE)
  write.csv(model_coefficients, model_coeffs_filename, row.names = FALSE)


  # Make categorical rasters as factor




  # cat("Predicting...", "\n")
  tic("Predicting...")
  predicted_model <- raster::predict(raster_data, my_model, na.rm = TRUE, type = "response", progress = "text")
  toc()

  cat("Saving raster...", "\n")
  writeRaster(predicted_model, raster_outname, options = "COMPRESS=LZW", overwrite = TRUE)

  ###############################
  #### Cross-validated model ####
  ###############################
  cat("Preparing data - cross validated model", "\n")
  modeling_data_cv <- format_data(explanatory_variables = raster_data, response_variable = PA_data, evaluation_data = PA_data_eval,
                                  VIF_select = FALSE, cross_validate = TRUE)

  # Fit model with stepwise AIC selection
  my_model_cv <- fit_model(modeling_data_cv, weights = FALSE)

  # Write model outputs (cross validated model) -----------------------------------------------------
  # Calculate variable importances
  var_imp_cv_raw <- variable_importance(data = modeling_data_cv$evaluation_data, model = my_model_cv,
                                        clean = TRUE)
  var_imp_cv <- var_imp_cv_raw %>%
    transmute(
      Variable,
      Importance = Iter_1,
      Importance_scaled_100 = (Iter_1 / sum(Iter_1)) * 100,
      Model_ID = paste0(model_id, "_cv"))

  # Calculate model assessment
  # evaluate_model(my_model, modeling_data)
  model_assessment_cv <- get_evaluations(fitted_model = my_model_cv, data = modeling_data_cv, ID = paste0(model_id, "_cv"))
  model_coefficients_cv <- clean_coeffs(model = my_model_cv, r_data = raster_data_raw, v_rasters = vif_rasters,
                                        m_assessment = model_assessment_cv)

  # Set file paths
  variable_imp_cv_filename <- paste0(eval_folder, "/Variable_importance_", paste0(model_id, "_cv"), ".csv")
  model_assessmet_cv_filename <- paste0(eval_folder, "/Model_assessment_", paste0(model_id, "_cv"), ".csv")
  model_coeffs_cv_filename <- paste0(eval_folder, "/Model_coefficients_", paste0(model_id, "_cv"), ".csv")
  # Save
  write.csv(var_imp_cv, variable_imp_cv_filename, row.names = FALSE)
  write.csv(model_assessment_cv$model_evaluation, model_assessmet_cv_filename, row.names = FALSE)
  write.csv(model_coefficients_cv, model_coeffs_cv_filename, row.names = FALSE)

  # tic("Predicting...")
  # # predicted_model_cv <- raster::predict(raster_data, my_model_cv, na.rm = TRUE, type = "response", progress = "text")
  # toc()

  # raster_outname_cv <- paste0(proc_folder, "/", model_id, "_cv.tif")

  cat("Saving raster...", "\n")
  # writeRaster(predicted_model_cv, raster_outname_cv, options = "COMPRESS=LZW", overwrite = TRUE)

}
