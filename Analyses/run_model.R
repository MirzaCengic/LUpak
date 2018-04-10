##%######################################################%##
#                                                          #
####             Main script to run models              ####
#                                                          #
##%######################################################%##
# Load packages


# Script setup ------------------------------------------------------------


pacman::p_load(raster, sp, sf, caret, Rahat,
               tidyr, tictoc, PresenceAbsence, tidyverse, LUpak)


# region_name <- "Korea_region"
# category_no <- "10"

# Set in which folder is the input data
path_backup <- "LU_data/Changes_v666/"


# Main function with error catching  --------------------------------------

# Function returns different error levels for regions
# Set variable values ####
# Set model ID


# run_model <- function(region_name, category_no, path_backup)
# {
model_id <- tolower(str_c(region_name, "_", category_no))
## Set processing folder and folder for evaluation output for each region
base_dir_path <- milkunize("Land_use_output_new/", "m5")

proc_folder <- str_c(base_dir_path, region_name)
eval_folder <- str_c(proc_folder, "/Eval")

dir_create(proc_folder)
dir_create(eval_folder)

raster_outname <- paste0(proc_folder, "/", model_id, ".tif")
print(raster_outname)
if (file.exists(raster_outname))
{
  print("Raster file finished")

} else {

  output_file <- paste0(milkunize("Projects/Land_use/R/Output/VIF/Below_threshold_files/"), region_name, "_variables.csv")

  # Here function returns VIF error if file with VIF selected variables exist, run script, else fail.

  cat("Reading data", "\n")
  vif_rasters <- read.csv(output_file, stringsAsFactors = FALSE)


  # Load data for model fitting ####
  # Load raster data
  raster_data <- get_rasters(region = region_name)
  # Force raster processing to byblock by lowering the maxmemory parameter
  rasterOptions(maxmemory = ncell(raster_data) - 1)

  # Use previous land cover layers for fit and evaluation data
  raster_to_subset_fit <- c(vif_rasters$Var, "Protected_areas_catg", "Previous_land_cover_fit_catg")
  raster_to_subset_eval <- c(vif_rasters$Var, "Protected_areas_catg", "Previous_land_cover_eval_catg")

  raster_data_fit <- raster::subset(raster_data, raster_to_subset_fit)
  raster_data_eval <- raster::subset(raster_data, raster_to_subset_eval)

  # Load presences/absences (argument "type" defines if the modeling data is for model fitting or evaluation)
  PA_data <- load_PA(region = region_name, category = category_no, type = "Fit", path = path_backup)
  PA_data_eval <- load_PA(region = region_name, category = category_no, type = "Eval", path = path_backup)


  ###########################
  #### Hind casted model ####
  ###########################
  cat("Preparing data - hind casted model", "\n")
  modeling_data <- format_data(explanatory_variables = raster_data_fit, response_variable = PA_data, evaluation_data = PA_data_eval,
                               VIF_select = FALSE, cross_validate = FALSE, explanatory_variables_eval = raster_data_eval)

  # Fit model with stepwise AIC selection
  my_model <- fit_model(modeling_data)

  # Get evaluations for the model (coefficients and assessment) and write them to disk
  # evaluate_model(my_model, modeling_data)
  get_evaluations(fitted_model = my_model, data = modeling_data, ID = model_id, output_folder = eval_folder)

  # Variable importance calculation should be still implemented.
  # Check what the data argument should be. Now I use modeling_data, which is extracted data from the response variable, not the full set
  # Maybe a fix should be added so a raster stack is accepted instead.!
  variable_importance(data = modeling_data$training_data, model = my_model, clean = TRUE)

  # Harmonize layer names
  names(raster_data_fit) <- str_replace(names(raster_data_fit), "_fit_catg", "_catg")


  cat("Predicting...", "\n")
  predicted_model <- raster::predict(raster_data_fit, my_model, na.rm = TRUE, type = "response", progress = "text")

  predicted_model <- predicted_model * 1000
  cat("Saving raster", "\n")
  writeRaster(predicted_model, raster_outname, dataType = "INT2U", options = "COMPRESS=LZW", overwrite = TRUE)

  ###############################
  #### Cross-validated model ####
  ###############################
  cat("Preparing data - cross validated model", "\n")
  modeling_data_cv <- format_data(explanatory_variables = raster_data_fit, response_variable = PA_data, evaluation_data = PA_data_eval,
                                  VIF_select = FALSE, cross_validate = TRUE)

  # Fit model with stepwise AIC selection
  my_model_cv <- fit_model(modeling_data_cv)

  # Get evaluations for the model (coefficients and assessment) and write them to disk...
  # evaluate_model(my_model_cv, modeling_data_cv)
  get_evaluations(fitted_model = my_model_cv, data = modeling_data_cv, ID = paste0(model_id, "_cv"), output_folder = eval_folder)

  # Save image

  # image_path <- paste0(proc_folder, "/", model_id, ".RData")
  # cat("Saving data", "\n")
  # save.image(image_path)
}
