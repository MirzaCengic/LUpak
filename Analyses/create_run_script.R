# Function to create script that will source and run biomod code for LC
# Script name is hardcoded, so are SLURM parameters

create_script <- function(region, category, path)
{
  # Create slurm header
  line1 <- paste0("#!/usr/bin/env Rscript

#SBATCH --partition=milkunshort
#SBATCH --mail-type=NONE
#SBATCH --mail-user=mirzaceng@gmail.com
#SBATCH --time=03:00:00
",

paste0("#SBATCH --output /vol/milkunarc/mcengic/Projects/Land_use/Output/Model_logs_VIF/New_coeffs_",
                         region, "_", category, ".log"),
"
#SBATCH --mem=16G

\n
library(Rahat)")

  # Create lines where values of regions and categories would be assigned to variables
  line2 <- paste0("region_name <- ", paste0('"', region, '"'))
  line3 <- paste0("category_no <- ", paste0('"', category, '"'))

  # Set path of script to be sourced
  line4 <- 'script_to_source <- milkunize2("Projects/Other/Personal_R_package/LUpak/Analyses/run_model_coeffs_only.R")'
  line5 <- 'source(script_to_source)'

  script_name <- paste0(path, "/", tolower(region), "_", category, ".R")


  # Create script containing the above lines
  fileConn <- file(script_name, "wb")
  writeLines(c(line1, line2, line3, line4, line5), fileConn)
  close(fileConn)
}


cats <- c(10, 30, 40)
# cats <- 10
library(Rahat)
library(sf)
library(tidyverse)
image_regions <- read_sf(milkunize2("Projects/Land_use/Data/Shapefile/IMAGE_regions/IMAGE_regions.shp", "archive"))
regions <- image_regions$Rgn_name

# regions_asia <- c("Central_Asia", "India", "Korea_region", "China_region", "Southeastern_Asia", "South_Asia")

milkunize2("tmp/Land_use/R/Scripts/Run_scripts_cluster/Run_model") %>%
  list.files(full.names = TRUE) %>%
  file.remove()



# regions <- regions[!regions %in% c("USA", "Russia_region", "Oceania")]

for (region in regions) {
  for (category in cats)
  {
    create_script(region, category = category, path = milkunize2("tmp/Land_use/R/Scripts/Run_scripts_cluster/Run_model/"))
  }
}

