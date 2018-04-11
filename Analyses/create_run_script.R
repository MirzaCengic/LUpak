# Function to create script that will source and run biomod code for LC
# Script name is hardcoded, so are SLURM parameters

create_script <- function(region, category, path)
{
  # Create slurm header
  line1 <- paste0("#!/usr/bin/env Rscript

#SBATCH --partition=milkunshort
#SBATCH --mail-type=FAIL
#SBATCH --mail-user=mirzaceng@gmail.com
#SBATCH --time=6:00:00
",
paste0("#SBATCH --output /vol/milkun1/Mirza_Cengic/Projects/Land_use/R/Output/Logs/Model_fitting_final_run/",
                         region, "_", category, ".log"),
"
#SBATCH --mem=20G\n
library(Rahat)")

  # Create lines where values of regions and categories would be assigned to variables
  line2 <- paste0("region_name <- ", paste0('"', region, '"'))
  line3 <- paste0("category_no <- ", paste0('"', category, '"'))

  # Set path of script to be sourced
  line4 <- 'script_to_source <- milkunize("Projects/Other/Personal_R_package/LUpak/Analyses/run_model.R")'
  line5 <- 'source(script_to_source)'

  script_name <- paste0(path, "/", tolower(region), "_", category, ".R")


  # Create script containing the above lines
  fileConn<-file(script_name, "wb")
  writeLines(c(line1, line2, line3, line4, line5), fileConn)
  close(fileConn)
}


cats <- c(10, 30, 40)
library(Rahat)
library(sf)
image_regions <- st_read(milkunize("Projects/Land_use/Data/Shapefile/IMAGE_regions/IMAGE_regions.shp"))
regions <- image_regions$Rgn_name

for (region in regions) {
  for (category in cats)
  {
    create_script(region, category = category, path = milkunize("Projects/Land_use/R/Scripts/Modeling/Run_final/"))
  }
}
