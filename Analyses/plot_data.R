##%######################################################%##
#                                                          #
####    Script to explore model's evaluation metrics    ####
#                                                          #
##%######################################################%##

# This script loads the finished agricultural models, cleans the data and prepare it
# for plotting and further analysis
# Mirza Cengic | 02.03.2018 | mirzaceng@gmail.com

# Script setup ------------------------------------------------------------

pacman::p_load(raster, dplyr, Rahat, fs, stringr, purrr, tidyr, readr, tidyverse, ggthemr, lubridate)

# Load data ---------------------------------------------------------------
# Load shapefile with IMAGE region names
IMAGE_rgn <- shapefile("Y:/Mirza_Cengic/Projects/Land_use/Data/Shapefile/IMAGE_regions/IMAGE_regions.shp")
image_regions <- tolower(unique(IMAGE_rgn$Rgn_name))

# Load csv files with finished evaluation files
# Files in Land_use_output contain model projections, model assessment,
# model coefficients and variable importance data
assessment_files_raw <- "Land_use_models" %>%
  milkunize("m5") %>%
  list.files(recursive = TRUE, pattern = "Model_assessment.*.csv$", full.names = TRUE) %>%
  map(read_csv) %>%
  reduce(rbind)

# Clean the loaded files -- add column "CV" which indicates whether the model is cross-validated or not,
# column "Region", column "Category_num" with numerical category descriptions (coded as chr), and Category_char,
# with description in strings
assessment_files <- assessment_files_raw %>%
  mutate(
    CV = ifelse(str_detect(Model_ID, "cv"), TRUE, FALSE),
    Region = Model_ID %>% str_replace("_cv", "") %>%
      str_sub(1, nchar(.) - 3),
    Category_num = Model_ID %>%  str_replace("_cv", "") %>%
      str_sub(nchar(.) - 1, nchar(.)),
    Category_char = case_when(
      Category_num == "10" ~ "Rainfed crops",
      Category_num == "20" ~ "Irrigated crops",
      Category_num == "30" ~ "Mosaic crops\n<50% vegetation",
      Category_num == "40" ~ "Mosaic crops\n>50% vegetation"))

# Get region mean and stdev
region_stats <- assessment_files %>%
  group_by(Region, Var) %>%
  summarize(
    region_mean = mean(Value),
    stdev = sd(Value)) %>%
  ungroup()

# Calculate stats per category
my_theme <- ggthemr("greyscale", set_theme = FALSE)$theme



assessment_files %>%
  group_by(Category_num, Var) %>%
  summarize(
    cat_mean = mean(Value),
    stdev = sd(Value)) %>%
  ungroup() %>%
  ggplot(aes(x = Category_num, y = cat_mean)) + geom_col() +
  facet_grid(.~Var) + my_theme

##
# Plot backcasted TSS


# Plotting ----------------------------------------------------------------
# Plot region stats
# region_stats %>% #str
#   mutate(
#     Region = as.factor(Region)
#   ) %>%
#   ggplot(aes(x = Region, y = region_mean)) + geom_point()# +
#   facet_grid(. ~ Var)
#
#
#   eval_files %>%
#     mutate(
#       Region = as.factor(Region)
#     ) %>%
#     ggplot(aes(x = Region, y = Value)) + geom_boxplot()# +
#   facet_grid(. ~ Var)
#


# TSS plot --------
# Create TSS plot to compare model TSS values between cross-validated and hind-casted models
p_TSS_cv <- assessment_files %>%
  filter(Var == "TSS") %>%
  mutate(
    CV2 = ifelse(CV == TRUE, "Cross-validated", "Hind-casted")
  ) %>%

  ggplot(aes(x = Category_char, y = Value, #group = Category2,
             fill = CV2)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype  = "dashed", color = "grey20", size = 1) +
  my_theme +
  labs(y = "TSS value", x = NULL, title = NULL) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.line.y = element_line(color = "grey50", size = 0.1),
        axis.text.y = element_text(
          # margin = margin(r = 0, l = 1),
          size = 14),
        axis.title.y = element_text(size = 20, margin = margin(t = 0, r = 20, b = 0, l = 0)),#, margin = margin(l = 0.1)),
        axis.text.x = element_text(size = 22),
        plot.margin = unit(rep(30, 4), "pt"),
        plot.title = element_text(margin = margin(b = 3), size = 16),
        plot.subtitle = element_text(margin = margin(t = 2, b = 5), size = 12),
        legend.text = element_text(size = 22, color = "grey10"),
        # legend.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.direction = "horizontal", legend.position = "top",
        legend.key.size = unit(2.7, "line")) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), name = "Evaluation method") +
  scale_y_continuous(#sec.axis = sec_axis(~., name = derive()),
    expand = c(0.05, 0),
    breaks = seq(0, 1, by = 0.2), limits = c(0, 1))
p_TSS_cv

ggsave(milkunize(paste0("Projects/Land_use/R/Output/Plots/Manuscript/TSS_boxplots_",
                        today(), ".png")),
       plot = p_TSS_cv, width = 12, height = 8, dpi = 600)


# Plot
eval_files %>%
  ggplot(aes(Category_char, Value, fill = CV)) +
  geom_boxplot() +
  # facet_wrap(~ Var + CV) +
  my_theme +
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), name = "Evaluation\nmethod")


# AUC plot ----------------------------------------------------------------

p_AUC_cv <- eval_files %>%
  filter(Var == "AUC") %>%
  mutate(
    CV2 = ifelse(CV == TRUE, "Cross-validated", "Hind-casted")
  ) %>%

  ggplot(aes(x = Category_char, y = Value, #group = Category2,
             # fill = CV2
  )) +
  geom_boxplot() + facet_wrap(~ CV2) +
  geom_hline(yintercept = 0.5, linetype  = "dashed", color = "grey20", size = 1) +
  my_theme +
  labs(y = "AUC value", x = NULL, title = NULL) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.line.y = element_line(color = "grey50", size = 0.1),
        axis.text.y = element_text(
          # margin = margin(r = 0, l = 1),
          size = 14),
        strip.text = element_text(size = 22),
        axis.title.y = element_text(size = 20, margin = margin(t = 0, r = 20, b = 0, l = 0)),#, margin = margin(l = 0.1)),
        axis.text.x = element_text(size = 18),
        plot.margin = unit(rep(30, 4), "pt"),
        plot.title = element_text(margin = margin(b = 3), size = 16),
        plot.subtitle = element_text(margin = margin(t = 2, b = 5), size = 12),
        legend.text = element_text(size = 22, color = "grey10"),
        # legend.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.direction = "horizontal", legend.position = "top",
        legend.key.size = unit(2.7, "line")) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), name = "Evaluation method") +
  scale_y_continuous(#sec.axis = sec_axis(~., name = derive()),
    expand = c(0.05, 0),
    breaks = seq(0, 1, by = 0.2), limits = c(0, 1))
p_AUC_cv

ggsave(milkunize(paste0("Projects/Land_use/R/Output/Plots/Manuscript/AUC_boxplots_",
                        today(), ".png")),
       plot = p_AUC_cv, width = 16, height = 8, dpi = 600)



# VIF values --------------------------------------------------------------

vif_values_raw <- "Projects/Land_use/R/Output/VIF/Below_threshold_files" %>%
  milkunize() %>%
  list.files(pattern = "csv$", full.names = TRUE) %>%
  map(read_csv) %>%
  reduce(rbind)

vif_values <- vif_values_raw %>%
  transmute(
    Variable = case_when(
      Var == "Soil_PH_norm" ~ "Soil pH",
      Var == "CHELSA_bio01_norm" ~ "Mean temperature",
      Var == "CHELSA_bio12_norm" ~ "Precipitation",
      Var == "Soil_AWCh_norm" ~ "Soil available water capacity",
      Var == "Urban_distance_norm" ~ "Distance from urban areas",
      Var == "Merit_slope_norm" ~ "Slope",
      Var == "CHELSA_bio15_norm" ~ "Precipitation seasonality",
      Var == "Roads_distance_norm" ~ "Distance from roads",
      Var == "TWI_norm" ~ "Topographical Wetness Index",
      Var == "Soil_ORC_norm" ~ "Soil organic carbon content",
      Var == "CHELSA_bio4_norm" ~ "Temperature seasonality",
      Var == "Human_population_ltr_norm" ~ "Population density",
      Var == "Merit_DEM_norm" ~ "Elevation",
      Var == "Soil_CEC_norm" ~ "Soil cation exchange capacity",
      Var == "Soil_CLY_norm" ~ "Soil clay content",
      Var == "Soil_SLT_norm" ~ "Soil silt content",
      Var == "Soil_SND_norm" ~ "Soil sand content",
      Var == "Protected_areas_catg1" ~ "Protected areas",
      Var == "Merit_northness_norm" ~ "Northness"),
    VIF = round(Vals, 2),
    Region = str_replace(Region, "_", " ")) %>%
  spread(Region, VIF)

write.csv(vif_values, milkunize(paste0("Projects/Land_use/R/Output/VIF/VIF_values_regions.csv")), row.names = FALSE)





#
r10 = raster("/vol/milkun1/Mirza_Cengic/Projects/Land_use/R/Output/Mosaics/mosaic_10.tif")
r30 = raster("/vol/milkun1/Mirza_Cengic/Projects/Land_use/R/Output/Mosaics/mosaic_30.tif")
r40 = raster("/vol/milkun1/Mirza_Cengic/Projects/Land_use/R/Output/Mosaics/mosaic_40.tif")

png(filename = milkunize(paste0("Projects/Land_use/R/Output/Maps/Projection_rainfed.png")),
    width = 480, height = 480)
plot(r10, main = "Suitability values for \nconversion to rainfed agriculture")
dev.off()


png(filename = milkunize(paste0("Projects/Land_use/R/Output/Maps/Projection_irrigated_mollweide.png")),
    width = 480, height = 480)
plot(mosaic10, main = "Suitability values for \nconversion to irrigated agriculture")
dev.off()


png(filename = milkunize(paste0("Projects/Land_use/R/Output/Maps/Projection_30.png")),
    width = 480, height = 480)
plot(r30, main = "Suitability values for \nconversion to mosaic crops\n<50% vegetation agriculture")
dev.off()


png(filename = milkunize(paste0("Projects/Land_use/R/Output/Maps/Projection_40.png")),
    width = 480, height = 480)
plot(r40, main = "Suitability values for \nconversion to mosaic crops\n>50% vegetation agriculture")
dev.off()



# Variable importance -----------------------------------------------------

var_importance_raw <- "Land_use_models" %>%
  milkunize("m5") %>%
  list.files(recursive = TRUE, pattern = "Variable_importance.*.csv$", full.names = TRUE) %>%
  map(read_csv) %>%
  reduce(bind_rows)

var_importance <- var_importance_raw %>%
mutate(
  CV = ifelse(str_detect(Model_ID, "cv"), TRUE, FALSE),
  Region = Model_ID %>% str_replace("_cv", "") %>%
    str_sub(1, nchar(.) - 3),
  Category_num = Model_ID %>%  str_replace("_cv", "") %>%
    str_sub(nchar(.) - 1, nchar(.)),
  Category_char = case_when(
    Category_num == "10" ~ "Homogeneous crops",
    Category_num == "20" ~ "Irrigated crops",
    Category_num == "30" ~ "Mosaic crops\n<50% vegetation",
    Category_num == "40" ~ "Mosaic crops\n>50% vegetation"))

# Plot
var_importance %>%
  filter(CV == FALSE) %>%
  ggplot(aes(x = Variable, Importance_scaled_100)) +
  geom_boxplot() +
  coord_flip() +
  facet_grid(~ Category_num)


my_theme <- ggthemr("greyscale", set_theme = FALSE)$theme

####
var_imp_cleaned <- var_importance %>%
  filter(CV == FALSE) %>%
  group_by(Category_num, Variable) %>%
  mutate(
    mean_val = mean(Importance_scaled_100),
    sd_val = sd(Importance_scaled_100),
    Variable_type = case_when(
      str_detect(Variable, "CHELSA") ~ "Climate",
      str_detect(Variable, "Soil") ~ "Soil",
      str_detect(Variable, "Merit|TWI") ~ "Topography",
      str_detect(Variable, "Protected|distance|population") ~ "Anthropogenic",
      TRUE ~ "Previous_LC"
    )
  ) %>%
  ungroup() %>%
  group_by(Category_num, Variable_type) %>%
  mutate(
    mean_type = mean(Importance_scaled_100),
    sd_type = sd(Importance_scaled_100)) %>%
  ungroup()



aa %>%
ggplot(aes(x = Variable_type, y = mean_type, ymin = mean_type - sd_type, ymax = mean_type + sd_type,
           fill = Variable_type)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_errorbar(position = position_dodge(width = 0.9), color = "grey50") +
  coord_flip() +
  my_theme +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  facet_grid(~ Category_num)


# Variable type barplot (with aggregated variable types)
variable_type_barplot <- var_imp_cleaned %>%
  ggplot(aes(x = Variable_type, y = mean_type,
             fill = Variable_type)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  coord_flip() +
  labs(fill = "Variable type", x = NULL, y = NULL) +
  my_theme +
  theme(
    strip.text = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.text = element_text(size = 12)
  ) +
  facet_grid(~ Category_char)

variable_type_barplot

ggsave(filename = milkunize(paste0("Projects/Land_use/R/Output/Plots/Manuscript/variable_type_barplot_", today(), ".png")),
       plot = variable_type_barplot,
       width = 12, height = 10)


# Barplot with all variables (colored by variable type)



barplot_all <- var_imp_cleaned %>%
  # select(Variable_type, mean_type, Category_num)
  ggplot(aes(x = Variable, y = mean_val,
             # ymin = mean_val - sd_val, ymax = mean_val + sd_val,
             fill = Variable_type)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  # geom_errorbar(position = position_dodge(width = 0.9), color = "grey50") +
  coord_flip() +
  labs(fill = "Variable type", x = NULL, y = NULL) +
  my_theme +
  theme(
    strip.text = element_text(size = 14),
    axis.text = element_text(size = 13),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.text = element_text(size = 14)) +
  facet_grid(~ Category_char)

barplot_all

ggsave(filename = milkunize(paste0("Projects/Land_use/R/Output/Plots/Manuscript/barplot_all_", today(), ".png")),
       plot = barplot_all, width = 14, height = 10)
