# updated: 2022-12-05 ----
# ========================================================== -----
# CONFIGURE SETTINGS ----
rm(list = ls())
library(tidyverse)   ## To manipulate data frames
library(here)   ## To manage directories

# Source functions 
path_fxn <- here("R/functions")
source(file = here(path_fxn, "ggbn-ecoadapt_fxn_basic.R"))
# source(file = here(path_fxn, "ggbn-ecoadapt_fxn_bcm.R"))
# source(file = here(path_fxn, "ggbn-ecoadapt_plot-settings.R"))

# ========================================================== -----
# CREATE DATA FRAMES ----

# ========================================================== -----
# READ SPATIAL RASTERS ----
# Confirm extent, coordinates ----
# (From ArcGIS Pro properties)
# coordinate system: NAD_1983_2011_California_Teale_Albers
#
# projected coordinate system: NAD 1983 (2011) California (Teale) Albers (Meters)
# projection: Albers
# WKID: 6414 (USE THIS for sp::CRS())
# authority: EPSG

# geographic coordinate system: NAD 1983 (2011)
# WKID: 6318 (DO NOT use this for sp::CRS())
# authority: EPSG
# angular unit: degree
# datum: D NAD 1983 2011
# spheroid: GRS 1980

# Read spatial raster files ----
# detach("package:raster", unload = TRUE)
list_tif <- list.files(path_raw_spatial)

# lookup_epsg <- as_tibble(rgdal::make_EPSG())
# lookup_epsg[grep("Albers", lookup_epsg$note),]

abcd <- here(path_raw_spatial, list_tif[17])
raster::raster(abcd)
#
#   [NOT RUN] fxn_raster_to_point ----
# Cannot execute this function because of an error in raster::raster()
# index_list <- list_tif
# index_name <- index_list[17]
# index_path <- path_raw_spatial
# 
# library(sp)
# library(rgdal)
# fxn_raster_to_point <- function(index_path, index_list){
# 
#   # Load libraries 
#   library(raster)
#   # the raster library masks dplyr::select()
# 
#   datalist <- list()
# 
#   for(index_name in index_list){
# 
#     # Create helpers  
#     index_variable <- str_sub(index_name, 6, 8)
#     index_metric <- str_sub(index_name, 10, 12)
#     index_scenario <- str_sub(index_name, 1, 4)
#     index_crs <- sp::CRS("+init=epsg:6414")
#     index_tif <- here(index_path, index_name)
# 
#     # Read tif as raster  
#     raster <- raster::raster(x = index_tif)
# 
#     # Error in (function (classes, fdef, mtable)  :
#     #             unable to find an inherited method for function ‘trim’ for signature ‘"character"’
# 
#     # Extract raster values as points 
#     points <-
#       rasterToPoints(raster) %>%
#       # Include the row ID column
#       as_tibble(rownames = "cell_id")
# 
#     # Detatch libraries 
#     detach("package:raster", unload = TRUE)
# 
#     # Reshape and annotate data  
#     # Rename the value column
#     colnames(points) <- c("cell_id",
#                           "x",
#                           "y",
#                           "value")
# 
# 
#     datalist[[index_name]] <-
#       points %>%
#       # Set values <0 to NA
#       mutate(value = ifelse(value < 0, NA, value),
#              # Annotate with variable, metric, scenario
#              variable = index_variable,
#              metric = index_metric,
#              variable_metric = paste(variable, metric, sep = "_"),
#              scenario = index_scenario) %>%
#       relocate(cell_id,
#                variable_metric,
#                variable,
#                metric,
#                scenario,
#                value)
# 
#   }
#   bind_datalist <- do.call(bind_rows, datalist)
# 
# }

test_1 <- 
  fxn_raster_to_point(index_path = path_raw_spatial, 
                      index_list = list_tif[1])

# ========================================================== -----
# HEADING ----
# ========================================================== -----
# HEADING ----
# ========================================================== -----
# GRAVEYARD ----
# ---------------------------------------------------------- -----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  -----
# ---------------------------------------------------------- -----
