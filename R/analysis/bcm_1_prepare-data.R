# updated: 2022-12-05 ----
# ========================================================== -----
# CONFIGURE SETTINGS ----
rm(list = ls())
library(tidyverse)   ## To manipulate data frames
library(here)   ## To manage directories

# Source functions 
path_fxn <- here("R/functions")
source(file = here(path_fxn, "ggbn-ecoadapt_fxn_basic.R"))
source(file = here(path_fxn, "ggbn-ecoadapt_fxn_bcm.R"))
# source(file = here(path_fxn, "ggbn-ecoadapt_plot-settings.R"))

# ========================================================== -----
# CREATE DATA FRAMES ----
# [NOT RUN] Read and tidy raw data ----
# # Values by point, within study area extent
# # Derived from rasters at 270m resolution
# # Read data table 
# bcm_raw <-
#   read_xlsx(here(path_raw, "bcm_raster-to-point.xlsx")) %>%
#   clean_names() %>%
#   select(-objectid) %>%
#   rename(point_id = pointid) 
# 
# # Reshape to long format 
# ncol_bcm_raw <- ncol(bcm_raw)
# 
# bcm_raw %>%
#   gather(column_name, value, 2:all_of(ncol_bcm_raw)) %>%
#   # Annotate with metric, variable, subset, time interval, bcm scenario
#   left_join(lookup_variables, "column_name") %>%
#   arrange(variable_metric,
#           desc(time_end),
#           scenario) %>%
#   select(point_id,
#          scenario_variable_metric,
#          variable_metric,
#          variable,
#          metric,
#          time_end,
#          scenario,
#          value) %>%
#   write_csv(here(path_derived, "bcm_raster-to-point_long.csv"))

# Reduce file size and write csv
# bcm_raw %>%
#   select(point_id,
#          scenario_variable_metric,
#          value) %>%
#   spread(scenario_variable_metric, value) %>%
#   write_csv(here(path_derived, "bcm_raster-to-point_wide.csv"))

#   bcm_tidy  ----
bcm_tidy <- read_csv(here(path_derived, "bcm_raster-to-point_long.csv")) 


# ========================================================== -----
# EVALUATE FUTURE CHANGE: VARIABLE AVERAGE----
# Calculate future minimum and average change from historic ----
# Used for variables with at least two metrics (jja, djf, avg)
#   TMP: AVG, JJA, DJF
#   PPT: DJF, JJA
#   RNR: RCH, RUN ?? Unsure if there are two metrics or one
#   CWD: Same as by-scenario because there is only one subset
# At each point, calculate the minimum value among the three futures
#   Finding the "minimum" does not change the historic data; there is only one value per point
#
# unique(lookup_variables$variable)
#
#   bcm_variable_change ----
bcm_variable_change <- 
  bcm_tidy %>%
  # Exclude points that lack data 
  drop_na(value) %>%
  group_by(point_id,
           variable_metric, 
           variable,
           metric,
           # Include time_end to separate historic, future values
           time_end) %>%  
  # Find the minimum value at each point among the three futures
  summarize(value_minimum = min(value, na.rm = TRUE)) %>%
  ungroup() %>%
  # Find the difference between the historic and future minimums
  spread(time_end, value_minimum) %>%
  mutate(future_minimum_difference = future - historic) %>%
  relocate(point_id, 
           variable_metric, 
           variable,
           metric, 
           historic, 
           future_minimum = future, 
           future_minimum_difference)

bcm_variable_change %>%
  write_csv(here(path_derived, "future-minimum_variable_change_all.csv"))

 
# Determine variable average (by bin) across all scenarios  ----
#   Use the difference between the historic and future minimum as input 
#   Bin values and count the number of points per bin
#   Abundance is the percent of total points within each bin 
#   Study area comprised of 92785 points (= total points)

# bcm_variable_change <-
#   read_csv(here(path_derived, "future-minimum_variable_change_all.csv"))

variable_bins_tmp <- 
  fxn_bin_by_variable(index_data = bcm_variable_change, 
                      index_variable = "tmp",
                      index_bin_size = 0.025) 

variable_bins_ppt <- 
  fxn_bin_by_variable(index_data = bcm_variable_change, 
                      index_variable = "ppt",
                      index_bin_size = 0.025) 
#   variable_bins ----
variable_bins <- 
  bind_rows(variable_bins_tmp, 
            variable_bins_ppt) %>%
  write_csv(here(path_derived, "future-minimum_variable_bin-0.025_tmp-ppt.csv"))

# Calculate abundance by bin ----
variable_bins_abundance_tmp <-
  fxn_abundance_by_variable(index_data = variable_bins_tmp)  

variable_bins_abundance_ppt <-
  fxn_abundance_by_variable(index_data = variable_bins_ppt) 

#   variable_bins_abundance ----
variable_bins_abundance <- 
  bind_rows(variable_bins_abundance_tmp, 
            variable_bins_abundance_ppt) %>%
  write_csv(here(path_derived, "future-minimum_variable_bin-0.025_abundance_tmp-ppt.csv"))

# ========================================================== -----
# EVALUATE FUTURE CHANGE: BY VARIABLE_METRIC ----
# Calculate future minimum, change from historic by scenario ----
#   bcm_variable_metric_change ----
bcm_variable_metric_change <- 
  bcm_tidy %>% 
  # Exclude points that lack data 
  drop_na(value) %>%
  select(point_id, 
         variable_metric,
         scenario,
         value) %>%
  spread(scenario, value)   %>%
  # Find the difference between the values for historic, each future
  mutate(ccsm_difference = ccsm - hist, 
         cnrm_difference = cnrm - hist, 
         hadg_difference = hadg - hist) %>%
  mutate(variable = str_sub(variable_metric, 1, 3), 
         metric = str_sub(variable_metric, 5, 7))

bcm_variable_metric_change %>%
  write_csv(here(path_derived, "future-minimum_variable-metric_change_all.csv"))

# Determine variable_metric abundance (by bin) by scenario ----
# bcm_variable_metric_change <-
#   read_csv(here(path_derived, "future-minimum_variable-metric_change.csv"))

#   variable_metric_bins  -----
variable_metric_bins <- 
  fxn_bin_by_variable_metric(index_data = bcm_variable_metric_change, 
                             index_list <- list_variable_metric,
                             index_bin_size <- 0.025) %>%
  write_csv(here(path_derived, "future-minimum_variable-metric_bin-0.025_all.csv"))


# Calculate abundance by bin ----
#   variable_metric_bins_abundance ---- 
variable_metric_bins_abundance <- 
  fxn_abundance_by_variable_metric(index_data = variable_metric_bins, 
                                   index_list <- list_variable_metric) %>%
  write_csv(here(path_derived, "future-minimum_variable-metric_bin-0.025_abundance_all.csv"))
# ========================================================== -----
# GRAVEYARD ----
# ---------------------------------------------------------- -----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  -----
# ---------------------------------------------------------- -----
