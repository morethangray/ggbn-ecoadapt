# updated: 2022-12-01 ----
# ========================================================== -----
# CONFIGURE SETTINGS ----
rm(list = ls())
library(tidyverse)   ## To manipulate data frames
library(here)   ## To manage directories

# Source functions 
path_fxn <- here("R/functions")
source(file = here(path_fxn, "ggbn-ecoadapt_fxn_basic.R"))

# ========================================================== -----
# CREATE DATA FRAMES ----
# [NOT RUN] Read and tidy raw data ----
# # Values by point, within study area extent 
# # Derived from rasters at 270m resolution 
# bcm_raw <- 
#   read_csv(here(path_raw, "bcm_raster-to-point_wide.csv")) %>%
#   clean_names() %>%
#   gather(column_name, value, ccsm4_avetemp:hist_tmxjja) %>%
#   select(-object_id) %>%
#   # Annotate with metric, variable, subset, time interval, bcm scenario
#   left_join(lookup_variables, "column_name") %>%
#   arrange(variable_metric,
#           desc(time_end), 
#           scenario) %>%
#   select(point_id, 
#          variable_metric, 
#          variable, 
#          metric,
#          time_end, 
#          scenario, 
#          scenario_variable_metric,
#          value) 
# 
# bcm_raw %>%
#   write_csv(here(path_derived, "bcm_raster-to-point_long.csv"))
#   
# # NOTE: Some cells are empty 
# # Null values in col 4, 7, 13, 16, 17, 20 (e.g., row 6211, 8401)
# #   ccsm4_cwda
# #   ccsm4_runrch
# #   cnrm_runrch
# #   cnrmd_cwda
# #   hadg_cwda
# #   hadg_runrch
# 
# Reduce file size and write csv 
# bcm_raw %>%
#   select(point_id, 
#          scenario_variable_metric, 
#          value) %>%
#   spread(scenario_variable_metric, value) %>%
#   write_csv(here(path_derived, "bcm_raster-to-point_wide.csv"))
# 
#   bcm_tidy  ----
# Read wide csv and reshape to long 
bcm_tidy <- 
  read_csv(here(path_derived, "bcm_raster-to-point_wide.csv")) %>%
  gather(scenario_variable_metric, value, 2:27) %>%
  # Annotate with metric, variable, subset, time interval, bcm scenario
  left_join(lookup_variables, "scenario_variable_metric") 

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
#   bcm_change_variable ----
bcm_change_variable <- 
  bcm_tidy %>%
  group_by(variable_metric, 
           variable,
           metric,
           time_end, # Separates historic from future values
           point_id) %>%
  # Some points lack data for cwd so min will be infinite
  drop_na(value) %>%
  # Find the minimum value at each point
  summarize(value_minimum = min(value, na.rm = TRUE)) %>%
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

bcm_change_variable %>%
  write_csv(here(path_derived, "bcm_future-minimum-change_variable-average.csv"))


# Determine variable average across all scenarios  ----
#   Use the difference between the historic and future minimum as input 
#   Bin values and count the number of points per bin
#   Abundance is the percent of total points within each bin 
#   Study area comprised of 92785 points (= total points)

# bcm_change_variable <-
#   read_csv(here(path_derived, "bcm_future-minimum-change_variable-average.csv"))

abundance_025_tmp <- 
  fxn_abundance_by_variable(index_data = bcm_future_change, 
                            index_variable = "tmp",
                            index_bin_size = 0.025) %>%
  write_csv(here(path_bcm, "future-minimum_bin-0.025_variable-average_tmp.csv"))
                    

abundance_025_ppt <- 
  fxn_abundance_by_variable(index_data = bcm_future_change, 
                            index_variable = "ppt",
                            index_bin_size = 0.025) %>%
  write_csv(here(path_bcm, "future-minimum_bin-0.025_variable-average_ppt.csv"))
                   

abundance_025_rnr <- 
  fxn_abundance_by_variable(index_data = bcm_future_change, 
                            index_variable = "rnr",
                            index_bin_size = 0.025) %>%
  write_csv(here(path_bcm, "future-minimum_bin-0.025_variable-average_rnr.csv"))
                    

abundance_025_cwd <- 
  fxn_abundance_by_variable(index_data = bcm_future_change, 
                            index_variable = "cwd",
                            index_bin_size = 0.025) %>%
  write_csv(here(path_bcm, "future-minimum_bin-0.025_variable-average_cwd.csv"))
                   


# ========================================================== -----
# EVALUATE FUTURE CHANGE: BY VARIABLE_METRIC ----
# Determine abundance by scenario for each variable_metric ----
# ---------------------------------------------------------- -----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  -----
# ---------------------------------------------------------- -----

# list_variable_metric <- unique(lookup_variables$variable_metric)
#   fxn_abundance_by_variable_metric ----
index_data = bcm_future_change
index_variable = "ppt"
index_bin_size = 0.025

fxn_abundance_by_variable_metric <- function(index_data, index_variable, index_bin_size){
  
  # Subset input data to variable_metric of interest ----
  subset <- 
    index_data %>%
    filter(variable %in% index_variable) %>%
    # Exclude NA values 
    drop_na(future_minimum_difference) %>%
    select(point_id, 
           variable_metric, 
           variable, 
           metric,
           value = future_minimum_difference)  
  
  # Create a helper to reshape columns ----
  n_column <- 
    lookup_variables %>%
    filter(variable %in% index_variable) %>%
    distinct(metric) %>%
    nrow() + 2
  
  # Prepare bins for grouping ----
  # Get max and min to define bins 
  value_min <- floor(min(subset$value))
  value_max <- ceiling(max(subset$value))
  
  # Create sequence with an interval of 0.025
  interval_sequence <- seq(from = value_min, to = value_max, by = index_bin_size)
  
  # Create annotation for bins
  bin_annotation <- 
    subset %>%
    # Use the sequence to bin the values for each variable 
    mutate(bin = cut(value, interval_sequence, include.lowest = TRUE)) %>%
    arrange(bin) %>%
    distinct(bin) %>%
    mutate(bin_from = word(bin, 1, sep = "\\,"), 
           bin_from = as.numeric(str_remove_all(bin_from, "\\(")), 
           bin_to = word(bin, 2, sep = "\\,"), 
           bin_to = as.numeric(str_remove_all(bin_to, "\\]")), 
           n_bin = 1:n()) 
  
  # Determine abundance (% total points) in each bin ----
  abundance <- 
    subset %>%
    # Use the sequence to bin the values for each variable 
    mutate(bin = cut(value, interval_sequence, include.lowest = TRUE)) %>%
    # Count the values for each bin by variable (for the numerator)
    group_by(variable_metric,
             variable, 
             bin) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    # Create all combinations to identify missing values
    spread(variable_metric, count) %>%
    gather(variable_metric, count, 3:all_of(n_column)) %>%
    # Replace NA with 0
    mutate(count = ifelse(is.na(count), 0, count), 
           # Calculate percent by bin
           percent = count/n_points, 
           bin_size = index_bin_size) %>%
    select(-count) %>%
    spread(variable_metric, percent) %>%
    # Join annotation for bins
    left_join(bin_annotation, "bin") %>%
    relocate(n_bin, 
             bin, 
             bin_from,
             bin_to, 
             bin_size,
             variable)
}

# ========================================================== -----
# HEADING ----
# ========================================================== -----
# HEADING ----
# ========================================================== -----
# GRAVEYARD ----
# ---------------------------------------------------------- -----
# START WORKING HERE -----
# ---------------------------------------------------------- -----
