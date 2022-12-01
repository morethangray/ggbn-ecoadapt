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
#   lookup_variables  -----
lookup_variables <- 
  read_csv(here(path_lookup, "bcm-variables.csv")) %>%
  select(column_name, 
         # n_column, 
         metric, 
         variable,
         time_end, 
         scenario)

#   bcm_raw ----
# Values by point, within study area extent 
# Derived from rasters at 270m resolution 
bcm_raw <- 
  read_csv(here(path_raw, "bcm_raster-to-point_wide.csv")) %>%
  clean_names() %>%
  gather(column_name, value, ccsm4_avetemp:hist_tmxjja) %>%
  select(-object_id) %>%
  # Annotate with metric, variable, subset, time interval, bcm scenario
  left_join(lookup_variables, "column_name") %>%
  arrange(metric,
          variable,
          desc(time_end), 
          scenario) %>%
  select(point_id, 
         metric, 
         variable, 
         time_end, 
         scenario, 
         column_name,
         value)  

#   bcm_raw_wide: Reduce file size and write csv ----
bcm_raw_wide <- 
  bcm_raw %>%
  unite(scenario_variable, c(scenario, variable)) %>%
  select(point_id, 
         scenario_variable, 
         value) %>%
spread(scenario_variable, value) 

bcm_raw_wide %>%
  write_csv(here(path_derived, "bcm_raster-to-point_wide.csv"))

# NOTE: Some bcm_raw cells are empty ---- 
# Null values in col 4, 7, 13, 16, 17, 20 (e.g., row 6211, 8401)
# ccsm4_cwda
# ccsm4_runrch
# cnrm_runrch
# cnrmd_cwda
# hadg_cwda
# hadg_runrch

# ========================================================== -----
# EVALUATE FUTURE CHANGE  ----
# Calculate future minimum and change from historic ----
#   bcm_future_change ----
# For each point: calculate the minimum value of each BCM variable, by time period
#   This finds the minimum among the three future scenarios 
#   Finding the "minimum" does not change the historic data; there is only one value per point
bcm_future_change <- 
  bcm_raw %>%
  group_by(metric,
           variable, 
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
           metric, 
           variable, 
           historic, 
           future_minimum = future, 
           future_minimum_difference)

bcm_future_change %>%
  write_csv(here(path_derived, "bcm_minimum-future-change.csv"))
# ---------------------------------------------------------- -----
# START WORKING HERE -----
# ---------------------------------------------------------- -----
# Compare abundance among future scenarios ----
# Use the difference between the historic and future minimum as input 
# Count the number of points within a series of 0.025 C bins  
# Abundance is the percent of total points within each bin 

# CONFIRM: 92785 points ----
#   Subset to temperature and future_min_diff ----
input_percent <- 
  bcm_future_min_diff %>%
  filter(metric %in% "temp") %>%
  select(variable, 
         value = future_min_diff, 
         point_id)
#   Prepare bins for grouping ----
# Get max and min for temperature metrics to define bins 
temp_min <- floor(min(input_percent$value))
temp_max <- ceiling(max(input_percent$value))

# Create sequence with an interval of 0.025
seq_025 <- seq(from = temp_min, to = temp_max, by = 0.025)

bins_025 <- 
  input_percent %>%
  # Use the sequence to bin the values for each variable 
  mutate(bin = cut(value, seq_025, include.lowest = TRUE)) %>%
  arrange(bin) %>%
  distinct(bin)

#   Determine count in each bin ----
# Calculate the total number of points by variable  (for the denominator)
temp_total <- max(input_percent$point_id)

bcm_future_min_diff_count <- 
  input_percent %>%
  # Use the sequence to bin the values for each variable 
  mutate(bin = cut(value, seq_025, include.lowest = TRUE)) %>%
  # Count the values for each bin by variable (for the numerator)
  group_by(variable, bin) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  # Create all combinations to identify missing values
  spread(variable, count) %>%
  gather(variable, count, avg:jja) %>%
  # Replace NA with 0
  mutate(count = ifelse(is.na(count), 0, count)) %>%
  spread(variable, count) 

# bcm_future_min_diff_count %>%
#   write_csv(here(path_derived, "bcm_minimum-change_count.csv"))
#   Determine abundance (% total points) in each bin ----
bcm_future_min_diff_count_percent <- 
  bcm_future_min_diff_count %>%
  gather(variable, count, avg:jja) %>%
  # Calculate percent by bin
  mutate(percent = count/temp_total) %>%
  select(-count) %>%
  spread(variable, percent) 

# bcm_future_min_diff_count_percent %>%
#   write_csv(here(path_derived, "bcm_minimum-change_count-percent.csv"))


# ========================================================== -----
# HEADING ----
# ========================================================== -----
# HEADING ----
# ========================================================== -----
# HEADING ----
# ========================================================== -----
# HEADING ----
# ========================================================== -----
# GRAVEYARD ----
# ---------------------------------------------------------- -----

