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
#   bcm_tidy  ----
bcm_tidy <- 
  read_csv(here(path_derived, "bcm_raster-to-point_wide.csv")) %>%
  gather(scenario_variable_metric, value, 2:29) %>%
  # Annotate with metric, variable, subset, time interval, bcm scenario
  left_join(lookup_variables, "scenario_variable_metric") 



# ========================================================== -----
# SUMMARY STATISTICS  ----
bcm_tidy %>%
  drop_na(value) %>%
  group_by(variable, metric, time_end, scenario) %>%
  summarize(mean = mean(value, na.rm = TRUE), 
            median = median(value, na.rm = TRUE),
            min = min(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE),
            count = n())  %>%
  write_csv(here(path_summarize, "bcm_summary-statistics.csv"))
# ========================================================== -----
# HEADING ----
# ========================================================== -----
# HEADING ----
# ========================================================== -----
# ========================================================== -----
# GRAVEYARD ----
# ---------------------------------------------------------- -----
# START WORKING HERE -----
# ---------------------------------------------------------- -----

