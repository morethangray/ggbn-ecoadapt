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
bcm_tidy <- read_csv(here(path_derived, "bcm_raster-to-point_long.csv")) 

# 
# ========================================================== -----
# CHECK CONTENTS  ----

bcm_tidy %>%
  filter(is.na(value)) %>%
  group_by(variable, metric, scenario) %>%
  count() %>%
  ungroup()
  
# The raster files for CWD, RCH, and RUN had 29 empty cells in each future 

# ========================================================== -----
# SUMMARY STATISTICS  ----
bcm_tidy %>%
  filter(variable %in% c("tmp", "ppt")) %>%
  drop_na(value) %>%
  group_by(variable, metric, scenario) %>%
  summarize(mean = mean(value, na.rm = TRUE), 
            median = median(value, na.rm = TRUE),
            min = min(value, na.rm = TRUE),
            y_max = max(value, na.rm = TRUE),
            z_count = n())  %>%
  gather(statistic, value, mean:z_count) %>%
  spread(scenario, value) %>%
  mutate(statistic = str_remove_all(statistic, "y_"), 
         statistic = str_remove_all(statistic, "z_")) %>%
  write_csv(here(path_summarize, 
                 paste0("bcm_summary-statistics_",
                        Sys.Date(),
                        ".csv")))
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

