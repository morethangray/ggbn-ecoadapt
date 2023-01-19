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
#   lookup_order_statistic ----
lookup_order_statistic <- tibble(statistic = c("mean", 
                                               "median", 
                                               "min", 
                                               "max", 
                                               "count"), 
                                 order = c(1, 2, 3, 4, 5))
# ========================================================== -----
# RAW VALUES  ----
#   bcm_tidy -----
bcm_tidy <- read_csv(here(path_derived, "bcm_raster-to-point_long.csv")) 

# Check values -----
bcm_tidy %>%
  filter(is.na(value)) %>%
  group_by(variable, metric, scenario) %>%
  count() %>%
  ungroup()
  
# The raster files for CWD, RCH, and RUN had 29 empty cells in each future 


# Create summary statistics table    ----
bcm_tidy %>%
  filter(variable %in% c("tmp", "ppt")) %>%
  drop_na(value) %>%
  group_by(variable, metric, scenario) %>%
  summarize(mean = mean(value, na.rm = TRUE), 
            median = median(value, na.rm = TRUE),
            min = min(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE),
            count = n())  %>%
  gather(statistic, value, mean:count) %>%
  spread(scenario, value) %>%
  left_join(lookup_order_statistic, "statistic") %>%
  arrange(variable, metric, order) %>%
  relocate(hist, .after = statistic) %>%
  mutate(comment = "raw values") %>%
  write_csv(here(path_summarize, 
                 paste0("bcm_summary-statistics_tmp-ppt_",
                        Sys.Date(),
                        ".csv")))
# ========================================================== -----
# BY VARIABLE ----
#   bcm_variable_change -----
bcm_variable_change <- read_csv(here(path_derived, "future-minimum_variable_change_all.csv"))
# Create summary statistics table: Future change   ----
bcm_variable_change %>%
  filter(variable %in% c("tmp", "ppt")) %>%
  rename(value = future_minimum_difference) %>%
  drop_na(value) %>%
  select(point_id, variable_metric, variable, metric, value) %>%
  group_by(variable, metric) %>%
  summarize(mean = mean(value, na.rm = TRUE), 
            median = median(value, na.rm = TRUE),
            min = min(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE),
            count = n())  %>%
  gather(statistic, value, mean:count) %>%
  spread(metric, value) %>%
  left_join(lookup_order_statistic, "statistic") %>%
  arrange(variable, order) %>%
  mutate(comment = "minimum future change from historic value, by variable") %>%
  write_csv(here(path_summarize, 
                 paste0("bcm_summary-statistics_future-minimum_variable_change_tmp-ppt_",
                        Sys.Date(),
                        ".csv")))
# ========================================================== -----
# BY VARIABLE_METRIC ----
#   bcm_variable_metric_change -----
bcm_variable_metric_change <- read_csv(here(path_derived, "future-minimum_variable-metric_change_all.csv"))
# Create summary statistics table: Future change   ----
bcm_variable_metric_change %>%
  filter(variable %in% c("tmp", "ppt")) %>%
  gather(column, value, ccsm_difference:hadg_difference) %>%
  drop_na(value) %>%
  mutate(scenario = str_sub(column, 1, 4)) %>%
  select(point_id, variable_metric, variable, metric, scenario, value) %>%
  group_by(variable, metric, scenario) %>%
  summarize(mean = mean(value, na.rm = TRUE), 
            median = median(value, na.rm = TRUE),
            min = min(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE),
            count = n())  %>%
  gather(statistic, value, mean:count) %>%
  spread(scenario, value) %>%
  left_join(lookup_order_statistic, "statistic") %>%
  arrange(variable, metric, order) %>%
  mutate(comment = "future change from historic value, by variable_metric") %>%
  write_csv(here(path_summarize, 
                 paste0("bcm_summary-statistics_future-minimum_variable-metric_change_tmp-ppt_",
                        Sys.Date(),
                        ".csv")))
  

# ========================================================== -----
# HEADING ----
# ========================================================== -----
# GRAVEYARD ----
# ---------------------------------------------------------- -----
# START WORKING HERE -----
# ---------------------------------------------------------- -----

