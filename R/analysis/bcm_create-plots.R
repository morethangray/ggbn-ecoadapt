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
# CREATE DATA FRAMES  -----
# Values by point, within landscape unit extent (LU)
#   bcm_input ----
bcm_input <- read_csv(here(path_derived, "bcm_raster-to-point_long.csv"))
bcm_future_change <- read_csv(here(path_derived, "bcm_minimum-future-change.csv"))
# ========================================================== -----
# FUTURE CHANGE  ----
# Compare abundance among future scenarios
# Use the difference between the historic and future minimum as input 
# Count the number of points within a series of 0.025 C bins  
# Abundance is the percent of total points within each bin 
bcm_future_min_diff_count 
bcm_future_min_diff_count_percent 
# Create plots ----
# bcm_future_min_diff_count_percent %>%
  input_percent %>%
  # Use the sequence to bin the values for each metric 
  mutate(bin = cut(value, seq_025, include.lowest = TRUE)) %>%
  mutate(n_bin = word(bin, 1, sep = "\\,"), 
         n_bin = as.numeric(str_remove_all(n_bin, "\\("))) %>%
  # arrange(value) %>%
  # mutate(value = as_factor(value)) %>%
  ggdensity(x = "n_bin",
            color = "metric")  

# Create plots showing abundance by scenario ----
# for AVG, JJA, DJF, CWD, PPT (DJF, JJA), RCH, RUN (or RNR?)
# Create plot showing average of scenarios ----
# for TMP (AVG, JJA, DJF), PPT (DJF, JJA), RNR (RCH, RUN)
# ========================================================== -----
# Other plots  ----
# Density plot by variable
join_compare %>%
  filter(metric %in% "temp") %>%
  # filter(check > 0.5 | check < -0.5) %>%
  select(variable, 
         point_id, 
         future_min_diff, 
         future_min_diff_orig)  %>%
  gather(calculation, value, future_min_diff:future_min_diff_orig) %>%
  ggdensity(x = "value",
            color = "variable") +
  facet_wrap(~calculation)

# Histogram bu variable
join_compare %>%
  filter(metric %in% "temp") %>%
  filter(check > 0.5 | check < -0.5) %>%
  select(variable, 
         point_id, 
         check) %>%
  gghistogram(x = "check",
              color = "variable")  + 
  facet_wrap(~variable)


# FOR REFERENCE: 
# ========================================================== -----
# HEADING ----
# ========================================================== -----
# HEADING ----
# ========================================================== -----
# HEADING ----
# ========================================================== -----
# ========================================================== -----
# GRAVEYARD ----
# ---------------------------------------------------------- -----

