# updated: 2022-12-01 ----
# ========================================================== -----
# CONFIGURE SETTINGS ----
rm(list = ls())
library(tidyverse)   ## To manipulate data frames
library(here)   ## To manage directories

# Source functions 
path_fxn <- here("R/functions")
source(file = here(path_fxn, "ggbn-ecoadapt_fxn_basic.R"))
source(file = here(path_fxn, "ggbn-ecoadapt_fxn_bcm.R"))
source(file = here(path_fxn, "ggbn-ecoadapt_plot-settings.R"))

# ========================================================== -----
# CREATE DATA FRAMES  -----
# Values by point, within landscape unit extent (LU)
# ========================================================== -----
# FUTURE CHANGE BY VARIABLE (AVERAGE OF 3 SCENARIOS) ----
# Create plot showing average of scenarios  
# for TMP (AVG, JJA, DJF), PPT (DJF, JJA)
bin_by_variable_tmp <- read_csv(here(path_bcm, "future-minimum_variable-average_bins-0.025_tmp.csv"))
bin_by_variable_ppt <- read_csv(here(path_bcm, "future-minimum_variable-average_bins-0.025_ppt.csv"))

bin_by_variable_tmp %>%
  fxn_plot_abundance_by_variable()

bin_by_variable_ppt %>%
  fxn_plot_abundance_by_variable()
# ---------------------------------------------------------- -----
# START WORKING HERE ----
# MISSING ALL VALUES FOR PPT DJF IN PLOT ----

# ---------------------------------------------------------- -----


# ========================================================== -----
# FUTURE CHANGE BY VARIABLE_METRIC (BY SCENARIO) ----
# Read input data ----
bcm_change_variable_metric <- 
  read_csv(here(path_derived, "future-minimum_variable-by-scenario_change.csv"))
# Determine abundance by scenario for each variable_metric ----
bin_by_variable_metric_tmp <- 
  fxn_bin_by_variable_metric(index_data = bcm_change_variable_metric, 
                             index_variable <- "tmp",
                             index_bin_size <- 0.025)

bin_by_variable_metric_ppt <- 
  fxn_bin_by_variable_metric(index_data = bcm_change_variable_metric, 
                             index_variable <- "ppt",
                             index_bin_size <- 0.025)

bin_by_variable_metric_cwd <- 
  fxn_bin_by_variable_metric(index_data = bcm_change_variable_metric, 
                             index_variable <- "cwd",
                             index_bin_size <- 0.025)

bin_by_variable_metric_rnr <- 
  fxn_bin_by_variable_metric(index_data = bcm_change_variable_metric, 
                             index_variable <- "rnr",
                             index_bin_size <- 0.025)
# Define path where plots will be saved to ----
path_bcm_plot <- here(path_bcm, "plots")

# Create plots showing abundance by scenario  ----
# Temperature 
bin_by_variable_metric_tmp %>%
  fxn_plot_abundance_by_variable_metric(index_variable_metric = "tmp_avg", index_path = path_bcm_plot)
bin_by_variable_metric_tmp %>%
  fxn_plot_abundance_by_variable_metric(index_variable_metric = "tmp_djf", index_path = path_bcm_plot)
bin_by_variable_metric_tmp %>%
  fxn_plot_abundance_by_variable_metric(index_variable_metric = "tmp_jja", index_path = path_bcm_plot)

# Precipitation 
bin_by_variable_metric_ppt %>%
  fxn_plot_abundance_by_variable_metric(index_variable_metric = "ppt_djf", index_path = path_bcm_plot)
bin_by_variable_metric_ppt %>%
  fxn_plot_abundance_by_variable_metric(index_variable_metric = "ppt_jja", index_path = path_bcm_plot)

# Climatic water deficit 
bin_by_variable_metric_cwd %>%
  fxn_plot_abundance_by_variable_metric(index_variable_metric = "cwd_avg", index_path = path_bcm_plot)

# Recharge and runoff 
bin_by_variable_metric_rnr %>%
  fxn_plot_abundance_by_variable_metric(index_variable_metric = "rnr_avg", index_path = path_bcm_plot)

# Create stacked plot showing abundance by scenario for tmp, ppt ----
bin_by_variable_metric_tmp %>%
  fxn_plot_abundance_by_variable_metric_facet()
# ========================================================== -----
# HEADING ----
# ========================================================== -----
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
# GRAVEYARD ----
# ---------------------------------------------------------- -----

