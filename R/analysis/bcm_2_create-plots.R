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
source(file = here(path_fxn, "ggbn-ecoadapt_plot-settings.R"))

# ========================================================== -----
# CREATE DATA FRAMES ----
#   variable_bins -----
# For plot showing average of scenarios  
variable_bins <- 
  read_csv(here(path_derived, "future-minimum_variable_bin-0.025_tmp-ppt.csv"))  
# arrange(variable_metric, bin_from) %>%
# mutate_if(is.character, as_factor)
#
#   variable_metric_bins ----- 
# For plots showing abundance by scenario  
variable_metric_bins <- 
  read_csv(here(path_derived, "future-minimum_variable-metric_bin-0.025_all.csv")) 
# arrange(scenario, variable_metric, bin_from) %>%
# mutate_if(is.character, as_factor)
#
# ========================================================== -----
# FUTURE CHANGE BY VARIABLE (AVERAGE OF 3 SCENARIOS) ----
# Create plot showing average of scenarios  
# Plots are automatically saved to index_path with today's date
# Temperature ----
variable_bins %>%
  fxn_plot_abundance_by_variable(index_variable = "tmp",
                                 index_path = path_bcm_plot, 
                                 hide_legend = FALSE)

# Precipitation ----
variable_bins %>%
  fxn_plot_abundance_by_variable(index_variable = "ppt",
                                 index_path = path_bcm_plot, 
                                 hide_legend = FALSE)

# ========================================================== -----
# FUTURE CHANGE BY VARIABLE_METRIC (BY SCENARIO) ----
# Create plots showing abundance by scenario  
# Plots are automatically saved to index_path with today's date
# Temperature ----
#   Single plots ----
variable_metric_bins %>%
  fxn_plot_abundance_by_variable_metric(index_variable_metric = "tmp_avg", 
                                        index_path = path_bcm_plot, 
                                        hide_legend = FALSE)
variable_metric_bins %>%
  fxn_plot_abundance_by_variable_metric(index_variable_metric = "tmp_djf", 
                                        index_path = path_bcm_plot, 
                                        hide_legend = FALSE)
variable_metric_bins %>%
  fxn_plot_abundance_by_variable_metric(index_variable_metric = "tmp_jja", 
                                        index_path = path_bcm_plot, 
                                        hide_legend = FALSE)
#   Stacked plot ----
variable_metric_bins %>%
  fxn_plot_abundance_by_variable_metric_facet(index_variable = "tmp",
                                              index_path = path_bcm_plot, 
                                              hide_legend = TRUE)

# Precipitation  ----
#   Single plots ----
variable_metric_bins %>%
  fxn_plot_abundance_by_variable_metric(index_variable_metric = "ppt_djf", 
                                        index_path = path_bcm_plot, 
                                        hide_legend = FALSE)
variable_metric_bins %>%
  fxn_plot_abundance_by_variable_metric(index_variable_metric = "ppt_jja", 
                                        index_path = path_bcm_plot, 
                                        hide_legend = FALSE)
#   Stacked plot ----
variable_metric_bins %>%
  fxn_plot_abundance_by_variable_metric_facet(index_variable = "ppt", 
                                              index_path = path_bcm_plot, 
                                              hide_legend = TRUE)
# [NOT RUN] ------------------------------------------------ -----
# Climatic water deficit  ----
# variable_metric_bins %>%
#   fxn_plot_abundance_by_variable_metric(index_variable_metric = "cwd_avg", 
#                                         index_path = path_bcm_plot, 
#                                         hide_legend = FALSE)
# 
# Recharge  ----
# variable_metric_bins %>%
#   fxn_plot_abundance_by_variable_metric(index_variable_metric = "rch_avg",
#                                         index_path = path_bcm_plot, 
#                                         hide_legend = FALSE)
# 
# Runoff  ----
# variable_metric_bins %>%
#   fxn_plot_abundance_by_variable_metric(index_variable_metric = "run_avg",
#                                         index_path = path_bcm_plot, 
#                                         hide_legend = FALSE)


# ========================================================== -----
# GRAVEYARD ----
