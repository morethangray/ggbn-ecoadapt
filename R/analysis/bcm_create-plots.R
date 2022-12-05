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
# FUTURE CHANGE BY VARIABLE (AVERAGE OF 3 SCENARIOS) ----
# Create plot showing average of scenarios  
#   variable_bins -----
variable_bins <- read_csv(here(path_derived, "future-minimum_variable-average_bin-0.025_tmp-ppt.csv"))
# Temperature ----
variable_bins %>%
  filter(variable %in% "tmp") %>%
  fxn_plot_abundance_by_variable(index_path = path_bcm_plot)

# Precipitation ----
variable_bins %>%
  filter(variable %in% "ppt") %>%
  fxn_plot_abundance_by_variable(index_path = path_bcm_plot)

# ========================================================== -----
# FUTURE CHANGE BY VARIABLE_METRIC (BY SCENARIO) ----
# Create plots showing abundance by scenario  
#   variable_metric_bins ----- 
variable_metric_bins <- 
  read_csv(here(path_derived, "future-minimum_variable-by-scenario_bin-0.025_all.csv"))


# Temperature ----
#   Single plots ----
variable_metric_bins %>%
  fxn_plot_abundance_by_variable_metric(index_variable_metric = "tmp_avg", index_path = path_bcm_plot)
variable_metric_bins %>%
  fxn_plot_abundance_by_variable_metric(index_variable_metric = "tmp_djf", index_path = path_bcm_plot)
variable_metric_bins %>%
  fxn_plot_abundance_by_variable_metric(index_variable_metric = "tmp_jja", index_path = path_bcm_plot)
#   Stacked plot ----
variable_metric_bins %>%
  fxn_plot_abundance_by_variable_metric_facet(index_variable = "tmp", index_path = path_bcm_plot)

# Precipitation  ----
#   Single plots ----
variable_metric_bins %>%
  fxn_plot_abundance_by_variable_metric(index_variable_metric = "ppt_djf", index_path = path_bcm_plot)
variable_metric_bins %>%
  fxn_plot_abundance_by_variable_metric(index_variable_metric = "ppt_jja", index_path = path_bcm_plot)
#   Stacked plot ----
variable_metric_bins %>%
  fxn_plot_abundance_by_variable_metric_facet(index_variable = "ppt", index_path = path_bcm_plot)

# Climatic water deficit  ----
variable_metric_bins %>%
  fxn_plot_abundance_by_variable_metric(index_variable_metric = "cwd_avg", index_path = path_bcm_plot)

# Recharge and runoff  ----
variable_metric_bins %>%
  fxn_plot_abundance_by_variable_metric(index_variable_metric = "rnr_avg", index_path = path_bcm_plot)


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

