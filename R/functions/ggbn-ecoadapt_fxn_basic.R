# updated: 2022-12-05  ----
# ========================================================== -----
# LOAD LIBRARIES ----
# File management 
library(readxl)   ## To read xlsx files

# Munge
# library(collapse)   ## For advanced data frame manipulation
# library(forcats)   ## For working with factor variables
# library(fuzzyjoin)   ## To join inexact matches
library(janitor)   ## To clean data tables
# library(lubridate)   ## To work with dates and times

# Graphing 
library(ggpubr)   ## For predefined ggplot layouts
# library(lemon)   ## To manipulate faceted plots
# library(patchwork)   ## To manipulate multi-panel plots

# Spatial
# library(rgdal)   ## Provides bindings to the 'Geospatial' Data Abstraction Library (GDAL)
# library(raster)   ## Reading, writing, manipulating, analyzing and modeling of spatial data
# NOTE: rgdal() and raster() mask dplyr functions; do not load library globally

# Tabling
# library(kableExtra)   ## To improve kable tables
# library(knitr)   ## To make kable tables

# ========================================================== -----
# DEFINE FILE PATHS ----
# For input data tables ----
path_in <-  here("input")
path_lookup <- here(path_in, "lookup-tables")
path_raw <- here(path_in, "data-raw")
path_raw_spatial <- here(path_raw, "spatial")
path_derived <- here(path_in, "data-derived")

# For output, excluding initial set of derived data  ----
path_out <- here("output/")
path_prep <- here(path_out, "0_prep-data")
path_explore <- here(path_out, "1_exploration")
path_summarize <- here(path_out, "2_summarize")
path_bcm <- here(path_out, "3_bcm")
path_bcm_plot <- here(path_bcm, "plots")
path_fire <- here(path_out, "4_fire")

# For markdown work ----
path_r <- here("R")
# path_markdown <- here(path_r, "markdown")
# path_fxn <- here(path_r, "functions")

# For final report or manuscript ----
path_report <- here("report")
path_figures <- here(path_report, "figures")
path_tables <- here(path_report, "tables")

# ========================================================== -----
# DEFINE FUNCTIONS ----
# Basic functions ----
#   %nin% ----
"%nin%" <- Negate("%in%")
#   spread_n ----
spread_n <- function(df, key, value) {
  # quote key
  keyq <- rlang::enquo(key)
  # break value vector into quotes
  valueq <- rlang::enquo(value)
  s <- rlang::quos(!!valueq)
  df %>% gather(variable, value, !!!s) %>%
    unite(temp, !!keyq, variable) %>%
    spread(temp, value)
}
#   is.Date ----
is.Date <- function(x) {
  inherits(x, c("Date", "POSIXt"))}
#   substrRight ----
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
#   fxn_digit ----
fxn_digit <- function(x){
  as.numeric(format(round(x, 3), nsmall = 3))
}
#   fxn_ci95 ----
fxn_ci95 <- function(df, index_value){
  df %>%
    rename(value = all_of(index_value)) %>%
    mutate(mean = mean(value, na.rm = TRUE), 
           sd = sd(value, na.rm = TRUE), 
           count = n(), 
           margin = (qt(0.975, df = count - 1))*(sd/sqrt(count)), 
           lci = mean - margin, 
           uci = mean + margin) %>%
    select(-margin, -sd, -count)
}
#   round_any ----
round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy}

#   fxn_kable ----
fxn_kable <- function(df){
  df  %>%
    knitr::kable() %>%
    kable_styling(bootstrap_options = c("striped", "hover"), 
                  full_width = F,  
                  position = "left", 
                  fixed_thead = T)
}

# GGBN-EcoAdapt functions ----
# ========================================================== -----
# CREATE HELPERS ----
#   lookup_variables  -----
lookup_variables <- 
  read_csv(here(path_lookup, "bcm-variables.csv")) %>%
  dplyr::select(column_name, 
                variable_metric,
                variable,
                metric,
                time_end, 
                scenario) %>%
  unite(scenario_variable_metric, c(scenario, variable, metric), remove = FALSE) 


#   lookup_labels_all  -----
lookup_labels_all <- 
  read_csv(here(path_lookup, "bcm-variables.csv")) %>%
  select(variable_metric,
         variable,
         metric,
         units,
         time_end, 
         scenario, 
         starts_with("lab"))  
  

#   lookup_labels_variable  -----
lookup_labels_variable <- 
  lookup_labels_all %>%
  distinct(variable,
           metric,
           units,
           lab_variable, 
           lab_metric)  
  

#   lookup_labels_scenario -----
lookup_labels_scenario <- 
  lookup_labels_all %>%
  distinct(time_end, 
           scenario, 
           lab_time, 
           lab_scenario)  
  

#   n_points ----

n_points <- 92785

#   list_variable ----
list_variable <- unique(lookup_variables$variable)
#   list_variable_metric ----
list_variable_metric <- unique(lookup_variables$variable_metric)
# ========================================================== -----
