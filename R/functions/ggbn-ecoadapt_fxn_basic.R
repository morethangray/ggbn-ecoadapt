# updated: 2022-12-01 ----
# ========================================================== -----
# LOAD LIBRARIES ----
# File management 
# library(fs)   ## To manage directories
# library(here)   ## To read from and write to the correct folder
# library(openxlsx)   ## To work with xlsx files
# library(readxl)   ## To read xlsx files
# library(xlsx)   ## To work with xlsx files

# Munge
# library(collapse)   ## For advanced data frame manipulation
# library(data.table)   ## We use the data.table way to wrangle data in this text
# library(forcats)   ## For working with factor variables
# library(furrr)   ## Combines tidyverse and parallel packages
# library(fuzzyjoin)   ## To join inexact matches
library(janitor)   ## To clean data tables
# library(lubridate)   ## To work with dates and times
# library(magrittr)   ## Instead of nesting functions, we use the pipe operator from this package
# library(stringr)   ## To wrangle character variables
# library(tidyverse)   ## To manipulate data frames

# Graphing 
# library(cowplot)   ## To combine plots
# library(dabestr)   ## To make several plot types
# library(ggforce)   ## For improved jitter plots
# library(ggplot2)   ## For plotting
library(ggpubr)   ## For predefined ggplot layouts
# library(ggradar)   ## To create a radar chart
# library(ggsci)   ## For color palettes
# library(ggsignif)   ## To add significance results to plots
# library(ggthemes)   ## For the colorblind palette
# library(lazyWeave)   ## For pretty p-values
# library(lemon)   ## To manipulate faceted plots
# library(patchwork)   ## To manipulate multi-panel plots
# library(viridis)   ## For the viridis color palette

# Tabling
# library(kableExtra)   ## To improve kable tables
# library(knitr)   ## To make kable tables


# ========================================================== -----
# DEFINE FILE PATHS ----
# For input data tables ----
path_in <-  here("input")
path_lookup <- here(path_in, "lookup-tables")
path_raw <- here(path_in, "data-raw")
path_derived <- here(path_in, "data-derived")

# For output, excluding initial set of derived data  ----
path_out <- here("output/")
path_prep <- here(path_out, "0_prep-data")
path_explore <- here(path_out, "1_exploration")
path_summarize <- here(path_out, "2_summarize")
path_analysis_1 <- here(path_out, "3_analysis-1")
path_analysis_2 <- here(path_out, "4_analysis-2")

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
  select(column_name, 
         # n_column, 
         metric, 
         variable,
         time_end, 
         scenario) %>%
  unite(scenario_variable, c(scenario, variable), remove = FALSE) 
# ========================================================== -----
