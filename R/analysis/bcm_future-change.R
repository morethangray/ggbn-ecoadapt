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

points <- 
  read_xlsx(here("cache/county_pts_TableToExcel.xlsx"), 
            sheet = "county_pts_TableToExcel") %>%
  clean_names()  %>%
  mutate(op_id = paste(str_pad(objectid, width = 5, side = "left", pad = "0"),
                       str_pad(pointid, width = 5, side = "left", pad = "0"), 
                       sep = "_")) %>%
  select(-grid_code) %>%
  gather(metric, value, avg_2010:fir_2069_gfdl) %>%
  mutate(metric = str_replace_all(metric, "fire", "fir"), 
         metric = str_replace_all(metric, "miroc", "miro"), 
         metric = str_replace_all(metric, "2010", "2010_hist"), 
         metric = str_replace_all(metric, "hist_gfdl", "hist")) %>%
  mutate(variable = str_sub(metric, 1, 3), 
         scenario = str_sub(metric, 10, 14)) %>%
  relocate(op_id, variable, scenario, metric, value) 

list_variables <- 
  points %>%
  distinct(variable) %>%
  filter(variable != "fir") %>%
  pull()


# Create function ----
fxn_midcentury_min <- function(){
  
  datalist_bind_minimum <- list()
  for(i in list_variables){
    
    subset <- 
      points %>%
      filter(variable %in% i) %>% 
      select(op_id, scenario, variable, value) %>%
      spread(scenario, value) 
    
   minimum <- 
      subset %>% 
      # Calculate difference by scenario: Subtract each future value from 2010 value
      mutate(c = cnrm - hist, 
             g = gfdl - hist, 
             m = miro - hist, 
             # Calculate minimum difference for three scenarios
             min = pmin(c, g, m, na.rm = TRUE)) %>%
      select(op_id, min)  
   
   colnames(minimum) <- c("op_id", paste0(i, "_diff_min"))
    
    datalist_bind_minimum[[i]] <- minimum
  }

  n_variables <- length(list_variables) + 1
  
  bind_minimum <- 
    do.call(bind_rows, datalist_bind_minimum) %>%
    gather(column, values, 2:all_of(n_variables)) %>%
    drop_na(values) %>%
    spread(column, values)
 
}

bind_minimum <- fxn_midcentury_min()

points_revised <- 
  points %>%
  select(op_id, metric, value) %>%
  spread(metric, value) %>%
  left_join(bind_minimum, "op_id") %>%
  gather(column, value, 2:38) %>%
  spread(column, value) %>%
  mutate(objectid = as.numeric(str_sub(op_id, 1, 5)), 
         pointid = as.numeric(str_sub(op_id, 7, 11))) %>%
  relocate(objectid, pointid) %>%
  select(-op_id)

points_revised %>%
  write_csv(here("analysis/difference-by-2069/county-points_with-difference.csv"))
