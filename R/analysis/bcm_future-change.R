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


# ========================================================== -----
# MINIMUM FUTURE CHANGE ----
input_data <- read_csv(here(path_in, "lu_pts_210301_TableToExcel.csv"))

input_data %>%
  mutate(CWD_MIN = pmin(CWD_2069_CNRM, 
                        CWD_2069_GFDL, 
                        CWD_2069_MIROC),
         CWD_DIFF_MIN = CWD_MIN-CWD_2010) %>%
  select(OBJECTID, pointid, CWD_MIN, CWD_DIFF_MIN) %>%
  write_csv(here(path_gis, "lu_pts_210301_cwd-min.csv"))

# ========================================================== -----
# HEADING ----
# ========================================================== -----
# HEADING ----
# ========================================================== -----
# ========================================================== -----
# GRAVEYARD ----
# ---------------------------------------------------------- -----

