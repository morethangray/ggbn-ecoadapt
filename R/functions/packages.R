# updated: 2022-11-18 ---- 
# ========================================================== -----
# ANALYSIS ----
#   anova ----
# library(afex)   ## For classic ANOVA linear models

#   diversity ----
# library(betapart)   ## To analyze beta diversity
# library(iNEXT)   ## To measure diversity per Chao et al. 2014
# library(MeanRarity)   ## To calculate mean rarity (richness)
library(vegan)   ## To calculate ecological metrics

#   glmm ----
library(DHARMa)   ## To evaluate GLMM model performance
library(glmmTMB)   ## To create generalized linear mixed models (GLMM)
# library(MASS)   ## We will use glm.nb from this package

#   joint sdm ----
library(Hmsc)   ## For joint SDM (Hierarchical Modelling of Species Communities)

#   lm ----
# library(nlme)   ## For GLS models

#   lmm ----
library(broom.mixed)   ## Broom for mixed models
library(lme4)   ## To create linear mixed models (LMM)
# library(lmerTest)   ## For inference with linear mixed models

#   model evaluation ----
# library(car)   ## For model checking
# library(emmeans)   ## To compute modeled means and contrasts
# library(insight)   ## To learn about models
library(MuMIn)   ## For model selection
library(performance)   ## To evaluate model performance

#   occupancy ----
library(AICcmodavg)   ## For 'unmarked' model selection 
library(unmarked)   ## To model occupancy

#   spatial ----
# library(leaflet)   ## To work with Leaflet maps

#   stats ----
library(bestNormalize)   ## To identify normalizing tranformation  
library(broom)   ## To format output tables from statistical tests
library(easystats)   ## To evaluate correlation
# library(fitdistrplus)   ## To evaluate data distribution
library(outliers)   ## To detect outliers 
library(rstatix)   ## For statistical tests

#   time series ----
# library(dygraphs)   ## To create dynamic time series plots
# library(xts)   ## To work with time series classes (eXtensible Time Series)
# GRAPHING AND TABLING ----
#   plot ----
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
library(patchwork)   ## To manipulate multi-panel plots
library(viridis)   ## For the viridis color palette

#   table ----
library(kableExtra)   ## To improve kable tables
# library(knitr)   ## To make kable tables

# IMPORT AND WRANGLING ----
#   file management ----
library(exiftoolr)   ## To work with exif details from jpg files
library(fs)   ## To manage directories
library(here)   ## To read from and write to the correct folder
library(openxlsx)   ## To work with xlsx files
library(readxl)   ## To read xlsx files
# library(xlsx)   ## To work with xlsx files

#   munge ----
library(collapse)   ## For advanced data frame manipulation
# library(data.table)   ## We use the data.table way to wrangle data in this text
library(forcats)   ## For working with factor variables
library(furrr)   ## Combines tidyverse and parallel packages
library(fuzzyjoin)   ## To join inexact matches
library(janitor)   ## To clean data tables
library(lubridate)   ## To work with dates and times
# library(magrittr)   ## Instead of nesting functions, we use the pipe operator from this package
# library(stringr)   ## To wrangle character variables
library(tidyverse)   ## To manipulate data frames

#   utilities ----
# library(devtools)   ## To install packages that are not on cran
# library(tictoc)   ## To measure timing


# SHINY ----
# library(shiny)   ## To create shiny apps
# ========================================================== -----
# NOT INSTALLED ----
# install.packages("afex")
# install.packages("betapart")
# install.packages("iNEXT")
# install.packages("MeanRarity")
# install.packages("MASS")
# install.packages("nlme")
# install.packages("lmerTest")
# install.packages("car")
# install.packages("emmeans")
# install.packages("insight")
# install.packages("leaflet")
# install.packages("fitdistrplus")
# install.packages("dygraphs")
# install.packages("xts")
# install.packages("cowplot")
# install.packages("dabestr")
# install.packages("ggforce")
# install.packages("ggplot2")
# install.packages("ggradar")
# install.packages("ggsci")
# install.packages("ggsignif")
# install.packages("ggthemes")
# install.packages("lazyWeave")
# install.packages("lemon")
# install.packages("knitr")
# install.packages("xlsx")
# install.packages("data.table")
# install.packages("magrittr")
# install.packages("stringr")
# install.packages("devtools")
# install.packages("tictoc")
# install.packages("shiny")