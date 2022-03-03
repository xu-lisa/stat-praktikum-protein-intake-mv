#***************************** packages ***************************************#
#* STEP 1
#* Description: 
#* This script loads in the neccesary packages, while checking if the neccesary 
#* packages are installed
#* 
#* Usage: 
#* Sourced in further steps
#* 
#* Recommended/used package versions: 
#* cowplot                  ‘1.1.1’
#* DataExplorer             ‘0.8.2’
#* data.table               ‘1.14.2’
#* tidyverse                ‘1.3.1’
#* gridExtra                ‘2.3’
#* mgcv                     ‘1.8.39’
#* pammmtools               ‘0.5.8’
#* survival                 ‘3.2.13’
#* survminer                ‘0.4.9’
#* svglite                  ‘2.1.0’
#* patchwork                ‘1.1.1’
#* extrafont                ‘0.17’
#* magrittr                 ‘2.0.2’
#* 
#* R Version: 
#* R version 4.1.1 (2021-08-10)
#******************************************************************************#
# necessary packages: 
# full list coming 
library(cowplot)
library(DataExplorer)
library(data.table)
library(tidyverse)
library(gridExtra)
library(mgcv)
library(pammtools)
library(survival)
library(survminer)
library(svglite)
library(patchwork)
library(parallel)
library(extrafont)
library(magrittr)


theme_set(theme_bw())

#  pkg <- c("cowplot", "DataExplorer", "data.table", "dplyr", "ggplot2",
# "gridExtra", "mgcv","pammtools","survival", "survminer", "svglite", 
# "tidyr", "patchwork", "purrr", "extrafont", "magrittr") # still; not all and might change most to tidy_verse 
# The loop doesnt work sometimes
# checks if installed/ installs 
# for (i in 1:length(pkg)){
#     if(require(pkg[i])){
#       paste(pkg[i],"package is loaded correctly", sep = ":")
#     } else {
#       paste("trying to install" ,pkg[i], sep = ":")
#       install.packages(pkg[i]) 
#       if(require(pkg[i])){
#         paste(pkg[i],"installed and loaded", sep = ":")
#       } else {
#         paste("could not install", pkg[i], sep = ":")
#       }
#     }
# }
