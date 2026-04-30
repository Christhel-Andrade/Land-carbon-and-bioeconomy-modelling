##---RothC C input files generation---##

# RothC-Bioeconomy: This script quantifies the aboveground and belowground C inputs in each PCU for all the scenarios under study
# TS input files are created to be used as input for RothC-Bioeconomy
# Script prepared reusing the code published for AMG in Andrade et al. 2022 (APEN) and adapted to Rothc format.
# This script is linked to the script C_in_calcul

#  Author: Christhel Andrade  Date: 09/11/2022

# Load libraries ---------------------------------------------------------------------
library(tidyverse)
library(readxl)
library(scales)
library(readr)
library(foreach)
library(doParallel)
library(stringr)
library(magrittr)
library(dplyr)
library(data.table)
numCores <- detectCores()
numCores


# Import data ----------------------------------------------------------------------
# Working directory
setwd("path/to/your/directory")

# Number of database to create = 1 or more files
list_files <- list.files(pattern = "Init_data_subset_", all.files = FALSE, full.names = FALSE)
list_files <- str_replace_all(list_files, pattern = "Init_data_subset_", replacement="")
list_files <- str_replace_all(list_files, pattern = ".csv", replacement="")


# option = "yes" -> data for SA (will generate 9 files) or "no" -> data for common scenario (only 1 generated file)
generating_data_for_sensitivity_analysis <- "yes"

#### PROCEDURE 1 = 1 scenario at once ----------------
# defining scenario = "Baseline" or "Pyrochar","Digestate","Gaschar","Hydrochar"

scenario <- "Digestate"
export_percentage <- 100

# Generating RothC input files in folder "input_RothC"

source("C_in_Updatd.R")

#### PROCEDURE 2 = all scenarios at once ----------------
# defining list of scenarios = c(Pyrochar","Digestate",...)
#"Pyrochar",,"Gaschar","Hydrochar"

list_scenarios <- c("Gaschar")

export_percentage <- 100

# Generating RothC input files in folder "input_RothC"
scenario <- ""

# Generating RothC input files in folder "input_RothC" multi-core option

for (scenario in list_scenarios){
  source("C_in_Updatd.R")
}

