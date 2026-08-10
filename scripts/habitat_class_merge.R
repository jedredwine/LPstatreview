# File name: habitat_class_merge.R
# Author: Jed Redwine
# email: jredwine@fiu.edu

# Purpose: add information from Johan's habitat characterization to envData_cln. dataset
# Required: QAQC'd plot-specific environmental data (envData_cln.csv)

#-------------------------------------------------------------------------------
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
#-------------------------------------------------------------------------------
# load data
locs <- read.csv('./data/processed/C123_Yr1_5_ALL Plots_locations.csv')
head(locs);nrow(locs)

habs <- read.csv('./data/raw_data/C123_Yr1_5_ALLPlots_locations_Johan_Habitat.csv')
head(habs);nrow(habs)

identical(habs$C123_Plot_, locs$C123_Plot_SNO) #determines that files contain identically sorted rows
#-------------------------------------------------------------------------------

#merge datasets by adding new columns created by Johan to locs dataset (locs is the processed dataset)
locs$notes <- habs$notes
locs$hab_25 <- habs$hab_25
locs$hab_09 <- habs$hab_09
#-------------------------------------------------------------------------------

#write output to a file
write_csv(locs, "./data/processed/C123_Yr1_5_ALL Plots_locations_habs.csv")

