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

#filter the dataset so that it only includes PSUs from the sample design


# load updated PSU data (modified Gann -- removed all PSUs outside of scope)
psu <- read.csv('./data/processed/C123_Yr1-5_PSU_Sampled.csv')
head(psu)
nrow(psu)

# PSU list
psu_valid <- psu$PSU_ID
length(psu_valid)

# region list
rgn_valid <- unique(psu$Region)

#restricts the file to only valid PSUs
locs_final <- locs[locs$PSUID %in% psu_valid,]
head(locs_final)
summary(locs_final$hab_09)

#write output to a file (this is the version of Johans file that is restricted
#to sampled PSUs.  Additional filtering likely necessary prior to analysis)
write_csv(locs_final, "./data/processed/C123_Yr1_5_ALL Plots_locations_habs_smpl.csv")

#add 2009 Habitat designation to envData_cln.csv
env$hab_09 <- locs_final$C123_Plot_SNO== paste(env$PSU,env$PLOT)
env$PSU_PLOT <- paste0(env$PSU,env$PLOT)

# merge habitat data to plot vegetation data 
env2 <- merge(env,locs_final[c('PlotID_New','hab_09')], by.x='PSU_PLOT', by.y='PlotID_New')
head(env2)

write_csv(env2, "./data/processed/envData_cln_2.csv")

#revised veg analysis (code adapted from plot_level_veg_analysis.R) using new habitat classification data
# load plot classification data
plot_cls <- sf::st_read('./data/spatial/C123_Yr1_5_ALLPlots_class.shp')
#x11();plot(plot_cls)
table(plot_cls$PSU,plot_cls$hab_09)

plot_cls_hab <- as.data.frame(plot_cls[plot_cls$hab_25 != '',])
head(plot_cls_hab)


