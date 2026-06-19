# File name: sampling_summary.R
# Author: Dan Gann
# email: gannd@fiu.edu

# Required: PSU sampling info (LP_R&S_C123_Yr1-5_PSU_Sampled.csv)
#-------------------------------------------------------------------------------
library(tidyr)
library(dplyr)
library(ggplot2)
#-------------------------------------------------------------------------------

# issue encountered in vegData.csv -
# - non-valid PSU
# - no 0 cover for absence
# - plot naming between cycles not consistent (i.e., 1,2,3 in C1 vs. C,E,N in C2 and C3 --> is 1=C, 2= E|N, 3 = E|N consistently?)

#-------------------------------------------------------------------------------
# load PSU shapefile with final approved LS analysis PSUs
psu_sp <- st_read('./data/PSU_RS_LS.shp')
x11()
psus_region <- ggplot() + 
  geom_sf(data = psu_sp, aes(fill = Region))

# PSU list
psu_list <- psu_sp$PSU_N
psu_list <- psu_list[order(psu_list)]
length(psu_list)
# ------------------------------------------------------------------------------
# load updated PSU data (modified Gann -- removed all PSUs outside of scope)
psu <- read.csv('./data/processed/LP_R&S_C123_Yr1-5_PSU_Sampled.csv')
head(psu)
nrow(psu)

# PSU list
psu_valid <- psu$PSU_ID
length(psu_valid)
# ------------------------------------------------------------------------------
# load vegetation data
veg_all <- read.csv('./data/processed/vegData.csv')
nrow(veg_all)
# check PSU count and plot surveys
veg_all$PSU <- substring(veg_all$PlotID,1,4)
veg <- veg_all[veg_all$PSU %in% psu_valid,]
nrow(veg)

# add plot ID
veg$PSU_PlotID <- substring(veg$PlotID,1,6)
veg$PlotID_Cycle <- paste(veg$PlotID,veg$PSU_Cycle,sep='_')
# convert cover to numeric
veg$Cover <- as.integer(veg$Cover)


#-------------------------------------------------------------------------------

# Summarize plot level species change by PSU and by Region

plot_list_cycle <- as.data.frame(unique(veg$PlotID_Cycle));names(plot_list_cycle) <- 'PlotID_Cycle'
  
# set species of interest (SOI)
soi <- 'CLAJAM'

veg_soi_cur <- veg[veg$SPCODE == soi,]

# merge data frames outer left join
veg_anl <- merge(plot_list_cycle,veg_soi_cur,by='PlotID_Cycle',all.x=TRUE)
veg_anl$Cover[is.na(veg_anl$Cover)] <- 0
veg_anl$SPCODE[is.na(veg_anl$SPCODE)] <- soi

# summarize SOI by plot over time
veg_cover_subplot <- veg_anl %>%
  group_by(PSU_PlotID,PSU_Cycle,SPCODE) %>%
  summarize(cover_mean = mean(Cover),
            cover_sd = sd(Cover)
            
  )







