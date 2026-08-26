# File name: bayes_model.R
# Author: Jed Redwine
# email: jredwine@fiu.edu

# Purpose: analyze percent cover changes for PSUs and regions using Bayesian spatial
# Beta regression models
# Required: vegetation and envrionmental data - envData_cln.csv and 
# C123Yr1_5_ALL_SppCover_Final.csv
# this code is adapted from a variety of sources, including B. Sarzo's seabird
# biodiversity modeling through Bayesian Spatial Beta Regression models

###############
## Libraries
###############
library(raster)
library(gstat)
library(sp)
library(sf)
library(PerformanceAnalytics)
require(sp)
library(geoR) 
library(spdep)
library(fields)
library(ggplot2)
library(RColorBrewer)
 #install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE) - doesn't work
 #install.packages("INLA", repos = c(getOption("repos"), INLA = "https://inla.r-inla-download.org/R/stable"), dep = TRUE, type = "binary") - works!

library('INLA') #requires Rtools 4.5 to be installed on computer locally C:\Rtools45
library(prettymapr) #to decorate maps
library(maps)
library(viridis)
library(shp2graph)
library(igraph)
library(sdmpredictors)
library(leaflet)
## To plot rasters in ggplot2
library(rasterVis)
library(car)
# sudo apt-get install r-cran-car
library(DirichletReg)
library(Matrix)

###################
## MODELS
###################
## WORK DATABASE

 # load ENV and species %cover data
 sites <- read.csv('./data/processed/C123_Yr1_5_ALL Plots_locations_habs_smpl.csv')
 head(sites)
 nrow(sites)
 
 env <- read.csv('./data/processed/envData_cln_2.csv')
 head(env)
 nrow(env)
 
 # load vegetation data
 veg_all <- read.csv('./data/processed/C123_Yr1_5_ALL_SppCover_Final.csv')
 nrow(veg_all)


# seabirds database from example code - this matrix contains response variables, 
 # location data, and covariates that are standardized - transformed to N(0,1) distributions

 # load("seabirds_def.RData") 
# seabirds1 <- seabirds_def

### Variable selection
## First we need to remove NA values
## Seabirds has not NAs

# library(subselect)
# 
# ## First remove the NAs
# pos.na1 <- which(is.na(seabirds1$ais)) 
# pos.na2 <- which(is.na(seabirds1$sss)) 
# pos.na3 <- which(is.na(seabirds1$climate_changes)) 
# pos.na4 <- which(is.na(seabirds1$aspE)) 
# 
# seabirds <- seabirds1[-c(pos.na1,pos.na2,pos.na3,pos.na4),]

##################################################################### 
 ####all of the code in this section is for cleaning/QC of vegetation data, once we have a final approved vegetation plot 
 ####database, this code can be replaced by reading in the final dataset
 
# load PSU shapefile with final approved LS analysis PSUs
psu_sp <- sf::st_read('./data/spatial/PSU_RS_LS.shp')
x11()
psus_region <- ggplot(psu_sp) + 
  geom_sf(aes(fill = Region)) +
  geom_sf_text(aes(label = PSU_N), size = 3) +
  theme_minimal()
psus_region

# generate species list
sp_list <- as.data.frame(table(veg_all$SPCODE));names(sp_list) <- c('SPCODE','freq')

# generate PSU list
psu_list <- psu_sp$PSU_N
psu_list <- psu_list[order(psu_list)]
length(psu_list)

# region list
rgn_valid <- unique(psu$Region)


# ------------------------------------------------------------------------------
# load updated PSU data (modified Gann -- removed all PSUs outside of scope)
psu <- read.csv('./data/processed/C123_Yr1-5_PSU_Sampled.csv')
head(psu)
nrow(psu)

# PSU list
psu_valid <- psu$PSU_ID
length(psu_valid)

# region list
rgn_valid <- unique(psu$Region)

# check PSU count and plot surveys
veg <- veg_all[veg_all$PSU %in% psu_valid,]
nrow(veg)

table(veg$PSUID)
length(unique(veg$PSUID))

# add Custer ID
veg$ClusterID <- substring(veg$PlotID,1,6)

# add plot ID
veg$PlotID_Cycle <- paste(veg$PlotID,veg$Cycle,sep='_')

# convert cover to numeric
veg$Cover <- as.integer(veg$Cover)
head(veg)
#######end of veg QC steps
#################################################################

# before first analysis, need to prepare datasets with appropriate elements - add coordinates for each plot and filter 
# to a single species, 

# select sub-regions that match region of interest
rgn_cur <- rgn_valid 


# ------------------------------------------------------------------------------
plot_list_cycle <- as.data.frame(unique(veg$PlotID_Cycle));names(plot_list_cycle) <- 'PlotID_Cycle'
plot_list_cycle$PlotID <- substr(plot_list_cycle$PlotID_Cycle,1,8)

# species of interest list (no more than 9)
soi_list <- c('CLAJAM','TYPDOM','ELECEL','NYMODO','NYMAQU','UTRPUR')
# ------------------------------------------------------------------------------
# generate plot_cycle_soi table
cy_list <- c('C1','C2','C3')


#merge location coordinates to all veg plot observations
veg_locs <- merge(veg,plot_cls[c('PlotID_New','hab_09','C3_X_NAD83','C3_Y_NAD83')], by.x='PlotID', by.y='PlotID_New')
#add region designation to veg locs
veg_locs_reg <- merge(veg_locs, psu[c('PSU_ID', 'Region')], by.x='PSUID', by.y='PSU_ID')

#steps in analysis - first do this for cladium only (dominant species), then work down list of top 6 species, consider 
# multivariate case eventually
soi <- "CLAJAM"
cy <- "C1"

soi_PSU_cy <- veg_locs_reg[veg_locs_reg$SPCODE == soi & veg_locs_reg$Cycle == cy,]
soi_PSU_cy1 <- soi_PSU_cy[c(7,12,13)] 
coordinates(soi_PSU_cy1) <- ~C3_X_NAD83+C3_Y_NAD83

if(nrow(soi_PSU_cy) > 5){
  
  
  emp_var <- variogram(Cover~1, soi_PSU_cy1, cutoff = 1000, width = 5)
  model.1 <- fit.variogram(emp_var,vgm(1000,"Gau",300,2.5))
  plot(emp_var, model=model.1)
  exp_variogram_data <- data.frame(distance = emp_var$dist, gamma = emp_var$gamma)
  ggplot(exp_variogram_data, aes(x = distance, y = gamma)) +
    geom_point() +
    geom_line() +
    labs(x = "Lag (h)", y = "Semivariance") +
    theme_minimal()
plot(emp_var)

# spatial weights matrix
  suppressWarnings({nb_points <- knn2nb(knearneigh(soi_PSU_cy[, c("C3_X_NAD83", "C3_Y_NAD83")], k = 7))})
  listw_points <- nb2listw(nb_points, style = "W")
  
  # test spatial autocorrelation in residuals Null: no spatial autocorrelation
  test_stat <- lm.morantest(mdl_0, listw_points)
  print(paste0("FOr ",soi," in ",PSU," and Cycle",cy,", p-value for M-I is:",test_stat$p.value))
}
 #step 1: plot semivariogram for all data pooled - test for significance of model fit, one species at a time
# Compute the empirical semivariogram



mdata <- cbind(veg_locs_reg$)



# Plot the empirical semivariogram
plot(emp_var, main = "Empirical Semivariogram for Vegetation Data")

 #step 2: introduce region and PSU as grouping variables (first psu, then region) and test for improvement in
 #        explanatory power over all data pooled.  Plot grouping structures and test for signficance of model fit

 #step 3:

