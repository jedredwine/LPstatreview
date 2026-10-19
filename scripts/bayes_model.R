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
# library(raster)
library(gstat)
library(sp)
library(sf)
library(PerformanceAnalytics)
require(sp)
# library(geoR) 
# library(spdep)
# library(fields)
library(ggplot2)
# library(RColorBrewer)
 #install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE) - doesn't work
 #install.packages("INLA", repos = c(getOption("repos"), INLA = "https://inla.r-inla-download.org/R/stable"), dep = TRUE, type = "binary") - works!

# library('INLA') #requires Rtools 4.5 to be installed on computer locally C:\Rtools45
# library(prettymapr) #to decorate maps
# library(maps)
# library(viridis)
# library(shp2graph)
# library(igraph)
# library(sdmpredictors)
# library(leaflet)
# ## To plot rasters in ggplot2
# library(rasterVis)
# library(car)
# # sudo apt-get install r-cran-car
# library(DirichletReg)
# library(Matrix)

###################
## MODELS
###################
## WORK DATABASE

 # load ENV and species %cover data
 sites <- read.csv('./data/processed/C123_Yr1_5_ALL Plots_locations_habs_smpl.csv')
 sites <- sites[!(is.na(sites$C3_X_NAD83) & is.na(sites$C3_Y_NAD83)), ]
 head(sites)
 nrow(sites)
 
 env <- read.csv('./data/processed/envData_cln_2.csv')
 head(env)
 nrow(env)
 
 # load vegetation data
 veg_all <- read.csv('./data/processed/C123_Yr1_5_ALL_SppCover_Final.csv')
 veg_all <- subset(veg_all, SPCODE != "XXXXXX") #remove all data lines where species is named XXXXX
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

# load plot classification data
plot_cls <- read.csv('./data/processed/C123_Yr1_5_ALL Plots_locations_habs_smpl.csv')
#x11();plot(plot_cls)
head(plot_cls);nrow(plot_cls)
table(plot_cls$PSU,plot_cls$hab_09)

#merge location coordinates to all veg plot observations
veg_locs <- merge(veg,plot_cls[c('PlotID_New','hab_09','hab_25', 'C3_X_NAD83','C3_Y_NAD83')], by.x='PlotID', by.y='PlotID_New') #this doesn't work
#add region designation to veg locs
veg_locs_reg <- merge(veg_locs, psu[c('PSU_ID', 'Region')], by.x='PSUID', by.y='PSU_ID')

#steps in analysis - first do this for cladium only (dominant species), then work down list of top 6 species, consider 
# multivariate case eventually
soi <- "CLAJAM"
cy <- "C1"
psu <- "P023"
hab <- "ridge"

soi_PSU_cy <- veg_locs_reg[veg_locs_reg$SPCODE == soi & veg_locs_reg$Cycle == cy & veg_locs_reg$PSUID == psu]
soi_PSU_cy <- veg_locs_reg %>% filter(SPCODE == soi & Cycle == cy & PSUID == psu)
soi_PSU_cy <- subset(veg_locs_reg, SPCODE == soi & Cycle == cy & PSUID == psu & hab_25 == hab)

# soi_PSU_cy1 <- soi_PSU_cy[c(1,7,11,12,13,14,16)] 
# soi_test <- soi_PSU_cy1[soi_PSU_cy1$PSUID == psu]
# Remove rows where 'Cover' or coordinates are missing
# (Replace 'X' and 'Y' with your actual longitude/latitude or easting/northing column names)
soi_PSU_cy1_cln <- soi_PSU_cy[!is.na(soi_PSU_cy$Cover) & !is.na(soi_PSU_cy$C3_X_NAD83) & !is.na(soi_PSU_cy$C3_Y_NAD83), ]

# Convert to an sf object
# Replace 'X' and 'Y' with your exact coordinate column names.
# Replace '4326' with your data's specific EPSG coordinate system code if known (4326 is standard WGS84 Lat/Lon)
soi_PSU_cy1_cln <- st_as_sf(soi_PSU_cy1_cln, coords = c("C3_X_NAD83", "C3_Y_NAD83"), crs = 26917)
total_plots <- sum(soi_PSU_cy1_cln$hab_25 == "ridge",  na.rm = TRUE)
psu_numbers <- paste(unique(soi_PSU_cy1_cln$PSUID), collapse = ", ") 

cover_psu <- ggplot(soi_PSU_cy1_cln) + 
  geom_sf(aes(size = Cover, color = hab_25), alpha = 0.8) +
  scale_size_continuous(name = "Cover", range = c(1, 4)) + 
  scale_color_manual(name = "hab_25", values = c("ridge"  = "#B85A1C", "slough" = "#1A5B8C"))+
  labs(title = "Map Points Sized by Response Value", 
       subtitle = paste("PSU ID:", psu_numbers), 
       caption = paste("Total Plots Used in Model (N) =", total_plots)
  ) +
  theme_minimal()

cover_psu
unique(soi_PSU_cy1_cln$hab_25)

#Use 2/3 of max length of data, should I make variograms one PSU at a time?

if(nrow(soi_PSU_cy) > 5){
  
  # emp_var1 <- variogram(Cover ~ 1 , data = soi_PSU_cy1_cln, cutoff = 5000, width = 5)
  emp_var_geo <- variogram(Cover ~ 1 , data = soi_PSU_cy1_cln, alpha = c(150, 60))
  plot(emp_var_geo)
  
  init_anis_model <- vgm(psill = 600, model = "Gau", range = 800, nugget = 75, anis = c(150, 1.0))
  nested_anis_model <- vgm(psill = 500, model = "Gau", range = 800, anis = c(60, 0.4), add.to = init_anis_model)
  print(nested_anis_model)
  
  
  fitted_anis_model <- fit.variogram(emp_var_geo, model = nested_anis_model)
  plot(emp_var_geo, model = fitted_anis_model, main = "Empirical Variogram", sub = "Nested Anisotropic Fit", xlab = "Distance", ylab = "Semivariance")
  print(fitted_anis_model)
  
  # 3. Extract the optimized parameters for the annotation text box
  nug_val   <- round(fitted_anis_model$psill[1], 1)
  psill_1   <- round(fitted_anis_model$psill[2], 1)
  range_1   <- round(fitted_anis_model$range[2], 1)
  psill_2   <- round(fitted_anis_model$psill[3], 1)
  range_2   <- round(fitted_anis_model$range[3], 1)
  tot_sill  <- round(sum(fitted_anis_model$psill), 1)
  
  param_text <- paste0(
    "Fitted Parameters:\n",
    "Global Nugget: ", nug_val, "\n",
    "Total Sill: ", tot_sill, "\n\n",
    "Structure 1 (330°):\n",
    "• Psill: ", psill_1, " | Range: ", range_1, "m\n\n",
    "Structure 2 (60°):\n",
    "• Psill: ", psill_2, " | Range: ", range_2, "m"
  )
  
  # 4. Extract continuous fitted lines for each angle using variogramLine()
  # We define a max distance matching the empirical data range
  max_dist <- max(emp_var_geo$dist)
  
  line_150 <- variogramLine(fitted_anis_model, maxdist = max_dist, dir = c(0, 1, 0)) # 150 deg direction vector
  line_60  <- variogramLine(fitted_anis_model, maxdist = max_dist, dir = c(sin(60*pi/180), cos(60*pi/180), 0))
  # Assign angles back to the data frames for clear plotting legends
  line_150$dir.hor <- 150
  line_60$dir.hor  <- 60
  fitted_lines     <- rbind(line_150, line_60)
  
  # Convert directory column to factors for discrete coloring
  emp_var_geo$dir.hor  <- as.factor(emp_var_geo$dir.hor)
  fitted_lines$dir.hor <- as.factor(fitted_lines$dir.hor)
  
  # 5. Build the ggplot2 Visualization
  ggplot() +
    # Draw the empirical points
    geom_point(data = emp_var_geo, aes(x = dist, y = gamma, color = dir.hor), size = 3) +
    # Draw the continuous theoretical curves
    geom_line(data = fitted_lines, aes(x = dist, y = gamma, color = dir.hor), linewidth = 1) +
    # Add the parameters text block box
    annotate("label", x = max_dist * 0.05, y = max(emp_var_geo$gamma) * 0.95, 
             label = param_text, hjust = 0, vjust = 1, 
             fill = "white", color = "darkblue", fontface = "bold", size = 3.5, alpha = 0.85) +
    # Styling and labels
    scale_color_manual(values = c("150" = "#D55E00", "60" = "#0072B2"),
                       labels = c("150" = "150° Azimuth (330° Axis)", "60" = "60° Azimuth")) +
    labs(
      title = "Empirical Variogram with Nested Anisotropic Fit",
      subtitle = paste0("Calculated in Meters | Gaussian Base Structures\n", 
                        paste("PSU ID:", psu_numbers)),
      x = "Distance (meters)",
      y = "Semivariance (Gamma)",
      color = "Directional Slices"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold")
    )
  
  
  
# spatial weights matrix
  suppressWarnings({nb_points <- knn2nb(knearneigh(soi_PSU_cy[, c("C3_X_NAD83", "C3_Y_NAD83")], k = 7))})
  listw_points <- nb2listw(nb_points, style = "W")
  
  # test spatial autocorrelation in residuals Null: no spatial autocorrelation
  test_stat <- lm.morantest(mdl_0, listw_points)
  print(paste0("For ",soi," in ",PSU," and Cycle",cy,", p-value for M-I is:",test_stat$p.value))
}
 #step 1: plot semivariogram for all data pooled - test for significance of model fit, one species at a time
# Compute the empirical semivariogram



mdata <- cbind(veg_locs_reg$)



# Plot the empirical semivariogram
plot(emp_var, main = "Empirical Semivariogram for Vegetation Data")

 #step 2: introduce region and PSU as grouping variables (first psu, then region) and test for improvement in
 #        explanatory power over all data pooled.  Plot grouping structures and test for signficance of model fit

 #step 3:

