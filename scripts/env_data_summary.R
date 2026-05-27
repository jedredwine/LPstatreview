# ==============================================================================

# File Name: env_data_summary.R
# Author: Dan Gann
# email: gannd@fiu.edu

# Required: Environmental data (xxxx.csv)
# ==============================================================================
library(sf)
library(tidyr)
library(dplyr)
library(ggplot2)
#-------------------------------------------------------------------------------

# DATA issues encountered by D. Gann for cleaned data 2026-03-23 (S. Bogan) 

{ 
  # Inconsistency of habitat names
  # -999 for NA
  # non-random PSUs -- 513 and 220?
  # plots outside PSUs
  # water depth SD too high for local variability
  
  
  ## First Cleaned Data
  
  # # load ENV data (Jay Sah raw data table converted to .csv modified column names)
  # # loc <- read.csv('./data/processed/plotsRef_byCycle.csv')
  # # env <- read.csv('./data/processed/envData_cln.csv')
  # # head(env)
  # # nrow(env)
  # 
  # # # generate PSU and PLOT_ID -- first time only then save
  # # env <- separate(env, PLOT_ID, into = c('PSU','PLOT'),sep = c(4))
  # 
  # # check NA status for location --> remove because point might fall outside cluster distance tolerance
  # # check NA status for location --> elevation
  # table(is.na(env$EAST_UTM_NAD83))
  # table(is.na(env$NORTH_UTM_NAD83))
  # table(!is.na(env$GR_ELEV))
  # 
  # env[is.na(env$EAST_UTM_NAD83),]
  # 
  # # check for WD errors
  # env$WD_SD <- apply(env[, c("WD1","WD2","WD3")], 1, function(x) sd(x, na.rm=TRUE))
  # env$WD_SD[!is.na(env$WD_SD) & env$WD_SD > 50]
  # 
  # # check habitat classes and MAP classes
  # as.data.frame(table(env$HABITAT_L2))                # typos fixed, but classes are still redundant or inconsistent in descriptors
  # as.data.frame(table(env$MAP_CLASS)) # -- omit                # many non-map class labels
  # 
  # # dropping sites that are outside of PSU boundary and those > 80 (added in cycle 3)
  # # check HABITAT_L2 against MAP_CLASS # -- omit
  # table(env$HABITAT_L2,env$MAP_CLASS)
  # # ------------------------------------------------------------------------------
  # # plot ELE and save to jpeg
  # psu.ELE.HAB <- ggplot(env, aes(x=PSU,y=GR_ELEV,fill=HABITAT_L2)) +
  #   geom_boxplot() +
  #   # geom_text(aes(label=Freq),color="black",size=3) +
  #   # scale_fill_gradient(low='white',high='slateblue') +
  #   # scale_x_discrete(labels=c("Cycle 1","Cycle 2","Cycle 3")) +
  #   labs(x='PSU', y='Ground Elevation (cm)',fill = "Habitat") +
  #   theme_minimal()
  # 
  # ggsave("./figures/psus_ELE_byHabitat.jpg", plot = psu.ELE.HAB, width = 10, height = 5, dpi = 400, device = "jpeg")
}
# ==============================================================================

# DATA issues encountered by D. Gann for cleaned data 2026-05-16 (J. Sah)

{
# Inconsistency of habitat names remains
# non-random PSUs still present -- removed for analysis
# 

# load PSU shapefile with final approved LS analysis PSUs
psu_sp <- st_read('./data/PSU_RS_LS.shp')
x11()
psus_region <- ggplot() + 
  geom_sf(data = psu_sp, aes(fill = Region))

# PSU list
psu_list <- psu_sp$PSU_N
length(psu_list)

# save plot
ggsave("./figures/psus_x_region.jpg", plot = psus_region, width = 4, height = 6, dpi = 400, device = "jpeg")
# ==============================================================================
# load ENV data (Jay Sah raw data table (2026-05-16) converted to .csv, column names fixed: no underscores at beginning, changed: WD_mean_cm, Class, EDEN_based_elev_cm CLAJAM_ht_cm)
loc <- read.csv('./data/processed/C123_Yr1_5_ALL Plots_locations.csv')
env <- read.csv('./data/processed/C123_Yr1_5_ALL Plots_envData.csv')
head(env)
nrow(env)

table(is.na(env$X_NAD83))
table(is.na(env$Y_NAD83))
table(!is.na(env$EDEN_based_elev_cm))
# ------------------------------------------------------------------------------
# subset plots to final PSU list
env <- env[env$PSU %in% psu_list,]

# check PSUs
table(env$PSUID)
length(unique(env$PSUID))
# ------------------------------------------------------------------------------

# PLOT LEVEL QA/QC

# check records with no location info
table(env[is.na(env$X_NAD83),]$PSUID,env[is.na(env$X_NAD83),]$Cluster)  # PSU 17, Clusters 48,49,50 in Cycle 1, Year 2

# convert plot data to spatial data frame
plot_sp <- st_as_sf(env[!is.na(env$X_NAD83),], coords = c("X_NAD83", "Y_NAD83"), crs = 26917)

# check location coordinate consistency across time
coord_test <- env %>%
  group_by(PlotID_New) %>%
  summarise(East_mm = mean(X_NAD83),
            East_sd = sd(X_NAD83),
            North_mm = mean(Y_NAD83),
            North_sd = sd(Y_NAD83))

# write summary to csv
write.csv(coord_test,'./analysis/plot_coordinate_consistency.csv')
# ----------------------
# convert to longform for plotting
coord_test_lf <- coord_test[,c('PlotID_New','East_sd','North_sd')] %>%
    pivot_longer(
      cols = c(East_sd,North_sd),
      names_to = 'variable',
      values_to = 'value'
    )

# plot standard deviations
plot.coord.sd <- ggplot(coord_test_lf[! is.na(coord_test_lf$value),], aes(x=variable,y=value)) +
  geom_boxplot() +
  labs(x='Coordinate', y='Coordiante SD (m)') +
  theme_minimal()

ggsave("./figures/plot_coord_sd.jpg", plot = plot.coord.sd, width = 10, height = 5, dpi = 400, device = "jpeg")
# ----------------------
# summarize spatial consistency of plots within PSUs
plot_psu_sp <- st_join(plot_sp,psu_sp,left=TRUE)

# group by PSU and count points
summary_plot_counts_PSU <- plot_psu_sp %>%
  group_by(PSU_ID) %>%
  summarise(point_count = n())
print(summary_plot_counts_PSU, n=100)

# write summary to csv
write.csv(as.data.frame(summary_plot_counts_PSU)[,c("PSU_ID","point_count")],'./analysis/plot_psu_count.csv')

# identify plot outside of PSUs & write to file
non_intersecting_pts <- plot_psu_sp %>% filter(is.na(PSU_ID))

# write summary to csv
write.csv(as.data.frame(non_intersecting_pts)[,c("C123Merged_EnvSNO","C123_Plot_SNO","PlotID_New","PSUID","Cycle.x","Cycle_Year")],'./analysis/plot_no_psu.csv')

# summarize plots by PSU an Cycle
write.csv(as.data.frame(table(non_intersecting_pts$PSUID)),'./analysis/plot_no_psu_count.csv')

# ----------------------
# identify plots in wrong PSUs & write to file
intersecting_pts <- plot_psu_sp %>% filter(!is.na(PSU_ID))

plot_psu_sp_noPSU <- intersecting_pts[intersecting_pts$PSUID != intersecting_pts$PSU_ID,]

# write summary to csv
write.csv(as.data.frame(non_intersecting_pts)[,c("C123Merged_EnvSNO","C123_Plot_SNO","PlotID_New","PSUID","Cycle.x","Cycle_Year")],'./analysis/plot_no_psu.csv')

# ------------------------------------------------------------------------------

# Water Depth QA/QC

# identify WD standard deviations of greater 30 cm
env$WD_SD <- apply(env[, c("WD_1_cm","WD_2_cm","WD_3_cm")], 1, function(x) sd(x, na.rm=TRUE))
env[!is.na(env$WD_SD) & env$WD_SD > 30,]  # 5 records
# ------------------------------------------------------------------------------
# Elevation QA/QC

# check derived elevation consistency across time
ele_test <- env %>%
  group_by(PlotID_New) %>%
  summarise(ele_sd = sd(EDEN_based_elev_cm),
            ele_range = max(EDEN_based_elev_cm, na.rm = TRUE) - min(EDEN_based_elev_cm, na.rm = TRUE))
# ----------------------
# write summary to csv
write.csv(ele_test,'./analysis/plot_elevation_consistency.csv')
# ----------------------
# plot standard deviations
plot.elev.sd <- ggplot(ele_test[!is.na(ele_test$ele_sd),], aes(x='',y=ele_sd)) +
  geom_boxplot() +
  labs(x='All Plots', y='Elevation SD (cm)') +
  theme_minimal()

ggsave("./figures/plot_elev_sd.jpg", plot = plot.elev.sd, width = 5, height = 5, dpi = 400, device = "jpeg")
# ----------------------
# plot range
plot.elev.rng <- ggplot(ele_test[!is.na(ele_test$ele_range),], aes(x='All Plots',y=ele_range)) +
  geom_boxplot() +
  labs(x='', y='Elevation Range (cm)') +
  theme_minimal()

ggsave("./figures/plot_elev_rng.jpg", plot = plot.elev.rng, width = 5, height = 5, dpi = 400, device = "jpeg")
# = = = = = = = = = = = =
# check derived elevation consistency across cycles 2 & 3
ele_test_c23 <- env[env$Cycle == "C1" | env$Cycle == "C2",] %>%
  group_by(PlotID_New) %>%
  summarise(ele_sd = sd(EDEN_based_elev_cm),
            ele_range = max(EDEN_based_elev_cm, na.rm = TRUE) - min(EDEN_based_elev_cm, na.rm = TRUE))
# ----------------------
# write summary to csv
write.csv(ele_test_2,'./analysis/plot_elevation_consistency_C1&C2.csv')
# ----------------------
# plot standard deviations
plot.elev.c1c2.sd <- ggplot(ele_test_c23[!is.na(ele_test_c23$ele_sd),], aes(x='',y=ele_sd)) +
  geom_boxplot() +
  labs(x='All Plots', y='Elevation SD (cm)') +
  theme_minimal()

ggsave("./figures/plot_elev_C1C2_sd.jpg", plot = plot.elev.c1c2.sd, width = 5, height = 5, dpi = 400, device = "jpeg")
# ----------------------
# plot range
plot.elev.c1c2rng <- ggplot(ele_test_c23[!is.na(ele_test_c23$ele_range),], aes(x='All Plots',y=ele_range)) +
  geom_boxplot() +
  labs(x='', y='Elevation Range (cm)') +
  theme_minimal()

ggsave("./figures/plot_elev_C1C2_rng.jpg", plot = plot.elev.c1c2.rng, width = 5, height = 5, dpi = 400, device = "jpeg")
# ------------------------------------------------------------------------------

# Vegetation Class QA/QC

# check habitat classes
as.data.frame(table(env$Class))       # typos initially fixed are wrong again, classes are still redundant or inconsistent in descriptors

# ==============================================================================

# PLOTS & ANALYSIS

# plot class differences in elevation by PSU
for (psu in unique(env$PSUID)){
  
  psu.ELE.HAB <- ggplot(env[env$PSUID == psu,], aes(x=psu,y=EDEN_based_elev_cm,fill=Class)) +
    geom_boxplot() +
    labs(y='Ground Elevation (cm)',fill = "Habitat") +
    theme_minimal()
  
  ggsave(paste0('./figures/psus_ELE_x_HAB/',psu,'_ele_x_hab.jpg'), plot = psu.ELE.HAB, width = 10, height = 5, dpi = 400, device = "jpeg")
}

}


