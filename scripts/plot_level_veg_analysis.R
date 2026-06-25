

# File name: vegetation_summary.R
# Author: Dan Gann
# email: gannd@fiu.edu

# Required: PSU sampling info (LP_R&S_C123_Yr1-5_PSU_Sampled.csv)
#-------------------------------------------------------------------------------
library(sf)
library(terra)
library(tidyr)
library(dplyr)
library(ggplot2)
library(rstatix)
library(Rmisc)
# ==============================================================================

# load PSU shapefile with final approved LS analysis PSUs
psu_sp <- sf::st_read('./data/PSU_RS_LS.shp')
x11()
psus_region <- ggplot() + 
  geom_sf(data = psu_sp, aes(fill = Region))

# PSU list
psu_list <- psu_sp$PSU_N
psu_list <- psu_list[order(psu_list)]
length(psu_list)
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
# ------------------------------------------------------------------------------
# load vegetation data
veg_all <- read.csv('./data/processed/C123_Yr1_5_ALL_SppCover_Final.csv')
nrow(veg_all)

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
# ------------------------------------------------------------------------------
# add mean cover by functional class
# load functional type class table
fnc_tbl <- read.csv('./data/processed/spp_functional_types.csv')

# join functional class to vegetation data
merge(veg_all,fnc_tbl,by= "SPCODE", x.all=TRUE)
# ------------------------------------------------------------------------------

# load plot classification data
plot_cls <- sf::st_read('//ditnocfs01.ad.fiu.edu/gannfs01/ACOE_RS/_data/C123_Yr1_5_ALLPlots_class_Johan.shp')
#x11();plot(plot_cls)
table(plot_cls$PSU,plot_cls$habitat)


plot_cls_hab <- as.data.frame(plot_cls[plot_cls$habitat != '',])
head(plot_cls_hab)
# ------------------------------------------------------------------------------

# Plotting and Analysis

#-------------------------------------------------------------------------------

# Summarize plot level species change by Plot Habitat Type by PSU  by Region
#     -- loop over all species of interest

# set region of interest
reg <- 'ENP'

# select sub-regions that match region of interest
reg_cur <- rgn_valid[grepl(reg, rgn_valid, fixed = TRUE)] 

# subset PSU list for ROI
psu_cur <- psu$PSU_ID[psu$Region %in% reg_cur]

# subset plots for ROI
veg_cur <- veg[veg$PSUID %in% psu_cur,]
head(veg_cur)
# ------------------------------------------------------------------------------
plot_list_cycle <- as.data.frame(unique(veg_cur$PlotID_Cycle));names(plot_list_cycle) <- 'PlotID_Cycle'
plot_list_cycle$PlotID <- substr(plot_list_cycle$PlotID_Cycle,1,8)

# merge habitat data to plot vegetation data 
plot_list_cycle <- merge(plot_list_cycle,plot_cls_hab[c('PlotID_New','habitat')], by.x='PlotID', by.y='PlotID_New')
plot_list_cycle

# species of interest list (no more than 9)
soi_list <- c('CLAJAM','TYPDOM','ELECEL','NYMODO','NYMAQU','UTRPUR')

# ------------------------------------------------------------------------------
# merge data frames outer left join
veg_anl <- merge(plot_list_cycle,veg_cur[,c('PlotID_Cycle','ClusterID','SPCODE','Cover')],by='PlotID_Cycle',all.x=TRUE)
head(veg_anl,200)
veg_anl$Cover[is.na(veg_anl$Cover)] <- 0
veg_anl$PSUID <- substring(veg_anl$PlotID_Cycle,1,4)
veg_anl$PlotID <- substring(veg_anl$PlotID_Cycle,1,8)
veg_anl$ClusterID <- substring(veg_anl$PlotID_Cycle,1,6)
veg_anl$Cycle <- substring(veg_anl$PlotID_Cycle,10,11)

# for PSU of interest and species of interest (SOI) plot habitat specific species abundance trends
for (PSU in psu_cur){
  veg_anl_PSU <- veg_anl[veg_anl$PSUID == PSU,]
  head(veg_anl_PSU)
  
  # subset to SOI list
  veg_anl_SOI <- veg_anl_PSU[veg_anl_PSU$SPCODE %in% soi_list,]
  
  # summarySE provides the standard deviation, standard error of the mean, and a (default 95%) confidence interval
  stats <- summarySE(veg_anl_SOI, measurevar="Cover", groupvars=c("SPCODE","habitat"))
 
  p <- ggplot(veg_anl_SOI, aes(x = Cycle, y = Cover, group = habitat,color=habitat)) +
    geom_line(aes(group = PlotID), colour = "grey70", alpha = 0.7) +
    geom_point(aes(group = PlotID), colour = "grey30", size = 1.5) +
    # geom_errorbar() +
    stat_summary(fun = median, geom = "point",size = 3.5) +
    stat_summary(fun = median, geom = "line", size = 1.2) +
    labs(title = paste("Cover Change",soi,"in PSU",PSU),
         x = "Cycle", y = "Cover") +
    theme_minimal(base_size = 13) +
    facet_wrap(~ SPCODE)

  # plot change of SOI of plot by PSU
  ggsave(paste("./figures/sp_trend_by_PSU/plot_SOI_trend_for_",PSU,".jpg"), 
         plot = p, width = 10, height = 10, dpi = 400, device = "jpeg")
  
  # write summary stats to file
  write.csv(stats,paste("./figures/sp_trend_by_PSU/plot_SOI_trend_for_",PSU,"_stats.csv"))
  
  # # for species of interest (SOI) rest for significance of change
  # for (soi in soi_list){
  #   print(soi)
  #   veg_soi_cur <- veg_anl_SOI[veg_anl_SOI$SPCODE == soi,]
  #   head(veg_soi_cur)
  # 
  #   # summarize SOI by cluster and cycle
  #   veg_cover_subplot <- veg_soi_cur %>%
  #     group_by(ClusterID,Cycle,SPCODE) %>%
  #     summarize(cover_mean = mean(Cover),
  #               cover_sd = sd(Cover)
  # 
  #     )
  #   # test difference by PSU
  #   veg_soi_cur %>% friedman_test(Cover ~ Cycle | PlotID)
  # }
}
# ==============================================================================












