# File name: end_data_summary.R
# Author: Dan Gann
# email: gannd@fiu.edu

# Required: Environmental data (xxxx.csv)
#-------------------------------------------------------------------------------
library(tidyr)
library(dplyr)
library(ggplot2)
#-------------------------------------------------------------------------------

# DATA ISSUES ENCOUNTERED -- Inconsistency of habitat names; -999 for NA; non-random PSUs removed -- 513 and 220?

#-------------------------------------------------------------------------------

# load ENV data (Jay Sah raw data table converted to .csv modified column names)
env <- read.csv('./data/processed/envData_cln.csv')
head(env)
nrow(env)

# # generate PSU and PLOT_ID -- first time only then save
# env <- separate(env, PLOT_ID, into = c('PSU','PLOT'),sep = c(4))

# check NA status for location --> remove because point might fall outside cluster distance tolerance 
# check NA status for location --> elevation
table(is.na(env$EAST_UTM_NAD83))
table(is.na(env$NORTH_UTM_NAD83))
table(!is.na(env$GR_ELEV))

env[is.na(env$EAST_UTM_NAD83),]

# check for WD errors
env$WD_SD <- apply(env[, c("WD1","WD2","WD3")], 1, function(x) sd(x, na.rm=TRUE))
env$WD_SD[!is.na(env$WD_SD) & env$WD_SD > 50]

# check habitat classes and MAP classes
as.data.frame(table(env$HABITAT_L2))                # typos fixed, but classes are still redundant or inconsistent in descriptors
as.data.frame(table(env$MAP_CLASS)) # -- omit                # many non-map class labels

# dropping sites that are outside of PSU boundary and those > 80 (added in cycle 3)
# check HABITAT_L2 against MAP_CLASS # -- omit
table(env$HABITAT_L2,env$MAP_CLASS)
# ------------------------------------------------------------------------------
# plot ELE and save to jpeg
psu.ELE.HAB <- ggplot(env, aes(x=PSU,y=GR_ELEV,fill=HABITAT_L2)) +
  geom_boxplot() +
  # geom_text(aes(label=Freq),color="black",size=3) +
  # scale_fill_gradient(low='white',high='slateblue') +
  # scale_x_discrete(labels=c("Cycle 1","Cycle 2","Cycle 3")) +
  labs(x='PSU', y='Ground Elevation (cm)',fill = "Habitat") +
  theme_minimal()

ggsave("./figures/psus_ELE_byHabitat.jpg", plot = psu.ELE.HAB, width = 10, height = 5, dpi = 400, device = "jpeg")
#-------------------------------------------------------------------------------

