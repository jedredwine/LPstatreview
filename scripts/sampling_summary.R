# File name: sampling_summary.R
# Author: Dan Gann
# email: gannd@fiu.edu

# Required: PSU sampling info (LP_R&S_C123_Yr1-5_PSU_Sampled.csv)
#-------------------------------------------------------------------------------
library(tidyr)
library(dplyr)
library(ggplot2)
#-------------------------------------------------------------------------------
# # load PSU data (Bogen modified)
# psu <- read.csv('./data/processed/psus.csv')
# psu.rgn.cyc <- psu %>% mutate(across(c('Cy1_WYr','Cy2_WYr','Cy3_WYr',), as.integer))
# head(psu)
# nrow(psu)
# 
# # convert to long form
# # table PSUs by Year and Region
# psu.rgn.cyc_lf <- psu.rgn.cyc[,c('PSU_ID','Region','Cy1_WYr','Cy2_WYr','Cy3_WYr')] %>%
#   pivot_longer(
#     cols = c(Cy1_WYr,Cy2_WYr,Cy3_WYr),
#     names_to = 'Cycle',
#     values_to = 'value'
#   )
#-------------------------------------------------------------------------------
# load PSU data (Jay Sah raw data table converted to .csv modified column names)
psu <- read.csv('./data/processed/LP_R&S_C123_Yr1-5_PSU_Sampled.csv')
head(psu)
nrow(psu)
#-------------------------------------------------------------------------------

# Summarize PSU count by Region and Cycle

# convert to LF
psu.rgn.cyc_lf <- psu[,c('PSU_ID','Region','C1','C2','C3')] %>%
  pivot_longer(
    cols = c(C1,C2,C3),
    names_to = 'Cycle_Name',
    values_to = 'Cycle'
  )

# table to plot
psu.rgn.cyc_lf.plt <- as.data.frame(table(psu.rgn.cyc_lf$Region,psu.rgn.cyc_lf$Cycle))
names(psu.rgn.cyc_lf.plt) <- c('Region','Cycle','Freq')

# plot and save to jpeg
psu.rgn.cycl_plot <- ggplot(psu.rgn.cyc_lf.plt[psu.rgn.cyc_lf.plt$Cycle != 'NS',], aes(x=Cycle,y=Region, fill=Freq)) +
  geom_tile() +
  geom_text(aes(label=Freq),color="black",size=3) +
  scale_fill_gradient(low='white',high='slateblue') +
  scale_x_discrete(labels=c("Cycle 1","Cycle 2","Cycle 3")) +
  labs(fill = "PSU Count") +
  theme_minimal()

ggsave("./figures/psus_byRegionCycle.jpg", plot = psu.rgn.cycl_plot, width = 4, height = 5, dpi = 400, device = "jpeg")
#-------------------------------------------------------------------------------

# Summarize PSU plot count by Region and Year

# convert to LF
psu.plt.rgn.cyc_lf <- psu[,c('PSU_ID','Region','C1_plot_count','C2_plot_count','C3_plot_count')] %>%
  pivot_longer(
    cols = c(C1_plot_count,C2_plot_count,C3_plot_count),
    names_to = 'Cycle_VarName',
    values_to = 'plot_count'
  )

# convert plot count from character to integer
psu.plt.rgn.cyc_lf$plot_count <- as.integer(psu.plt.rgn.cyc_lf$plot_count)

# group data for plotting
psu.plt.rgn.cyc_lf.plot <- psu.plt.rgn.cyc_lf %>%
  group_by(Region,Cycle_VarName) %>%
  summarize(plt_mn = mean(plot_count,na.rm = TRUE),plt_sd=sd(plot_count,na.rm = TRUE))

# plot and save to jpeg
psu.plt.rgn.cyc_plt <- ggplot(psu.plt.rgn.cyc_lf.plot, aes(x=Cycle_VarName,y=Region, fill=plt_mn)) +
  geom_tile() +
  geom_text(aes(label=paste(round(plt_mn,1),'\u00B1',round(plt_sd,1))),color="black",size=3) +
  scale_fill_gradient(low='white',high='slateblue') +
  scale_x_discrete(labels=c("Cycle 1","Cycle 2","Cycle 3")) +
  labs(x='Cycle',fill = "Mean Plot Count (PSUs)") +
  theme_minimal()

ggsave("./figures/psus_plotCount_byRegionCycle.jpg", plot = psu.plt.rgn.cyc_plt, width = 6, height = 4, dpi = 400, device = "jpeg")
#-------------------------------------------------------------------------------

# Summarize PSU plot count by Cycle

# convert to LF
psu.plt.rgn.cyc_lf <- psu[,c('PSU_ID','Region','C1_plot_count','C2_plot_count','C3_plot_count')] %>%
  pivot_longer(
    cols = c(C1_plot_count,C2_plot_count,C3_plot_count),
    names_to = 'Cycle_VarName',
    values_to = 'plot_count'
  )

# convert plot count from character to integer
psu.plt.rgn.cyc_lf$plot_count <- as.integer(psu.plt.rgn.cyc_lf$plot_count)

# plot and save to jpeg
psu.plt.cyc_plt <- ggplot(psu.plt.rgn.cyc_lf, aes(x=Cycle_VarName,y=PSU_ID, fill=plot_count)) +
  geom_tile() +
  geom_text(aes(label=plot_count),color="black",size=3) +
  scale_fill_gradient(low='white',high='slateblue') +
  scale_x_discrete(labels=c("Cycle 1","Cycle 2","Cycle 3")) +
  labs(x='Cycle',fill = "Plot Count") +
  theme_minimal()

ggsave("./figures/psus_plotCount_byCycle.jpg", plot = psu.plt.cyc_plt, width = 4, height = 10, dpi = 400, device = "jpeg")
#-------------------------------------------------------------------------------


