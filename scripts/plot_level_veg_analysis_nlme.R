

# File name: vegetation_summary.R
# Author: Dan Gann, with extensive modifications by Jed Redwine
# email: gannd@fiu.edu, jredwine@fiu.edu

# Required: PSU sampling info (LP_R&S_C123_Yr1-5_PSU_Sampled.csv)...and more
#-------------------------------------------------------------------------------
library(sf)
library(terra)
library(tidyr)
library(dplyr)
library(ggplot2)
library(rstatix)
# library(Rmisc)
library(spdep)
library(nlme) 
library(gstat) 
library(sp)
library(DHARMa)
library(glmmTMB)


# ==============================================================================

# load PSU shapefile with final approved LS analysis PSUs
psu_sp <- sf::st_read('./data/spatial/PSU_RS_LS.shp')
x11()
psus_region <- ggplot(psu_sp) + 
  geom_sf(aes(fill = Region)) +
  geom_sf_text(aes(label = PSU_N), size = 3) +
  theme_minimal()
psus_region # plots the PSUs contained in the valid design and displays PSU number colored by region

ggsave("./figures/psus_x_region_nums.jpg", width = 6, height = 4, units = "in")


# PSU list
psu_list <- psu_sp$PSU_N
psu_list <- psu_list[order(psu_list)]
length(psu_list)

# ------------------------------------------------------------------------------
# load updated PSU data (modified Gann -- removed all PSUs outside of scope)
# JR - this is the same information as the file
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
#add Region designation to vegetation data - left join
veg_all$Region <- psu$Region[match (veg_all$PSUID, psu$PSU_ID)] 
veg_all$PSU <- substring(veg_all$PSUID,2,4)

  
# species list
sp_list <- as.data.frame(table(veg_all$SPCODE));names(sp_list) <- c('SPCODE','freq')

# write to table
# write.csv(sp_list,'./data/processed/C123_Yr1_5_ALL_SppCover_list_freq.csv') #only needed to be written once
# ------------------------------------------------------------------------------
# check PSU count and plot surveys
veg <- veg_all[veg_all$PSUID %in% psu_valid,] #filters veg information down to only observations within statistical design PSUs
nrow(veg)

table(veg$PSUID)
length(unique(veg$PSUID))

# species list
sp_list_des <- as.data.frame(table(veg$SPCODE));names(sp_list) <- c('SPCODE','freq')
# write to table
 # write.csv(sp_list_des,'./data/processed/C123_Yr1_5_ALL_SppCover_design_list_freq.csv') #only needed to be written once

# add Custer ID
veg$ClusterID <- substring(veg$PlotID,1,6)

# add plot ID
veg$PlotID_Cycle <- paste(veg$PlotID,veg$Cycle,sep='_') #this serves as unique identifier of each sampling plot

# convert cover to numeric
# develop a list of cover values and identify the frequency of their occurrence
# cover_val_list <- as.data.frame(table(veg$Cover));names(cover_val_list) <- c('Cover','freq') #used to detect odd values and get a rough sense of how values were collected
na_rows <- veg[veg$Cover == c("NA", "-999"),]
# write.csv(cover_val_list,'./data/raw_data/Cover_value_list_freq.csv') #only needed to be written once
veg_cln <-  veg[!is.na(veg$Cover) & veg$Cover != -999, ] # 1% of vegetation dataset (682 rows) were removed with this command

veg_cln$Cover <- as.integer(veg_cln$Cover) #this is now the dataset being used for subsequent analysis
head(veg_cln)


# ------------------------------------------------------------------------------
# # add mean cover by functional class
# # load functional type class table
fnc_tbl <- read.csv('./data/processed/spp_functional_types.csv')

# Values in df1$id that are NOT in df2$id
only_in_obs <- setdiff(veg_cln$SPCODE, fnc_tbl$SPCODE) #list of species only in observed veg data
only_in_obs
# write.csv(only_in_obs,'./data/raw_data/UK_SPCODE.csv') #only needed to be written once


# only_in_fnc_tbl <- setdiff(fnc_tbl$SPCODE, veg_cln$SPCODE) #list of species only in fnc_tbl
# only_in_fnc_tbl

#correct "Chara" and "SAGLAn" SPCODEs in observed data

veg_cln$SPCODE[veg_cln$SPCODE == "Chara"] <- "CHARA"
veg_cln$SPCODE[veg_cln$SPCODE == "SAGLAn"] <- "SAGLAN"

# Replace UK species codes with "UKSPP" - this affects 194 observations
veg_cln$SPCODE[veg_cln$SPCODE %in% only_in_obs] <- "UKSPP" # works


# # join functional class to vegetation data
veg_cln <- merge(veg_cln,fnc_tbl,by= "SPCODE", x.all=TRUE)
# ------------------------------------------------------------------------------

# load plot classification data
plot_cls <- read.csv('./data/processed/C123_Yr1_5_ALL Plots_locations_habs_smpl.csv')
#x11();plot(plot_cls)
head(plot_cls);nrow(plot_cls)
table(plot_cls$PSU,plot_cls$hab_09)

# identical(habs$C123_Plot_, plot_cls$C123_Plot_SNO) #determines that files contain identically sorted rows

plot_cls_hab <- as.data.frame(plot_cls[plot_cls$hab_09 != '',])
head(plot_cls_hab) #this is QC'd dataframe of siample locations and habitat characterizations will be fused with veg data subsequently
# ------------------------------------------------------------------------------

# Plotting and Analysis

#-------------------------------------------------------------------------------

# Summarize plot level species change by Plot, Habitat Type, PSU  and Region
#     -- loop over all species of interest

# set region of interest
rgn <- 'ENP'

# select sub-regions that match region of interest
rgn_cur <- rgn_valid[grepl(rgn, rgn_valid, fixed = TRUE)] 

# subset PSU list for ROI
psu_cur <- psu$PSU_ID[psu$Region %in% rgn_cur]

# subset plots for ROI
veg_cur <- veg_cln[veg_cln$PSUID %in% psu_cur,] 
head(veg_cur) 
# ------------------------------------------------------------------------------
plot_list_cycle <- as.data.frame(unique(veg_cur$PlotID_Cycle));names(plot_list_cycle) <- 'PlotID_Cycle'
plot_list_cycle$PlotID <- substr(plot_list_cycle$PlotID_Cycle,1,8)


# ------------------------------------------------------------------------------
# remove all plots other than cycle 1
cy_list <- c('C1','C2','C3')
fn_list <- unique(fnc_tbl$FNCODE)

# # # Generate combinations
  combos <- expand.grid(list(plot_list_cycle[,1],fn_list),stringsAsFactors = FALSE)
# 
# # join with underscore
 plot_cycle_fn_list <- as.data.frame(apply(combos, 1, paste, collapse = "_"));names(plot_cycle_fn_list) <- 'plotid_cycle_fn'
 plot_cycle_fn_list$PlotID <- substring(plot_cycle_fn_list$plotid_cycle_fn,1,8)
 head(plot_cycle_fn_list)

# merge habitat data to plot vegetation data 
plot_cycle_fn_list <- merge(plot_cycle_fn_list,plot_cls[c('PlotID_New','hab_09', 'hab_25', 'C3_X_NAD83','C3_Y_NAD83')], by.x='PlotID', by.y='PlotID_New')
head(plot_cycle_fn_list)
# ------------------------------------------------------------------------------
# ...
veg_cur$plotid_cycle_fn <- paste(veg_cur$PlotID_Cycle,veg_cur$FNCODE,sep='_')
 
# merge data frames outer left join
veg_anl <- merge(plot_cycle_fn_list,veg_cur[,c('plotid_cycle_fn','SPCODE', 'FNCODE','Cover','Region','Cluster')],by='plotid_cycle_fn',all.x=TRUE)
head(veg_anl,200)
#veg_anl$SPCODE <- substring(veg_anl$plotid_cycle_sp,13,18)
veg_anl$Cover[is.na(veg_anl$Cover)] <- 0
veg_anl$PSUID <- substring(veg_anl$plotid_cycle_fn,1,4)
#veg_anl$PlotID <- substring(veg_anl$plotid_cycle_fn,1,8)
veg_anl$ClusterID <- substring(veg_anl$plotid_cycle_fn,1,6)
veg_anl$Cycle <- substring(veg_anl$plotid_cycle_fn,10,11)
veg_anl <- veg_anl[!is.na(veg_anl$plotid_cycle_fn), ]


vg_mdl <- veg_anl %>%
  as.data.frame() %>% # Clear any hidden or corrupted group metadata  
  dplyr::group_by(plotid_cycle_fn) %>%
  dplyr::summarize(
    total_cover = sum(Cover, na.rm = TRUE),
  )

head(vg_mdl,30)




vg_mdl <- merge(vg_mdl,veg_anl,by='plotid_cycle_fn',all.x=TRUE)

vg_mdl <- vg_mdl[!is.na(vg_mdl$plotid_cycle_fn), ]

vg_mdl$Cover[is.na(vg_mdl$Cover)] <- 0
vg_mdl$PSUID <- substring(vg_mdl$plotid_cycle_fn,1,4)
vg_mdl$PlotID <- substring(vg_mdl$plotid_cycle_fn,1,8)
vg_mdl$ClusterID <- substring(vg_mdl$plotid_cycle_fn,2,6)
vg_mdl$Cluster <- as.numeric(substring(vg_mdl$plotid_cycle_fn,2,6))
vg_mdl$Cycle <- substring(vg_mdl$plotid_cycle_fn,10,11)
# vg_mdl$Cycle_ID <- as.numeric(substring(vg_mdl$Cycle,11))
vg_mdl$FNCODE <- substring(vg_mdl$plotid_cycle_fn,13)
vg_mdl <-  vg_mdl %>% distinct(plotid_cycle_fn, .keep_all = TRUE)
# vg_mdl <- vg_mdl[vg_mdl$Cycle_ID == 1,]  remove all plots other than cycle 1
vg_mdl <- vg_mdl %>%
  group_by(PlotID) %>%
  fill(Region, .direction = "downup") %>%
  ungroup()


# -------------------------------------------------------------------------
# STEP 1: Prep the database to match desired structure for building candidate 
# models using Maximum Likelihood (method = "ML"). We test different fixed effects
#  structures for predicting cover of ridge vegetation.
# -------------------------------------------------------------------------
vg_mdl.1 <- as.data.frame(vg_mdl)
class(vg_mdl.1) <- "data.frame"
attr(vg_mdl.1, "formula") <- NULL
attr(vg_mdl.1, "formulaList") <- NULL

vg_mdl.1 <- vg_mdl.1[!is.na(vg_mdl$C3_X_NAD83), ]
vg_mdl.1 <- vg_mdl.1[!is.na(vg_mdl$C3_Y_NAD83), ]

vg_mdl.new <- as.data.frame(vg_mdl.1) %>%
  filter(FNCODE %in% c("ridge", "slough", "transition")) %>%
  distinct(PlotID, Cycle, FNCODE, .keep_all = TRUE) %>%   # one row per plot x cycle x group
  mutate(
    PSU_u = interaction(Region, PSUID, drop = TRUE),
    Cl_u  = interaction(PSU_u, ClusterID, drop = TRUE),
    p     = pmin(total_cover, 100) / 100,
    y     = asin(sqrt(p))                                      # or qlogis() with an offset
  )
mean(vg_mdl.new$total_cover == 0)

plot_means <- vg_mdl.new %>%
  dplyr::group_by(FNCODE, Region, PSU_u, Cl_u, PlotID) %>%
  dplyr::summarise(
    y  = mean(y),
    x  = dplyr::first(C3_X_NAD83),
    yc = dplyr::first(C3_Y_NAD83),
    .groups = "drop"
  ) %>%
  as.data.frame() #doesn't work
table(vg_mdl.new$Cycle)

<<<<<<< HEAD
=======
# vc_fit <- function(d) {
#   m0 <- gls(y ~ 1, data = vg_mdl.new, method = "REML")
#   m1 <- lme(y ~ 1, random = ~ 1 | PSU_u,         data = vg_mdl.new, method = "REML")
#   m2 <- lme(y ~ 1, random = ~ 1 | PSU_u/Cl_u,    data = vg_mdl.new, method = "REML")
#    list(tests = anova(m0, m1, m2), m2 = m2)
# }
# groups <- c("ridge", "slough", "transition")
# res <- lapply(groups, function(g) vc_fit(subset(plot_means, FNCODE == g)))
>>>>>>> 8157609bf5dd06584f01c6847b3ca4a2ef67bf77

vg_mdl.new$p   <- pmin(pmax(vg_mdl.new$total_cover, 0), 100) / 100
vg_mdl.new$p_b <- pmin(vg_mdl.new$p, 0.999)
vg_mdl.new <- vg_mdl.new[!is.na(vg_mdl.new$p_b), ]

stopifnot(all(vg_mdl.new$p_b >= 0 & vg_mdl.new$p_b < 1))

#model fitting process compares a set of models of increasing complexity to address two distinct questions:
# 1. Do the spatial groupings explain variation in mean cover (random intercepts)?
# 2. Do they explain variation in change (random cycle slopes)? 
#
#
# Cycle is your change term. With only 3 time points, treat it as a fixed factor, so you get 
# cycle-to-cycle contrasts without assuming linearity. The plot random intercept handles the 
# repeated measures.

########################################################################################
fit_group <- function(g) {
  d <- droplevels(subset(vg_mdl.new, FNCODE == g))
  f <- list(family = beta_family(link = "logit"), ziformula = ~ 1, data = d)
  
  # 1. random-intercept structure (spatial levels of mean cover)
  m0 <- glmmTMB(p_b ~ Region * Cycle + (1 | PlotID), family = beta_family(link = "logit"), 
                  ziformula = ~1, data = vg_mdl.new)
  m1 <- update(m0, . ~ . + (1 | PSU_u))
  m2 <- update(m1, . ~ . + (1 | Cl_u))
  
  # 2. fixed effects of change (refit best random structure)
  m_add <- update(m2, . ~ . - Region:Cycle)       # no Region x Cycle
  m_noc <- update(m_add, . ~ . - Cycle)           # no Cycle at all
  
  list(data = d, m0 = m0, m1 = m1, m2 = m2, m_add = m_add, m_noc = m_noc)
}

groups <- c("ridge", "slough", "transition")
res <- lapply(groups, fit_group); names(res) <- groups #this works 
lapply(res, function(r) anova(r$m0, r$m1, r$m2))      # do PSU and Cluster matter?
lapply(res, function(r) anova(r$m_noc, r$m_add, r$m2)) # Cycle effect, then Region x Cycle
lapply(res, function(r) VarCorr(r$m2))

#Reporting Change
library(emmeans)
emm <- emmeans(res$ridge$m_add, ~ Cycle, type = "response")
pairs(emm)                                          # C1 vs C2, C2 vs C3, C1 vs C3

emm_r <- emmeans(res$ridge$m2, ~ Cycle | Region, type = "response")
pairs(emm_r)                                        # region-specific change results seem reasonable

#Plot the analytical design
#1. Regions: bars by habitat, coloured by functional group
library(dplyr); library(ggplot2); library(forcats)

fn_cols <- c(ridge = "#2E7D32", slough = "#1976D2", transition = "#F9A825")
<<<<<<< HEAD
lvh <- c("ridge", "slough", "trans")
=======
lvh <- c("ridge", "slough", "transition")
>>>>>>> 8157609bf5dd06584f01c6847b3ca4a2ef67bf77
lvf <- c("ridge", "slough", "transition", "UK")

plot_cov <- vg_mdl.new %>%
  dplyr::filter(hab_09 %in% lvh, FNCODE %in% lvf) %>%
  dplyr::group_by(Region, PSU_u, ClusterID, Cl_u, PlotID, hab_09, FNCODE) %>%
  dplyr::summarise(cover = mean(total_cover, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(FNCODE = factor(FNCODE, levels = lvf),
<<<<<<< HEAD
         hab_09 = factor(hab_09, levels = lvh))
=======
         hab_09 = factor(hab_09, levels = lvh), Cycle  = factor(Cycle))
>>>>>>> 8157609bf5dd06584f01c6847b3ca4a2ef67bf77

summ_level <- function(df, level) {
  df %>%
    dplyr::group_by(across(all_of(c(level, "hab_09", "FNCODE")))) %>%
    dplyr::summarise(n_plots = n(), mean = mean(cover), se = sd(cover) / sqrt(n_plots),
              .groups = "drop")
}
#####cover by cycle
plot_cov_cyc <- vg_mdl.new %>%
  dplyr::filter(hab_09 %in% lvh, FNCODE %in% lvf) %>%
  dplyr::group_by(Region, PSU_u, ClusterID,  Cl_u, PlotID, hab_09, FNCODE, Cycle) %>%
  dplyr::summarise(cover = mean(total_cover, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(FNCODE = factor(FNCODE, levels = lvf),
                hab_09 = factor(hab_09, levels = lvh),
                Cycle  = factor(Cycle))

summ_level_cyc <- function(df, level) {
  df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(level, "hab_09", "FNCODE", "Cycle")))) %>%
    dplyr::summarise(n_plots = dplyr::n(), mean = mean(cover),
                     se = sd(cover) / sqrt(n_plots), .groups = "drop")
}
table(plot_cov_cyc$Cycle)
vg_mdl.new %>% dplyr::distinct(PlotID, Cycle, hab_09) %>%
  dplyr::count(PlotID, hab_09) %>% dplyr::count(n)   # most plots should have n = 3


p_region_cyc <- summ_level_cyc(plot_cov_cyc, "Region") %>%
  ggplot(aes(Cycle, mean, colour = FNCODE, group = FNCODE)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.8) +
  geom_errorbar(aes(ymin = pmax(mean - se, 0), ymax = mean + se),
                width = 0.15, na.rm = TRUE) +
  facet_grid(hab_09 ~ Region) +
  scale_colour_manual(values = fn_cols, name = "Species functional group") +
  labs(x = "Cycle", y = "Mean percent cover (%)",
       subtitle = "Rows = habitat (hab_09); columns = region") +
  theme_bw() +
  theme(legend.position = "bottom",
        strip.text.x = element_text(size = 7))
p_region_cyc

<<<<<<< HEAD
ggsave(paste0('./figures/func_hab/Ftype_by_hab_in_RegionXcycle.jpeg'), 
=======
ggsave(paste0('./figures/func_hab/Ftyope_by_hab_in_RegionXcycle.jpeg'), 
>>>>>>> 8157609bf5dd06584f01c6847b3ca4a2ef67bf77
       plot = p_region_cyc, width = 10, height = 10, dpi = 400, device = "jpeg")

#####################
2. Cover by Cluster and Cycle
cl_means_cyc <- plot_cov_cyc %>%
  dplyr::group_by(Region, PSU_u, ClusterID, Cl_u, hab_09, FNCODE, Cycle) %>%
  dplyr::summarise(cover = mean(cover), n_plots = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n_plots >= 2)

p_cluster_cyc <- ggplot(cl_means_cyc, aes(Cycle, cover, fill = FNCODE)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6,
               position = position_dodge(0.8), width = 0.7) +
  geom_point(aes(colour = FNCODE),
             position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8),
             size = 0.5, alpha = 0.3, show.legend = FALSE) +
  facet_wrap(~ hab_09, nrow = 1) +
  scale_fill_manual(values = fn_cols, name = "Species functional group") +
  scale_colour_manual(values = fn_cols) +
  labs(x = "Cycle", y = "Cluster-mean percent cover (%)") +
  theme_bw() + theme(legend.position = "bottom")
p_cluster_cyc

# by region as well
p_cluster_cyc_reg <- p_cluster_cyc + facet_grid(hab_09 ~ Region)
p_cluster_cyc_reg
<<<<<<< HEAD
out_dir <- "./figures/func_hab"

ggsave(file.path(out_dir, "cover_by_cluster_cycle.png"),
       plot = p_cluster_cyc, width = 9, height = 5, dpi = 300)
ggsave(file.path(out_dir, "cover_by_cluster_cycle_region.png"),
       plot = p_cluster_cyc_reg, width = 16, height = 7, dpi = 300)


=======

ggsave(file.path(out_dir, "cover_by_cluster_cycle.png"),
       p_cluster_cyc, width = 9, height = 5, dpi = 300)
ggsave(file.path(out_dir, "cover_by_cluster_cycle_region.png"),
       p_cluster_cyc_reg, width = 16, height = 7, dpi = 300)
>>>>>>> 8157609bf5dd06584f01c6847b3ca4a2ef67bf77

################################


# the PSUs actually present in your data (avoids unused factor levels)
psu_list <- sort(unique(as.character(cl_means_cyc$PSU_u)))
length(psu_list)    # should be 11

# unfiltered cluster means, so min_plots is applied inside the function
cl_means_all <- plot_cov_cyc %>%
  dplyr::group_by(Region, PSU_u, ClusterID, Cl_u, hab_09, FNCODE, Cycle) %>%
  dplyr::summarise(cover = mean(cover), n_plots = dplyr::n(), .groups = "drop")

plot_psu_cycle <- function(psu, min_plots = 2, ymax = NULL) {
  d <- cl_means_all %>%
    dplyr::filter(as.character(PSU_u) == psu, n_plots >= min_plots)
  
  ggplot(d, aes(Cycle, cover, fill = FNCODE)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.6,
                 position = position_dodge(0.8), width = 0.7) +
    geom_point(aes(colour = FNCODE),
               position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8),
               size = 0.8, alpha = 0.5, show.legend = FALSE) +
    facet_wrap(~ hab_09, nrow = 1) +
    scale_fill_manual(values = fn_cols, name = "Species functional group") +
    scale_colour_manual(values = fn_cols) +
    coord_cartesian(ylim = c(0, 100)) +
    labs(title = paste0("PSU ", psu, "  (Region: ", unique(d$Region)[1], ")"),
         subtitle = paste0(dplyr::n_distinct(d$Cl_u), " clusters shown; cells with < ",
                           min_plots, " plots omitted"),
         x = "Cycle", y = "Cluster-mean percent cover (%)") +
    theme_bw() + theme(legend.position = "bottom")
}

# test one
plot_psu_cycle(psu_list[1])

y_max <- 100

pdf(file.path(out_dir, "cluster_cover_by_psu_cycle.pdf"), width = 9, height = 5)
for (p in psu_list) print(plot_psu_cycle(p, ymax = y_max))
dev.off()

for (p in psu_list) {
  ggsave(file.path(out_dir, paste0("cluster_cover_cycle_PSU_", p, ".png")),
         plot_psu_cycle(p, ymax = y_max), width = 9, height = 5, dpi = 300)
}

#See each PSUs trajectory
plot_psu_traj <- function(psu, min_plots = 2) {
  d <- cl_means_all %>%
    dplyr::filter(as.character(PSU_u) == psu, n_plots >= min_plots)
  
  ggplot(d, aes(Cycle, cover, group = Cl_u, colour = FNCODE)) +
    geom_line(alpha = 0.5) + geom_point(size = 1) +
    facet_grid(hab_09 ~ FNCODE) +
    scale_colour_manual(values = fn_cols, guide = "none") +
    labs(title = paste0("PSU ", psu), x = "Cycle", y = "Cluster-mean percent cover (%)") +
    theme_bw()
}
plot_psu_traj(psu_list[1])
for (p in psu_list) {
  ggsave(file.path(out_dir, paste0("cluster_cover_cycle_PSU_traj", p, ".png")),
         plot_psu_traj(p), width = 9, height = 5, dpi = 300)
}
<<<<<<< HEAD
pdf(file.path(out_dir, "cluster_cover_by_psu_cycle_PSU_traj.pdf"), width = 9, height = 5)
for (p in psu_list) print(plot_psu_traj(p))
dev.off()

=======
>>>>>>> 8157609bf5dd06584f01c6847b3ca4a2ef67bf77
####################
#2. PSUs: heatmap with one row per PSU

psu_sum <- summ_level(plot_cov, "PSU_u") %>%
  dplyr::left_join(distinct(plot_cov, PSU_u, Region), by = "PSU_u") %>%
  dplyr::mutate(combo = interaction(hab_09, FNCODE, sep = " | ", lex.order = TRUE),
         PSU_u = fct_rev(fct_inorder(as.character(PSU_u))))

p_psu <- ggplot(psu_sum, aes(combo, PSU_u, fill = mean)) +
  geom_tile(colour = "white", linewidth = 0.2) +
  facet_grid(Region ~ ., scales = "free_y", space = "free_y") +
  scale_fill_viridis_c(name = "Mean cover (%)", option = "C", na.value = "grey90") +
  labs(x = "Habitat | species functional group", y = "PSU") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 5),
        strip.text.y = element_text(angle = 0, size = 7),
        legend.position = "right")
p_psu
<<<<<<< HEAD
ggsave(file.path(out_dir, "cover_by_psu.png"),  plot =p_psu,     width = 8,  height = 12, dpi = 300)
=======
>>>>>>> 8157609bf5dd06584f01c6847b3ca4a2ef67bf77

#####################
#3. Clusters: how to show 16 clusters × 53 PSUs

cl_means <- plot_cov %>%
  dplyr::group_by(Region, PSU_u, ClusterID, Cl_u, hab_09, FNCODE) %>%
  dplyr::summarise(cover = mean(cover), n_plots = n(), .groups = "drop") %>%
  dplyr::filter(n_plots >= 2)

p_cluster <- ggplot(cl_means, aes(hab_09, cover, fill = FNCODE)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6,
               position = position_dodge(0.8), width = 0.7) +
  geom_point(aes(colour = FNCODE),
             position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8),
             size = 0.5, alpha = 0.35, show.legend = FALSE) +
  scale_fill_manual(values = fn_cols, name = "Species functional group") +
  scale_colour_manual(values = fn_cols) +
  labs(x = "Habitat (hab_09)", y = "Cluster-mean percent cover (%)") +
  theme_bw() + theme(legend.position = "bottom")

p_cluster + facet_wrap(~ Region, ncol = 5)
<<<<<<< HEAD
ggsave(file.path(out_dir, "cover_by_cluster.png"),
       p_cluster + facet_wrap(~ Region, ncol = 5),
       width = 12, height = 6, dpi = 300)
graphics.off()
=======

>>>>>>> 8157609bf5dd06584f01c6847b3ca4a2ef67bf77

plot_psu_clusters <- function(psu) {
  plot_cov %>%
    dplyr::filter(PSU_u == psu) %>%
    dplyr::group_by(ClusterID, hab_09, FNCODE) %>%
    dplyr::summarise(cover = mean(cover), n_plots = n(), .groups = "drop") %>%
    ggplot(aes(hab_09, factor(ClusterID), fill = cover)) +
    geom_tile(colour = "white") +
    geom_text(aes(label = n_plots), size = 2.5, colour = "white") +
    facet_wrap(~ FNCODE, nrow = 1) +
    scale_fill_viridis_c(name = "Mean cover (%)", option = "C", na.value = "grey90",
                         limits = c(0, 100)) +
    labs(title = paste("PSU", psu), x = "Habitat (hab_09)", y = "Cluster",
         subtitle = "Number = plots in the cluster with that habitat") +
    theme_bw()
}

plot_psu_clusters(levels(plot_cov$PSU_u)[1])

# save one page per PSU
<<<<<<< HEAD
# out_dir <- "./figures/func_hab"
# dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
=======
out_dir <- "./figures/func_hab"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
>>>>>>> 8157609bf5dd06584f01c6847b3ca4a2ef67bf77

pdf(file.path(out_dir, "clusters_by_psu.pdf"), width = 9, height = 6)
for (p in levels(plot_cov$PSU_u)) print(plot_psu_clusters(p))
dev.off()

#save plots#
<<<<<<< HEAD
ggsave(file.path(out_dir, "cover_by_cluster.png"),
       p_cluster + facet_wrap(~ Region, ncol = 5),
       width = 12, height = 6, dpi = 300)
dev.off()
=======
ggsave(file.path(out_dir, "cover_by_region.png"),  p_region,  width = 12, height = 6,  dpi = 300)
ggsave(file.path(out_dir, "cover_by_psu.png"),     p_psu,     width = 8,  height = 12, dpi = 300)
ggsave(file.path(out_dir, "cover_by_cluster.png"),
       p_cluster + facet_wrap(~ Region, ncol = 5),
       width = 12, height = 6, dpi = 300)

>>>>>>> 8157609bf5dd06584f01c6847b3ca4a2ef67bf77
list.files(out_dir)

#DHARMa model checks
check_group <- function(g) {
  r   <- res[[g]]
  d   <- r$data[complete.cases(r$data[, c("p_b","Region","Cycle","PSU_u","Cl_u","PlotID")]), ]
  sim <- simulateResiduals(r$m2, n = 500, seed = 1)
  stopifnot(nrow(d) == length(sim$scaledResiduals))
  
  plot(sim, title = g, quantreg = TRUE)
  print(testDispersion(sim, plot = FALSE))
  print(testZeroInflation(sim, plot = FALSE))
  plotResiduals(sim, factor(d$Region), main = paste(g, "- by Region"))
  plotResiduals(sim, factor(d$Cycle),  main = paste(g, "- by Cycle"))
  invisible(sim)
}

sims <- lapply(groups, check_group); names(sims) <- groups

# percent of variance at each level - this isn't working yet
pct <- function(m) {
  v <- suppressWarnings(as.numeric(VarCorr(res)[, "Variance"])); v <- v[!is.na(v)]
  setNames(round(100 * v / sum(v), 1), c("PSU", "Cluster", "Residual (plot)"))
}
sapply(res, function(m) pct(res$m2))

#check whether any fine-scale spatial structure remains
# m_s <- res[["ridge"]]$m2
d_r <- subset(plot_means, FNCODE == "ridge")
d_r <- droplevels(d_r)

m_s <- lme(y ~ 1, random = ~ 1 | PSU_u/Cl_u, data = d_r, method = "REML")

# d_r <- plot_means[plot_means$FNCODE == "ridge", ]

plot(Variogram(m_s, form = ~ x + yc, resType = "normalized", data = d_r,
               maxDist = 6000, robust = TRUE))  #works!




m_exp <- update(m_s, correlation = corExp(form = ~ x + yc, nugget = TRUE))
m_sph <- update(m_s, correlation = corSpher(form = ~ x + yc, nugget = TRUE))
AIC(m_s, m_exp, m_sph)


# plot( vg_mdl.new ) # trellis plot by Subject formula( vg_mdl.new ) 

fm1 <- lme( vg_mdl.new ) # fixed and groups formulae extracted from object 
fm1



# # stats summary collector
# stats.df <- data.frame(PSU = character(), SPCODE=character(), n = integer(),stat = numeric(),df=integer(),p=numeric())

# ------------------------------------------------------------------------------
# test for spatial autocorrelation to test assumption that samples could have been drawn with simple random procedure
soi <- "CLAJAM"

# for (PSU in psu_cur){
#   veg_anl_PSU <- veg_anl[veg_anl$PSUID == PSU,]
#   head(veg_anl_PSU)
  
  # test for spatial autocorrelation to test assumption that samples could have been drawn with simple random procedure
  for (cy in cy_list){
    
    soi_PSU_cy <- veg_anl_PSU[veg_anl_PSU$SPCODE == soi & veg_anl_PSU$Cycle == cy,]
    if(nrow(soi_PSU_cy) > 5){
      
      mdl_0 <- lm(Cover ~ 1, data = soi_PSU_cy)
      
      # spatial weights matrix
      suppressWarnings({nb_points <- knn2nb(knearneigh(soi_PSU_cy[, c("C3_X_NAD83", "C3_Y_NAD83")], k = 7))})
      listw_points <- nb2listw(nb_points, style = "W")
      
      # test spatial autocorrelation in residuals Null: no spatial autocorrelation
      test_stat <- lm.morantest(mdl_0, listw_points)
      print(paste0("FOr ",soi," in ",PSU," and Cycle",cy,", p-value for M-I is:",test_stat$p.value))
    }
  }
}
# ------------------------------------------------------------------------------
# for PSU of interest and species of interest (SOI) plot habitat specific species abundance trends
for (PSU in psu_cur){
  veg_mdl.new_PSU <- veg_mdl.new[veg_mdl.new$PSUID == PSU,]
  head(veg_mdl.new_PSU)
  
  # summarySE provides the standard deviation, standard error of the mean, and a (default 95%) confidence interval
  stats <- summarySE(veg_anl_PSU, measurevar="Cover", groupvars=c("Cycle","SPCODE","hab_09"))
 
  p <- ggplot(veg_anl_PSU, aes(x = Cycle, y = total_cover, group = hab_09, color=hab_09)) +
    geom_line(aes(group = PlotID), colour = "grey70", alpha = 0.7) +
    geom_point(aes(group = PlotID), colour = "grey30", size = 1.5) +
    #geom_errorbar() +
    stat_summary(fun = mean, geom = "point",size = 3.5) +
    stat_summary(fun = mean, geom = "line", size = 1.2) +
    labs(title = paste("Species Cover Change in PSU",PSU),
         x = "Cycle", y = "Cover") +
    theme_minimal(base_size = 13) +
    facet_wrap(~ SPCODE)

  # plot change of SOI of plot by PSU
  ggsave(paste0('./figures/SOI_trend_by_PSU/v2/',PSU,'_SOI_trend.jpg'), 
         plot = p, width = 10, height = 10, dpi = 400, device = "jpeg")
  
  # write summary stats to file
  write.csv(stats,paste0('./figures/SOI_trend_by_PSU/v2/',PSU,'_SOI_trend_stats_v2.csv'))

  # # for species of interest (SOI) test for significance of change
  # for (soi in soi_list){
  #   
  #   # remove unreplicated plots
  #   with(veg_soi_cur, table(PlotID, Cycle))
  #   
  #   print(soi)
  #   veg_soi_cur <- veg_anl_PSU[veg_anl_PSU$SPCODE == soi,]
  #   head(veg_soi_cur)
  # 
  #   veg_soi_cur$Cover_prp <- veg_soi_cur$Cover/100
  #   
  #   # test difference by PSU --> which test? and at what level?
  #   soi_mdl.stats <- test|model(Cover ~ Cycle | PlotID)
  #   
  #   stats.df <- rbind(stats.df, c(PSU,soi,soi_mdl.stats$n,soi_mdl.stats$statistic,soi_mdl.stats$df,soi_mdl.stats$p))
  # }
}
# # write stats summary table to file
# write.csv(stats.df,paste0('./figures/SOI_trend_by_PSU/SOI_trend_PlotID_mdl',rgn,'.csv'))


## EOF








