# Ridge & Slough Landscape Panel Statistical Review

### Objectives & Goals
This is the work environment for the Ridge & Slough Landscape Panel Statistical 
Review of the Restoration, Coordination, Verification (RECOVER) for the 
Comprehensive Everglades Restoration Plan (CERP).

The purpose of the review is to .....

The repository is organized with the following folders structure:

### Repository Folder Structure

analysis: 
  (1) QAQC: data QAQC output for data review and error checks
  (2) tests_models: statistical tests and models (not descriptive stats of raw data visualization)

data:
  (1) raw_data: raw datasheets
  (2) processed:  cleaned and filtered data
  (3) spatial: spatial vector and raster data 

docs: supporting documents of sampling designs, field protocols, QAQC protocol

figures: data visualization and associated descriptive summary statistics
  root contains global summaries, sub folders contain individual units summaries (e.g., plot, psu, etc.)
  (1) at different focal scales (plot, cluster, psu, lsu) 
  (2) variables of interest (SOI = species of interest, COM = community, HAB = habitat)
  (3) environmental variables (ELE = elevation, WD = water depth, HP = hydroperiod)

scripts: scripts for QAQC, data visualization and statistical analysis
  env_data_summary.R - summary statistics of environmental data at various focal scales
  plot_level_veg_analysis.R - vegetation analysis at the plot level
  plot_QAQC.R - sampling design consistency and summary stats at different focal scales
  
_archive folders contain deprecated versions of scripts