# Ridge & Slough Landscape Panel Statistical Review

### Objectives & Goals
This is the work environment for the Ridge & Slough Landscape Panel Statistical 
Review of the Restoration, Coordination, Verification (RECOVER) for the 
Comprehensive Everglades Restoration Plan (CERP).<br>

The purpose of the review is to .....<br>

The repository is organized with the following folders structure:<br>

### Repository Folder Structure

#### analysis: 
  <p style="text-indent: 2em;">
  (1) QAQC: data QAQC output for data review and error checks<br>
  (2) tests_models: statistical tests and models (not descriptive stats of raw data visualization)<br>

#### data:
  <p style="text-indent: 2em;">(1) raw_data: raw datasheets<br>
   <p style="text-indent: 2em;">(2) processed:  cleaned and filtered data<br>
   <p style="text-indent: 2em;">(3) spatial: spatial vector and raster data<br>

#### docs: supporting documents of sampling designs, field protocols, QAQC protocol

#### figures: data visualization and associated descriptive summary statistics. The root contains global summaries, sub folders contain individual units summaries (e.g., plot, psu, etc.)<br>
 <p style="text-indent: 2em;">(1) at different focal scales (plot, cluster, psu, lsu)<br>
 <p style="text-indent: 2em;">(2) variables of interest (SOI = species of interest, COM = community, HAB = habitat)<br>
 <p style="text-indent: 2em;">(3) environmental variables (ELE = elevation, WD = water depth, HP = hydroperiod)<br>

#### scripts: scripts for QAQC, data visualization and statistical analysis
<p style="text-indent: 2em;">(1) env_data_summary.R - summary statistics of environmental data at various focal scales<br>
<p style="text-indent: 2em;">(2) plot_level_veg_analysis.R - vegetation analysis at the plot level<br>
<p style="text-indent: 2em;">(3) plot_QAQC.R - sampling design consistency and summary stats at different focal scales<br>
  
#### _archive folders 
contain deprecated versions of scripts and data files
