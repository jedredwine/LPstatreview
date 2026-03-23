# File name: prep_plots.R
# Author: S. Bogen
# Requires all files in the 'data/Raw Vegetation Data' subdirectory
# Generates the file 'processed/plots.csv'

#-Import required libraries-----------------------------------------------------

library(readxl)
library(dplyr)
library(purrr)

#-Import raw plot data----------------------------------------------------------

# define path
path <- "data/Raw Vegetation Data"

# define file names
cy1.file <- file.path(path, "PSU_C1_Yr1_5 (2009-2015)/W912HZ1020030_PSU_C1_Yr1_5_(2009-2015)_ALL_Data_ 2024.02.28.xlsx")
cy2.file <- file.path(path, "PSU_C2_Yr1_5 (2015-2020)/W912HZ1520027_PSU_C2_Yr1_5_(2015-2020)_ALL_Data_2024.02.28.xlsx")
cy3.dir <- file.path(path, "PSU_C3_Yr1_5 (2020-2025)")
cy3.yr1and2.file <- file.path(cy3.dir, "W912HZ2020038_PSU_C3_Yr1_2_(2020-2022)_ALL Data_2023.02.28.xlsx")
cy3.yr3.file <- file.path(cy3.dir, "W912HZ2020038_PSU_C3_Yr3_ALL Data_for_Geodatabase_2024.03.20.xlsx")
cy3.yr4.file <- file.path(cy3.dir, "W912HZ2020038_PSU_C3_Yr4_ALL_Data_for_Geodatabase_2025.09  V2_Prov..xlsx")
cy3.yr5.file <- file.path(cy3.dir, "W912HZ2020038_PSU_C3_Yr5_ALL_Data_for_Geodatabase_2025.09.30_Prov..xlsx")

# Some cycles store plots info in a sheet called plots_sampled, others store
# in plots_coordinates, others have both. The plots_coordinates sheet was used 
# unless it is not available.

# The warnings that appear below come from text values in the "PSU" column,
# which read_xlsx expects to be numeric. Column is omitted since it is
# redundant with the "PSU_ID"column.

cy1.plots <- read_xlsx(cy1.file, sheet = "PSU_C1_Y1_5 Plots_Coordinates")
cy2.plots <- read_xlsx(cy2.file, sheet = "PSU_C2_Yr1_5_Plots_Sampled")
cy3.yr1and2.plots <- read_xlsx(cy3.yr1and2.file, sheet = "PSU_C3Y1_2 Plots_Coordinates")
cy3.yr3.plots <- read_xlsx(cy3.yr3.file, sheet = "C3Yr3_Plot Sampled-Coordinates")
cy3.yr4.plots <- read_xlsx(cy3.yr4.file, sheet = "PSU_C3Y4 Plots_Coordinates")
cy3.yr5.plots <- read_xlsx(cy3.yr5.file, sheet = "PSU_C3Yr5_Plots_Coordinates")

# Clean up space
rm(list = ls(pattern = "\\.file$"))
rm(cy3.dir, path)

#-Standardize column names------------------------------------------------------

# Remove serial number columns from all cycles
cy1.plots <- cy1.plots[, 2:13]
cy2.plots <- cy2.plots[, 4:15]
cy3.yr1and2.plots <- cy3.yr1and2.plots[, 4:14]
cy3.yr3.plots <- cy3.yr3.plots[, 4:15]
cy3.yr4.plots <- cy3.yr4.plots[, 5:16]
cy3.yr5.plots <- cy3.yr5.plots[, 5:16]

# Omit Site Sampled? columns
cy2.plots <- cy2.plots[, 1:11]
cy3.yr3.plots <- cy3.yr3.plots[, 1:11]
cy3.yr4.plots <- cy3.yr4.plots[, 1:11]
cy3.yr5.plots <- cy3.yr5.plots[, 1:11]

# Check Cycle 3 names
names(cy3.yr1and2.plots) == names(cy3.yr3.plots)
names(cy3.yr1and2.plots) == names(cy3.yr4.plots)
names(cy3.yr1and2.plots) == names(cy3.yr5.plots)

# Standardize Cycle 3 names
names(cy3.yr4.plots)[1] <- "PSU_Cycle"
names(cy3.yr4.plots)[2] <- "PSU_Year"

# Coerce correct value for PSU_Year columns based on file names
cy3.yr4.plots$PSU_Year <- rep(4, 1755)

# Combine Cycle 3 data into one frame
cy3.plots <- rbind(cy3.yr1and2.plots, cy3.yr3.plots, 
                   cy3.yr4.plots, cy3.yr5.plots)
rm(cy3.yr1and2.plots, cy3.yr3.plots, cy3.yr4.plots, cy3.yr5.plots)

# Standardize columns of Cycle 1 to other two cycles - omit PlotID_Old,
# re-order, and re-name columns
cy1.plots <- cy1.plots[, c("PSU_Cycle", "Cycle_Year", "PSUID", "PlotID_New",
                           "Cluster", "PlotLetter", "EastNAD83", "NorthNAD83", "SampDate")]

# remove PSU number and Cycle_PlotID from cycles 2 and 3
cy2.plots <- cy2.plots[, -c(5, 8)]
cy3.plots <- cy3.plots[, -c(5, 8)]

# standardize column names across all three cycles
names(cy2.plots)[1:2] <- c("PSU_Cycle", "PSU_Year")
names(cy1.plots) <- names(cy2.plots)
names(cy3.plots) <- names(cy2.plots)

#-Rename Cycle-specific columns-------------------------------------------------

names(cy1.plots)[2] <- "Cy1_SampYear"
names(cy2.plots)[2] <- "Cy2_SampYear"
names(cy3.plots)[2] <- "Cy3_SampYear"

names(cy1.plots)[9] <- "Cy1_SampDate"
names(cy2.plots)[9] <- "Cy2_SampDate"
names(cy3.plots)[9] <- "Cy3_SampDate"

# grab cycle-specific columns and ID
cy1.spec <- cy1.plots[, c("PlotID", "Cy1_SampYear", "Cy1_SampDate")]
cy2.spec <- cy2.plots[, c("PlotID", "Cy2_SampYear", "Cy2_SampYear")]
cy3.spec <- cy3.plots[, c("PlotID", "Cy3_SampYear", "Cy3_SampYear")]



#-Deal with common columns------------------------------------------------------

commonCols <- rbind(cy1.plots[, 3:8], 
                    cy2.plots[, 3:8],
                    cy3.plots[, 3:8]) |> unique()

#-Save results------------------------------------------------------------------

# Ensure expected data types first!





# Check whether columns are equivalent


