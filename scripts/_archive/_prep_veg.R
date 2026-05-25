# File name: prep_veg.R
# Author: S. Bogen
# Requires all files in the 'data/Raw Vegetation Data' subdirectory
# Generates the file 'processed/vegData.csv'

#-Load required libraries-------------------------------------------------------

library(readxl)
library(dplyr)
library(purrr)

#-Read in raw vegetation cover data---------------------------------------------

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

cy1.veg <- read_xlsx(cy1.file, sheet = "C1Y1_5 Species Cover_Final")
cy2.veg <- read_xlsx(cy2.file, sheet = "C2_Yr1_5 Species Cover_Final")
cy3.yr1and2.veg <- read_xlsx(cy3.yr1and2.file, sheet = "C3Y1_2 Species Cover_Final")
cy3.yr3.veg <- read_xlsx(cy3.yr3.file, sheet = "C3Yr3_Species Cover")
cy3.yr4.veg <- read_xlsx(cy3.yr4.file, sheet = "C3Y4 Species Cover_Final")
cy3.yr5.veg <- read_xlsx(cy3.yr5.file, sheet = "C3Y5 Species Cover_Final")

# Clean up space
rm(list = ls(pattern = "\\.file$"))
rm(cy3.dir, path)

#-Standardize columns-----------------------------------------------------------

names(cy1.veg)
names(cy2.veg)
names(cy3.yr1and2.veg)
names(cy3.yr3.veg)
names(cy3.yr4.veg)
names(cy3.yr5.veg)

# Remove serial numbers:
cy1.veg <- cy1.veg[, -1]
cy2.veg <- cy2.veg[, 5:20]
cy3.yr1and2.veg <- cy3.yr1and2.veg[, 5:20]
cy3.yr3.veg <- cy3.yr3.veg[, 5:20]
cy3.yr4.veg <- cy3.yr4.veg[, 5:20]
cy3.yr5.veg <- cy3.yr5.veg[, 7:22]

# Remove unneeded columns - Retain only PlotID, PSUCycle, Species, Cover Final
names(cy1.veg)
cy1.veg <- cy1.veg[, c("PlotID_New", "Cyle", "SPCODE", "Cover")]
names(cy2.veg)
cy2.veg <- cy2.veg[, c("PlotID", "PSUCycle", "Species", "Cover Final")]
names(cy3.yr1and2.veg)
cy3.yr1and2.veg <- cy3.yr1and2.veg[, c("PlotID", "PSU_Cycle", 
                                       "Species", "Cover Final")]
names(cy3.yr3.veg)
cy3.yr3.veg <-  cy3.yr3.veg[, c("PlotID", "PSU_Cycle", "Species", "Cover Final")]
names(cy3.yr4.veg)
cy3.yr4.veg <- cy3.yr4.veg[, c("PlotID", "PSU_Cycle", "Species", "Cover Final")]
names(cy3.yr5.veg)
cy3.yr5.veg <- cy3.yr5.veg[, c("PlotID", "PSU_Cycle", "Species", "Cover Final")]

# Rename columns
names(cy1.veg) <- c("PlotID", "PSU_Cycle", "SPCODE", "Cover")
names(cy2.veg) <- c("PlotID", "PSU_Cycle", "SPCODE", "Cover")
names(cy3.yr1and2.veg) <- c("PlotID", "PSU_Cycle", "SPCODE", "Cover")
names(cy3.yr3.veg) <- c("PlotID", "PSU_Cycle", "SPCODE", "Cover")
names(cy3.yr4.veg) <- c("PlotID", "PSU_Cycle", "SPCODE", "Cover")
names(cy3.yr5.veg) <- c("PlotID", "PSU_Cycle", "SPCODE", "Cover")

#-Combine all data frames-------------------------------------------------------

result <- rbind(cy1.veg, cy2.veg, cy3.yr1and2.veg, cy3.yr3.veg,
                cy3.yr4.veg, cy3.yr5.veg)

#-Verify Keys and data types----------------------------------------------------

dim(unique(result[, c("PlotID", "PSU_Cycle", "SPCODE")]))

dups_all <- result[
  duplicated(result[, c("PlotID", "PSU_Cycle", "SPCODE")]) |
    duplicated(result[, c("PlotID", "PSU_Cycle", "SPCODE")], fromLast = TRUE),
]

dim(dups_all)

# It is not clear from existing documentation where these duplicates came from.

#-Save result-------------------------------------------------------------------

write.csv(result, file = "data/processed/vegData.csv")
