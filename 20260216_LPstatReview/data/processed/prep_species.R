# File name: prep_species.R
# Author: S. Bogen
# Requires all files in the 'data/Raw Vegetation Data' subdirectory
# Generates the file 'processed/species.csv'

#-Load required libraries-------------------------------------------------------

library(readxl)
library(dplyr)
library(purrr)

#-Define file names-------------------------------------------------------------

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

#-Species info closer look------------------------------------------------------

cy1.sp <- read_xlsx(cy1.file, sheet = "PSU_C1_Yr1-5_Species")
cy2.sp <- read_xlsx(cy2.file, sheet = "PSU_C2_Yr1_5_Species")
cy3.yr1and2.sp <- read_xlsx(cy3.yr1and2.file, sheet = "PSU_C3Yr1_2 Species")
cy3.yr3.sp <- read_xlsx(cy3.yr3.file, sheet = "PSU_C3Yr3 Species")
cy3.yr4.sp <- read_xlsx(cy3.yr4.file, sheet = "PSU_C3Yr4 Species")
cy3.yr5.sp <- read_xlsx(cy3.yr5.file, sheet = "PSU_C3Yr5 Species")

#-Process data------------------------------------------------------------------

# get only unique rows for cycle 3 year 5
cy3.yr5.sp <- unique(cy3.yr5.sp)

# remove index column from cycle 1, cycle 2, and cycle 3 years 1 and 2
cy1.sp <- cy1.sp[, -1]
cy2.sp <- cy2.sp[, -1]
cy3.yr1and2.sp <- cy3.yr1and2.sp[, -1]

# confirm species codes are unique in each table
length(unique(cy1.sp$SPCODE))
length(unique(cy2.sp$SPCODE))
length(unique(cy3.yr1and2.sp$SPCODE))
length(unique(cy3.yr3.sp$SPCODE))
length(unique(cy3.yr4.sp$SPCODE))
length(unique(cy3.yr5.sp$SPCODE))
# (they are, great!)

# check for consistency in columns 1-4 across individual files
# Assume cy3.yr5.sp is the most comprehensive version, so omit others from here forward. Harmonize column names of the rest
names(cy1.sp)[1:4] <- names(cy2.sp)[1:4]
names(cy3.yr5.sp)[1:4] <- names(cy2.sp)[1:4]

# re-code presence-absence columns to zeroes and ones
cy1.sp[, 5:9] <- lapply(cy1.sp[, 5:9], function(x) !is.na(x) & x == "*")
cy2.sp[, 5:9] <- lapply(cy2.sp[, 5:9], function(x) !is.na(x) & x == "*")
cy3.yr1and2.sp[, 5:6] <- lapply(cy3.yr1and2.sp[, 5:6], function(x) !is.na(x) & x == "*")
cy3.yr3.sp[, 5:7] <- lapply(cy3.yr3.sp[, 5:7], function(x) !is.na(x) & x == "*")
cy3.yr4.sp[, 5:8] <- lapply(cy3.yr4.sp[, 5:8], function(x) !is.na(x) & x == "*")
cy3.yr5.sp[, 5:9] <- lapply(cy3.yr5.sp[, 5:9], function(x) !is.na(x) & x == "*")

# check for consistency in columns 1-4 across individual files
firstFourCombined <- unique(rbind(cy1.sp[, 1:4], cy2.sp[, 1:4], cy3.yr5.sp[, 1:4]))

# get duplicates
dup_codes <- names(which(table(firstFourCombined$SPCODE) > 1))

# view columns with duplicates:
firstFourCombined[firstFourCombined$SPCODE %in% dup_codes, ] |> View()

