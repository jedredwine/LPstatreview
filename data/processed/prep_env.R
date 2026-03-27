# File name: prep_env.R
# Author: S. Bogen
# Requires all files in the 'data/Raw Vegetation Data' subdirectory
# Generates the file 'processed/envData.csv'

#-Load required libraries-------------------------------------------------------

library(readxl)
library(dplyr)
library(purrr)

#-Read in raw environmental data------------------------------------------------

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

# The warnings that appear below come from text values in the "PSU" column,
# which read_xlsx expects to be numeric. Column is omitted since it is
# redundant with the "PSU_ID"column.

cy1.env <- read_xlsx(cy1.file, sheet = "C1_Yr1_5_ Environmental Data")
cy2.env <- read_xlsx(cy2.file, sheet = "C2_Yr1_5_ Environmental Data")
cy3.yr1and2.env <-  read_xlsx(cy3.yr1and2.file, sheet = "C3Yr1_2 Environmental Data")
cy3.yr3.env <- read_xlsx(cy3.yr3.file, sheet = "C3Yr3_Environmental Data")
cy3.yr4.env <- read_xlsx(cy3.yr4.file, sheet = "C3Yr4 Environmental Data")
cy3.yr5.env <- read_xlsx(cy3.yr5.file, sheet = "C3Yr5 Environmental Data" )

# Clean up space
rm(list = ls(pattern = "\\.file$"))
rm(cy3.dir, path)

#-Standardize columns-----------------------------------------------------------

names(cy1.env)
names(cy2.env)
names(cy3.yr1and2.env)
names(cy3.yr3.env)
names(cy3.yr4.env)
names(cy3.yr5.env)

##-Standardize across cycle 3 years---------------------------------------------

names(cy3.yr1and2.env) == names(cy3.yr3.env)
names(cy3.yr1and2.env) == names(cy3.yr4.env)
names(cy3.yr1and2.env) == names(cy3.yr5.env)[-19]

# Remove extra column from cycle 3 year 5
cy3.yr5.env <- cy3.yr5.env[, -19]

# Note there are some typos - need to harmonize and combine
names(cy3.yr1and2.env)[names(cy3.yr1and2.env) != names(cy3.yr5.env)]
names(cy3.yr5.env)[names(cy3.yr1and2.env) != names(cy3.yr5.env)]

names(cy3.yr5.env)[18] <- "Soil Depth Corrected_New (cm)"
names(cy3.yr5.env)[21] <- "Ridge-Slough"

# Combine cy3 data frames and clean up space
cy3.env <- rbind(cy3.yr1and2.env, cy3.yr3.env, cy3.yr4.env, cy3.yr5.env)
rm(cy3.yr1and2.env, cy3.yr3.env, cy3.yr4.env, cy3.yr5.env)

##-Standardize across cycles ---------------------------------------------------

# Check cycle 2 and cycle 3.
names(cy2.env)
names(cy3.env)
# Extra column in cy2 is "Comments (Daa QA/QC)" - omit
cy2.env <- cy2.env[, -29]

# for cycles 2 and 3, remove all serial numbers except for raw version
cy2.env <- cy2.env[, 6:28]
cy3.env <- cy3.env[, 6:28]

names(cy2.env) == names(cy3.env)
# (Differences are now a difference of typo rather than content)

names(cy3.env)
names(cy2.env)
names(cy1.env)

cy1.env <- cbind(data.frame("Datasheet Raw SNO" = 1:8597),
                 cy1.env)

# TODO: harmonize the column order and names
names(cy1.env)
names(cy2.env)
names(cy3.env)

# Take out cy1 serial number
cy1.env <- cy1.env[, -2]
names(cy1.env)
names(cy2.env)

# Add PSU_cycle column to Cycle 1 and omit PlotID_Old.
names(cy1.env)[2] <- "PSU_cycle"
cy1.env$PSU_cycle <- rep("C1", 8597)

names(cy1.env)
names(cy2.env)

# seems like we can remove cycle 1's PLOT (numeric value that encodes the same 
# information as the more descriptive Plot column)
cy1.env <- cy1.env[, -6]

names(cy1.env)
names(cy2.env)

# reorder
cy1.env <- cy1.env[, c(1:4, 7, 5, 6, 8:21)]

names(cy1.env)
names(cy2.env)

# remove psu column from cy2 and cy3
cy2.env <- cy2.env[, -6]
cy3.env <- cy3.env[, -6]

names(cy1.env)
names(cy2.env)

# remove Cycle_PlotID from 2 and 3 (redundant - 8)
cy2.env <- cy2.env[, -8]
cy3.env <- cy3.env[, -8]

names(cy1.env)
names(cy2.env)
names(cy3.env)

# good for 1:10
names(cy1.env)[10:21]
names(cy2.env)[10:21]
names(cy3.env)[10:21]

# order should be 1:10 for all, then:
# for all, the four wd values, cladium height, ridge-slough classification, field notes
# for cycle 1, soil depth and the three EDEN-derived measures
# for cycles 2 and 3, soil depth, two map classification, observer

cy1.env <- cy1.env[, c(1:10, 13:16, 20, 19, 21, 11:12, 17:18)]
cy2.env <- cy2.env[, c(1:10, 15:18, 19, 14, 20, 11, 12, 13, 21)]
cy3.env <- cy3.env[, c(1:10, 15:18, 19, 14, 20, 11, 12, 13, 21)]

# Next, common column harmonize names to cycle 2 columns
names(cy1.env)[1:17] <- names(cy2.env)[1:17]
names(cy3.env) <- names(cy2.env)

# Add empty columns and combine

# keeps all columns, fills with NA where missing (the warnings are the point)
cy1.env$PSU_ID <- as.character(cy1.env$PSU_ID)
cy3.env$NorthNAD83 <- as.numeric(cy3.env$NorthNAD83)
cy3.env$EastNAD83 <- as.numeric(cy3.env$EastNAD83)
cy3.env$`WD1 (cm)` <- as.numeric(cy3.env$`WD1 (cm)`)
cy3.env$`WD3 (cm)` <- as.numeric(cy3.env$`WD3 (cm)`)

# Combine rows
result <- bind_rows(cy1.env, cy2.env, cy3.env)

# Remove serial number
result <- result[, -1]

#-Verify keys-------------------------------------------------------------------

# Each row is a sampling event
# Uniquely identified by...PSU Cycle and PlotID? Check:

dim(unique(result[, c("PlotID", "PSU_Cycle")]))

# There appear to be 56 duplicates. View them:
duplicates <- result[duplicated(result[, c("PlotID", "PSU_Cycle")]) | 
                       duplicated(result[, c("PlotID", "PSU_Cycle")], fromLast = TRUE), ]

# Duplicates are all in Cycle 1 and have invalid value X in Clt. 
# Filter these out:
result <- result[result$Pt != "X", ]

# Check for uniqueness again:
dim(unique(result[, c("PlotID", "PSU_Cycle")]))

# There appear to still be 26 duplicates. View them:
duplicates2 <- result[duplicated(result[, c("PlotID", "PSU_Cycle")]) | 
                       duplicated(result[, c("PlotID", "PSU_Cycle")], 
                                  fromLast = TRUE), ]

# Wow - they appear to be all NA rows. Filter those out:
result <- result[!apply(is.na(result), 1, all), ]

# Check one more time
dim(unique(result[, c("PlotID", "PSU_Cycle")]))
# good!

# Final step: remove all irrelevant columns and re-order columns
result <- result[, c(4, 1, 2, 9:24)]

#-Save result-------------------------------------------------------------------

write.csv(result, file = "data/processed/envData.csv")
