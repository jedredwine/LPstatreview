# File name: prep_psus.R
# Author: S. Bogen
# Requires all files in the 'data/Raw Vegetation Data' subdirectory
# Generates the file 'processed/psus.csv'

#-Load required libraries-------------------------------------------------------

library(readxl)
library(dplyr)
library(purrr)

#-Read in raw PSU data----------------------------------------------------------

path <- "data/Raw Vegetation Data"

# Define raw file names
cy1.file <- file.path(path, "PSU_C1_Yr1_5 (2009-2015)/W912HZ1020030_PSU_C1_Yr1_5_(2009-2015)_ALL_Data_ 2024.02.28.xlsx")
cy2.file <- file.path(path, "PSU_C2_Yr1_5 (2015-2020)/W912HZ1520027_PSU_C2_Yr1_5_(2015-2020)_ALL_Data_2024.02.28.xlsx")
cy3.dir <- file.path(path, "PSU_C3_Yr1_5 (2020-2025)")
cy3.yr1and2.file <- file.path(cy3.dir, "W912HZ2020038_PSU_C3_Yr1_2_(2020-2022)_ALL Data_2023.02.28.xlsx")
cy3.yr3.file <- file.path(cy3.dir, "W912HZ2020038_PSU_C3_Yr3_ALL Data_for_Geodatabase_2024.03.20.xlsx")
cy3.yr4.file <- file.path(cy3.dir, "W912HZ2020038_PSU_C3_Yr4_ALL_Data_for_Geodatabase_2025.09  V2_Prov..xlsx")
cy3.yr5.file <- file.path(cy3.dir, "W912HZ2020038_PSU_C3_Yr5_ALL_Data_for_Geodatabase_2025.09.30_Prov..xlsx")

# Read PSU data sheets
cy1.sampled <- read_xlsx(cy1.file, sheet = "PSU_C1_Yr1_5 Sampled")
cy2.sampled <- read_xlsx(cy2.file, sheet = "PSU_C2_Yr1_5 Sampled")
cy3.yr1and2.sampled <- read_xlsx(cy3.yr1and2.file, sheet = "PSU_C3Y1_2 Sampled")
cy3.yr3.sampled <- read_xlsx(cy3.yr3.file, sheet = "PSU_C2_Yr3_Sampled")
cy3.yr4.sampled <- read_xlsx(cy3.yr4.file, sheet = "PSU_C3Y4 Sampled")
cy3.yr5.sampled <- read_xlsx(cy3.yr5.file, sheet = "PSU_C3Yr5_Sampled")

# Combine cycle 3 info
cy3.sampled <- rbind(cy3.yr1and2.sampled, cy3.yr3.sampled,
                     cy3.yr4.sampled, cy3.yr5.sampled)

# Clean up space
rm(list = ls(pattern = "\\.file$"))
rm(cy3.yr1and2.sampled, cy3.yr3.sampled, cy3.yr4.sampled, cy3.yr5.sampled)
rm(cy3.dir, path)

#-Standardize column names------------------------------------------------------

##-Common columns---------------------------------------------------------------
# The following columns should be common across all cycles where the PSU appears:
# PSU_ID, PSU, X, Y, Region OR Region*, Historical R&S, 
# Cycle-X Sampling Year (WYr) (the water year the PSU was sampled in) - 
#       not present in every cycle, but if it appears more than once it should 
#       be identical to the other places it appears.

# save only Cycle-specific WYr info
cy2.sampled <- cy2.sampled[, -5]
cy3.sampled <- cy3.sampled[, c(-5, -6)]

# Standardize common column names
names(cy3.sampled)[1] <- "PSU_ID"
names(cy3.sampled)[7] <- "Region"
names(cy3.sampled)[9]<- "X_UTMNAD83"
names(cy3.sampled)[10] <- "Y_UTMNAD83"

##-Cycle-specific columns-------------------------------------------------------
# The following columns appear to contain cycle-specific information: 
# Cycle-X year (the cycle year in which the PSU was sampled, 1-5)
# Sampdate OR Cycle 3 Sampling date
# Cycle-X No. of plots (integer number of plots sampled in the PSU/cycle)

# Make non-standard column names cycle-specific. OMIT Cycle column (redundant)

names(cy1.sampled)[4] <- "Cy1_Year"
names(cy2.sampled)[4] <- "Cy2_Year"
names(cy3.sampled)[4] <- "Cy3_Year"

names(cy1.sampled)[5] <- "Cy1_WYr"
names(cy2.sampled)[5] <- "Cy2_WYr"
names(cy3.sampled)[5] <- "Cy3_WYr"

names(cy1.sampled)[6] <- "Cy1_Sampdates"
names(cy2.sampled)[6] <- "Cy2_Sampdates"
names(cy3.sampled)[6] <- "Cy3_Sampdates"

names(cy1.sampled)[11] <- "Cy1_NumPlots"
names(cy2.sampled)[11] <- "Cy2_NumPlots"
names(cy3.sampled)[11] <- "Cy3_NumPlots"

#-Assemble common columns-------------------------------------------------------

##-Check for consistency--------------------------------------------------------

# Get number of unique PSU_IDs
c(cy1.sampled$PSU_ID, cy2.sampled$PSU_ID, cy3.sampled$PSU_ID) |> unique() |> length()

# PSU_ID and PSU - want 69
rbind(cy1.sampled[, c("PSU_ID", "PSU")],
      cy2.sampled[, c("PSU_ID", "PSU")],
      cy3.sampled[, c("PSU_ID", "PSU")]) |> unique() |> dim()
# We have 70 - check
foo <- rbind(cy1.sampled[, c("PSU_ID", "PSU")],
             cy2.sampled[, c("PSU_ID", "PSU")],
             cy3.sampled[, c("PSU_ID", "PSU")]) |> unique()
# culprit is for P108

# PSU_ID and Historical R&S - want 69
rbind(cy1.sampled[, c("PSU_ID", "Historical R&S")],
      cy2.sampled[, c("PSU_ID", "Historical R&S")],
      cy3.sampled[, c("PSU_ID", "Historical R&S")]) |> unique() |> dim()
# Good!

# Assemble common columns:
common_cols <- rbind(cy1.sampled[, c("PSU_ID", "PSU", "Historical R&S")],
                     cy2.sampled[, c("PSU_ID", "PSU", "Historical R&S")],
                     cy3.sampled[, c("PSU_ID", "PSU", "Historical R&S")]) |> unique()

# Remove duplicate and order by PSU_ID
common_cols <- common_cols[-61, ]
common_cols <- common_cols[order(common_cols$PSU_ID), ]

##-Standardize region-----------------------------------------------------------
## The "Region" column was updated to include more detail for some PSUs during
##  cycle 3. To handle this, harmonized PSU data prioritizes cycle 3 value first
##  if available, then cycle 2 value if available, then default to cycle 1 value.

# get cycle 3 region
common_cols <- merge.data.frame(common_cols, cy3.sampled[, c("PSU_ID", "Region")], 
                        by = "PSU_ID", all.x = TRUE) |> unique()

# get cycle 3 X and Ys
common_cols <- merge.data.frame(common_cols, cy3.sampled[, c("PSU_ID", 
                                                             "X_UTMNAD83", 
                                                             "Y_UTMNAD83")], 
                                by = "PSU_ID", all.x = TRUE) |> unique()

# get missing region and X/Y coordinates
missing_from_cy3 <- common_cols$PSU_ID[is.na(common_cols$Region)]

for(psu in missing_from_cy3){
  
  # check if in cy2.sampled$PSU_ID
  # if fails, check cy1.sampled
  # if fails, throw error
  
  if(psu %in% cy2.sampled$PSU_ID){
    
    # get info from cy2
    common_cols[common_cols$PSU_ID==psu, "Region"] <- 
      cy2.sampled$Region[cy2.sampled$PSU_ID == psu]
    common_cols[common_cols$PSU_ID==psu, "X_UTMNAD83"] <- 
      cy2.sampled$X_UTMNAD83[cy2.sampled$PSU_ID == psu]
    common_cols[common_cols$PSU_ID==psu, "Y_UTMNAD83"] <- 
      cy2.sampled$Y_UTMNAD83[cy2.sampled$PSU_ID == psu]
    
  } else if(psu %in% cy1.sampled$PSU_ID){
    # get info from cy1
    
    common_cols[common_cols$PSU_ID==psu, "Region"] <- 
      cy1.sampled$Region[cy1.sampled$PSU_ID == psu]
    common_cols[common_cols$PSU_ID==psu, "X_UTMNAD83"] <- 
      cy1.sampled$X_UTMNAD83[cy1.sampled$PSU_ID == psu]
    common_cols[common_cols$PSU_ID==psu, "Y_UTMNAD83"] <- 
      cy1.sampled$Y_UTMNAD83[cy1.sampled$PSU_ID == psu]
    
  } else {
    paste("ERROR for", psu)
  }
  
}

#-Merge common and cycle-specific columns---------------------------------------

# Then merge common columns with cycle 1-unique columns, cycle 2 unique columns, 
# and cycle 3 unique columns. Then save.

cy1_specific_cols <- cy1.sampled[, c("PSU_ID", "Cy1_Year", "Cy1_WYr", 
                                     "Cy1_Sampdates", "Cy1_NumPlots")]
cy2_specific_cols <- cy2.sampled[, c("PSU_ID", "Cy2_Year", "Cy2_WYr", 
                                     "Cy2_Sampdates", "Cy2_NumPlots")]
cy3_specific_cols <- cy3.sampled[, c("PSU_ID", "Cy3_Year", "Cy3_WYr", 
                                     "Cy3_Sampdates", "Cy3_NumPlots")]

psu.df.combined <- merge.data.frame(common_cols, cy1_specific_cols, 
                                    by = "PSU_ID", all.x = TRUE)
psu.df.combined <- merge.data.frame(psu.df.combined, cy2_specific_cols, 
                                    by = "PSU_ID", all.x = TRUE)
psu.df.combined <- merge.data.frame(psu.df.combined, cy3_specific_cols, 
                                    by = "PSU_ID", all.x = TRUE)

# remove duplicate (row 42)
psu.df.combined <- psu.df.combined[-42, ]

# Prior to saving, ensure all columns are expected data types
lapply(psu.df.combined, class)
# (good!)

# Change all N/A to NA and replace Cy1_year value with NA for that row.
# (This happens in PSU P005)
psu.df.combined[psu.df.combined=="N/A"] <- NA
psu.df.combined$Cy1_Year[psu.df.combined$PSU_ID=="P005"] <- NA

# Ensure Cy1_NumPlots is numeric
psu.df.combined$Cy1_NumPlots <- as.numeric(psu.df.combined$Cy1_NumPlots)

#-Save result-------------------------------------------------------------------

write.csv(psu.df.combined, file = "data/processed/psusRef.csv")

