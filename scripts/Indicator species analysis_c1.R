# ======================================================================
# CYCLE 1 INDICATOR SPECIES ANALYSIS
# ======================================================================
#
# Purpose:
#   Indicator species analysis by:
#       1. Region
#       2. PSU
#
# Method:
#   multipatt()
#   Indicator statistic = IndVal.g
#   Permutations = 9,999
#   max.order = 1
#

# Analyst: Deus Rugemalila
# ======================================================================

# ======================================================================
# 1. CLEAN ENVIRONMENT
# ======================================================================
rm(list = ls())

# Reproducible permutation results
set.seed(123)


# ======================================================================
# 2. LOAD PACKAGES
# ======================================================================
library(readxl)
library(reshape2)
library(dplyr)
library(vegan)
library(indicspecies)
library(permute)

# ======================================================================
# 3. DEFINE FILE PATHS
# ======================================================================
base_dir <- "C:/Users/druge/Dropbox/PROJECTS/GitHub/FIU/LPstatreview"
species_file <- file.path("C:/Users/druge/Dropbox/PROJECTS/GitHub/FIU/LPstatreview/data/processed")
region_file <- file.path(base_dir, "data/raw_data/",
                         "1. PSU Sampled_C1, C2 & C3, Scheudled-C4.xlsx")

habitat_file <- file.path(base_dir, "data/processed",
                          "C123_Yr1_5_ALL Plots_locations_habs.csv")

output_dir <- file.path(base_dir,
                        "analysis/Indicator_Species_Analysis")

# Create output folder
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ================================================================
# 3. LOAD SPECIES COVER DATA
# ================================================================
c1_cover <- read.csv("C:/Users/druge/Dropbox/PROJECTS/GitHub/FIU/LPstatreview/data/processed/C123_Yr1_5_ALL_SppCover_Final.csv", header = TRUE)

# Keep c1 species
c1_cover <- subset(c1_cover, Cycle == "C1")
c1_cover <- subset(c1_cover, select = c("PlotID", "SPCODE", "Cover"))

### Get PSU from another file
PSUs <- read.csv("C:/Users/druge/Dropbox/PROJECTS/GitHub/FIU/LPstatreview/data/processed/C123_Yr1_5_ALL Plots_envData.csv", header = TRUE)

PSUs <- subset(PSUs, select = c("PlotID_New", "PSU"))

c1_cover <- merge(PSUs, c1_cover, by.x = "PlotID_New", by.y = "PlotID")

# Rename columns
colnames(c1_cover) <- c("PlotID", "PSU", "Species", "Cover")
str(c1_cover)

# Check unique species
sort(unique(c1_cover$Species))

## Use grep() to remove all UNKNOWN species. They start with UNK
c1_cover <- c1_cover[!grepl("^UNK", c1_cover$Species, ignore.case = TRUE), ]

sort(unique(c1_cover$Species))

#Remove BS1, BS2, BS3 DPM and 513 
c1_cover <- c1_cover[!grepl("BS1|BS2|BS3|DPM|513", c1_cover$PSU), ]
sort(unique(c1_cover$PSU))
str(c1_cover)

# ================================================================
# 4. CREATE SPECIES-BY-PLOT MATRIX
# ================================================================
c1_species_matrix <- dcast(c1_cover, PSU + PlotID ~ Species, value.var = "Cover",
                           fun.aggregate = sum)

# View column names
colnames(c1_species_matrix)


# ================================================================
# 5. LOAD REGION INFORMATION
# ================================================================
regions <- read_excel(region_file, sheet = "PSU_C1-4 Sampled, C4-Scheduled",
                      col_types = "text")

regions <- regions %>% select(PSU = `PSU...1`, Region) %>% distinct()

# Make sure PSU has the same data type
c1_species_matrix$PSU <- as.character(c1_species_matrix$PSU)
regions$PSU <- as.character(regions$PSU)

# ================================================================
# 6. ADD REGION TO SPECIES MATRIX
# ================================================================
c1_species_matrix <- c1_species_matrix %>% left_join(regions, by = "PSU")

# ================================================================
# 7. LOAD HABITAT INFORMATION
# ================================================================
habitat_raw <- read.csv(habitat_file, header = TRUE)


# ================================================================
# 8. CREATE FINAL HABITAT VARIABLE
# ================================================================
habitat <- habitat_raw %>% transmute(PlotID = PlotID_New, PSU = as.character(PSU),
                                     habitat = if_else(
                                       is.na(hab_09) | trimws(hab_09) == "",
                                       hab_25,
                                       hab_09)) %>% distinct()

# ================================================================
# 9. MERGE SPECIES, REGION, AND HABITAT DATA
# ================================================================
analysis_data <- habitat %>% inner_join(c1_species_matrix,
                                        by = c("PlotID", "PSU"))

# ================================================================
# 10. REMOVE RECORDS MISSING ESSENTIAL METADATA
# ================================================================
analysis_data <- analysis_data %>% filter(!is.na(PlotID),
                                          !is.na(PSU),
                                          !is.na(habitat),
                                          habitat != "",
                                          !is.na(Region),
                                          Region != "")
# ================================================================
# 11. CREATE UNIQUE ROW IDENTIFIER
# ================================================================
#
# Using PSU + PlotID is safer than PlotID alone

analysis_data$RowID <- paste(analysis_data$PSU, analysis_data$PlotID, sep = "_")

# Check that the identifier (RowID) is unique
if (anyDuplicated(analysis_data$RowID) > 0) {
  stop("PSU + PlotID does not uniquely identify every row. ",
       "Check for duplicated plots after merging.")}

rownames(analysis_data) <- analysis_data$RowID


# ================================================================
# 12. SEPARATE METADATA AND SPECIES DATA
# ================================================================
Plotdata <- analysis_data %>% select(RowID, PlotID, PSU, habitat, Region)
rownames(Plotdata) <- Plotdata$RowID


# Select species columns by excluding metadata/Plotdata
c1spp <- analysis_data %>% select(-RowID, -PlotID, -PSU, -habitat, -Region)

# ================================================================
# 13. ENSURE SPECIES COLUMNS ARE NUMERIC
# ================================================================
c1spp[] <- lapply(c1spp, as.numeric)


# ================================================================
# 14. HANDLE MISSING SPECIES VALUES
# ================================================================
#
# This assumes NA in a species cell means the species was not
# recorded in that plot and should therefore be treated as zero.

c1spp[is.na(c1spp)] <- 0

# ================================================================
# 15. REMOVE SPECIES ABSENT FROM THE ENTIRE DATASET
# ================================================================
keep_species <- colSums(c1spp, na.rm = TRUE) > 0
c1spp <- c1spp[,keep_species, drop = FALSE]

# ================================================================
# 16. REMOVE PLOTS WITH NO SPECIES COVER
# ================================================================
keep_plots <- rowSums(c1spp, na.rm = TRUE) > 0
c1spp <- c1spp[keep_plots, ,drop = FALSE]

Plotdata <- Plotdata[keep_plots, ,drop = FALSE]

# ================================================================
# 17. CHECK ALIGNMENT. If not aligned, an error msg will pop-up
# ================================================================

if (!identical(rownames(c1spp), rownames(Plotdata))) {
  stop("Species matrix and metadata are not aligned.")
}


# ================================================================
# 18. HELLINGER TRANSFORMATION
# ================================================================
# NOTE: decostand(..., method = "hellinger") already performs the
# square-root component of the Hellinger transformation. 

c1spp_hell <- decostand(c1spp, method = "hellinger")

# ================================================================
# 19. SET INDICATOR SPECIES FUNCTION
# ================================================================
#
# This function:
#   - subsets one Region or PSU
#   - counts plots and habitats
#   - removes empty plots
#   - removes species absent from the group
#   - checks whether multipatt() can run
#   - runs IndVal.g
#   - extracts significant species
#   - records diagnostics (Analysis is done on PSU or regions with at least two habitats)

# ================================================================
run_indval <- function(group_value, 
                       group_variable, 
                       metadata, 
                       spp, 
                       alpha = 0.05,
                       nperm = 9999) {

  # --------------------------------------------------------------
  # A. SELECT GROUP
  # --------------------------------------------------------------
  keep <- (!is.na(metadata[[group_variable]]) & metadata[[group_variable]] == group_value)
  meta_sub <- metadata[keep,,drop = FALSE]
  
  spp_sub <- spp[keep,,drop = FALSE]
  
  
  # --------------------------------------------------------------
  # B. INITIAL DIAGNOSTIC COUNTS
  # --------------------------------------------------------------
  n_plots_initial <- nrow(spp_sub)
  n_habitats_initial <- length(unique(na.omit(meta_sub$habitat)))
  n_species_initial <- if (nrow(spp_sub) > 0) {
    sum(colSums(spp_sub, na.rm = TRUE) > 0)
    } else {
      0
      }

    # --------------------------------------------------------------
  # C. DIAGNOSTIC HELPER
  # --------------------------------------------------------------
  
  make_diagnostic <- function(status,
                              reason,
                              n_plots_final = NA_integer_,
                              n_habitats_final = NA_integer_,
                              n_species_final = NA_integer_,
                              n_significant = 0L) {
    data.frame(
      Group_Type = group_variable,
      Group = as.character(group_value),
      Plots_Initial = as.integer(n_plots_initial), 
      Plots_Analyzed = as.integer(n_plots_final),
      Habitats_Initial =  as.integer(n_habitats_initial),
      Habitats_Analyzed = as.integer(n_habitats_final),
      Species_Initial = as.integer(n_species_initial),
      Species_Analyzed = as.integer(n_species_final),
      Significant_Indicators = as.integer(n_significant), 
      Status = status, Reason = reason, stringsAsFactors = FALSE)
    }
  
  
  # --------------------------------------------------------------
  # D. CHECK INITIAL NUMBER OF PLOTS
  # --------------------------------------------------------------
    if (n_plots_initial < 2) {
      diagnostic <- make_diagnostic(status = "Skipped",
                                    reason = "Fewer than 2 plots")
      return(list(results = NULL,
                  diagnostic = diagnostic))}
  
  
  # --------------------------------------------------------------
  # E. REMOVE EMPTY PLOTS
  # --------------------------------------------------------------
  keep_rows <- rowSums(spp_sub, na.rm = TRUE) > 0
  spp_sub <- spp_sub[keep_rows,,drop = FALSE]
  meta_sub <- meta_sub[keep_rows, ,drop = FALSE]
  
  
  # --------------------------------------------------------------
  # F. REMOVE SPECIES ABSENT FROM THIS GROUP
  # --------------------------------------------------------------
  
  if (nrow(spp_sub) > 0) {keep_columns <- colSums(spp_sub, na.rm = TRUE) > 0
  spp_sub <- spp_sub[ ,keep_columns, drop = FALSE]}
  
  
  # --------------------------------------------------------------
  # G. FINAL COUNTS
  # --------------------------------------------------------------
  n_plots_final <- nrow(spp_sub)
  n_species_final <- ncol(spp_sub)
  meta_sub$habitat <- droplevels(factor(meta_sub$habitat))
  n_habitats_final <- nlevels(meta_sub$habitat)
  
  
  # --------------------------------------------------------------
  # H. CHECK NUMBER OF PLOTS
  # --------------------------------------------------------------
  if (n_plots_final < 2) {diagnostic <- make_diagnostic(status = "Skipped",
                                                        reason = "Fewer than 2 non-empty plots",
                                                        n_plots_final = n_plots_final,
                                                        n_habitats_final = n_habitats_final,
                                                        n_species_final = n_species_final)
  return(list(results = NULL, diagnostic = diagnostic))}
  
    # --------------------------------------------------------------
  # I. CHECK NUMBER OF SPECIES
  # --------------------------------------------------------------
  if (n_species_final == 0) {diagnostic <- make_diagnostic(status = "Skipped",
                                                           reason = "No species present",
                                                           n_plots_final = n_plots_final,
                                                           n_habitats_final = n_habitats_final,
                                                           n_species_final = n_species_final)
  return(list(results = NULL,
              diagnostic = diagnostic))}
  
  # --------------------------------------------------------------
  # J. CHECK NUMBER OF HABITATS
  # --------------------------------------------------------------
  if (n_habitats_final < 2) {diagnostic <- make_diagnostic(status = "Skipped",
                                                           reason = "One habitat represented",
                                                           n_plots_final = n_plots_final,
                                                           n_habitats_final = n_habitats_final,
                                                           n_species_final = n_species_final)
  return(list(results = NULL, diagnostic = diagnostic))}
  
  # --------------------------------------------------------------
  # K. RUN INDICATOR SPECIES ANALYSIS
  # --------------------------------------------------------------
  
  model <- tryCatch(multipatt(spp_sub,
                              meta_sub$habitat,
                              func = "IndVal.g",
                              control = how(nperm = nperm),
                              max.order = 1), error = function(e) {
                                attr(e, "error_message") <- conditionMessage(e)
                                return(e)})
  
  # --------------------------------------------------------------
  # L. HANDLE MULTIPATT ERROR (If any)
  # --------------------------------------------------------------
  if (inherits(model, "error")) {
    diagnostic <- make_diagnostic(status = "Error",
                                  reason = paste("multipatt error:",
                                                 conditionMessage(model)),
                                  n_plots_final = n_plots_final,
                                  n_habitats_final = n_habitats_final,
                                  n_species_final = n_species_final)
    return(list(results = NULL, diagnostic = diagnostic))}
  
  
  # --------------------------------------------------------------
  # M. CHECK WHETHER MODEL RETURNED RESULTS
  # --------------------------------------------------------------
  if (is.null(model$sign) || nrow(model$sign) == 0) {
    diagnostic <- make_diagnostic(status = "Completed",
                                  reason = "multipatt returned no indicator results",
                                  n_plots_final = n_plots_final,
                                  n_habitats_final = n_habitats_final,
                                  n_species_final = n_species_final,
                                  n_significant = 0)
    return(list(results = NULL,
                diagnostic = diagnostic))}
  
  
  # --------------------------------------------------------------
  # N. EXTRACT ALL MODEL RESULTS
  # --------------------------------------------------------------
  results <- data.frame(Species = rownames(model$sign),
                        model$sign,
                        row.names = NULL, check.names = FALSE)
  
  # --------------------------------------------------------------
  # O. CHECK P-VALUE COLUMN
  # --------------------------------------------------------------
  if (!"p.value" %in% names(results)) {diagnostic <- make_diagnostic(
    status = "Error",
    reason = "p.value column not found in multipatt output",
    n_plots_final = n_plots_final,
    n_habitats_final = n_habitats_final,
    n_species_final = n_species_final)
  return(list(results = NULL,
              diagnostic = diagnostic))}
  
  
  # --------------------------------------------------------------
  # P. KEEP SIGNIFICANT INDICATOR SPECIES
  # --------------------------------------------------------------
  significant_results <- results %>% filter(!is.na(p.value), p.value <= alpha)
  n_significant <- nrow(significant_results)
  
  
  # --------------------------------------------------------------
  # Q. NO SIGNIFICANT INDICATORS
  # --------------------------------------------------------------
  if (n_significant == 0) {
    diagnostic <- make_diagnostic(status = "Completed",
                                  reason = paste0("No significant indicators at alpha = ",
                                                  alpha),
                                  n_plots_final = n_plots_final,
                                  n_habitats_final = n_habitats_final,
                                  n_species_final = n_species_final,
                                  n_significant = 0)
    return(list(results = NULL,
                diagnostic = diagnostic))}
  
  
  # --------------------------------------------------------------
  # R. ADD GROUP INFORMATION
  # --------------------------------------------------------------
  significant_results <- significant_results %>% mutate(Group_Type = group_variable,
                                                        Group = as.character(group_value),
                                                        .before = 1)

  
  # --------------------------------------------------------------
  # S. DIAGNOSTIC FOR SUCCESSFUL ANALYSIS
  # --------------------------------------------------------------
  diagnostic <- make_diagnostic(status = "Completed",
                                reason = "Significant indicator species detected",
                                n_plots_final = n_plots_final,
                                n_habitats_final = n_habitats_final,
                                n_species_final = n_species_final,
                                n_significant = n_significant)
  
  # --------------------------------------------------------------
  # T. RETURN BOTH RESULTS AND DIAGNOSTICS
  # --------------------------------------------------------------
  return(list(results = significant_results, 
              diagnostic = diagnostic))}


# ================================================================
# 20. FUNCTION TO RUN ALL GROUPS
# ================================================================
run_all_groups <- function(group_variable,
                           metadata,
                           spp,
                           alpha = 0.05,
                           nperm = 9999) {
  group_values <- sort(unique(na.omit(metadata[[group_variable]])))
  
  analyses <- lapply(group_values, function(x) {run_indval(
    group_value = x, group_variable = group_variable, 
    metadata = metadata, spp = spp, alpha = alpha, nperm = nperm)})
  
  
  # Combine significant indicator results
  results <- bind_rows(lapply(analyses,function(x) x$results))
  
  # Combine diagnostics
  diagnostics <- bind_rows(lapply(analyses,
                                  function(x) x$diagnostic))
  return(list(
    results = results,
    diagnostics = diagnostics))}

# ================================================================
# 21. REGION-LEVEL ANALYSIS UPDATES
# ================================================================
cat("\n========================================\n",
    "RUNNING REGION-LEVEL ANALYSIS\n",
    "========================================\n")

region_analysis <- run_all_groups(group_variable = "Region",
                                  metadata = Plotdata,
                                  spp = c1spp_hell,
                                  alpha = 0.05,
                                  nperm = 9999)


region_results <- region_analysis$results
region_diagnostics <- region_analysis$diagnostics

# ================================================================
# 22. PSU-LEVEL ANALYSIS UPDATES
# ================================================================
cat("\n========================================\n",
    "RUNNING PSU-LEVEL ANALYSIS\n",
    "========================================\n")

psu_analysis <- run_all_groups(group_variable = "PSU",
                               metadata = Plotdata,
                               spp = c1spp_hell,
                               alpha = 0.05,
                               nperm = 9999)


psu_results <- psu_analysis$results
psu_diagnostics <- psu_analysis$diagnostics

# ================================================================
# 23. SORT COMBINED RESULTS
# ================================================================
if (nrow(region_results) > 0) {
  region_results <- region_results %>% arrange(Group, p.value, desc(stat))}
if (nrow(psu_results) > 0) {
  psu_results <- psu_results %>% arrange(Group, p.value, desc(stat))}

# ================================================================
# 24. SORT DIAGNOSTIC TABLES
# ================================================================
region_diagnostics <- region_diagnostics %>% arrange(Group)
psu_diagnostics <- psu_diagnostics %>% arrange(Group)


# ================================================================
# 25. WRITE COMBINED REGION RESULTS
# ================================================================
write.csv(region_results,
          file.path(output_dir, "c1_All_Region_Indicator_Species_Results.csv"),
          row.names = FALSE)


# ================================================================
# 26. WRITE COMBINED PSU RESULTS
# ================================================================
write.csv(psu_results,
          file.path(output_dir, "c1_All_PSU_Indicator_Species_Results.csv"), 
          row.names = FALSE)


# ================================================================
# 27. WRITE REGION DIAGNOSTICS
# ================================================================
write.csv(region_diagnostics,
          file.path(output_dir,
                    "c1_Region_Indicator_Species_Diagnostics.csv"),
          row.names = FALSE)

# ================================================================
# 28. WRITE PSU DIAGNOSTICS
# ================================================================
write.csv(psu_diagnostics, 
          file.path(output_dir,
                    "c1_PSU_Indicator_Species_Diagnostics.csv"),
          row.names = FALSE)

# 
# # ================================================================
# # 29. DISPLAY ANALYSIS SUMMARY
# # ================================================================
# cat("\n\n========================================\n",
#     "CYCLE 1 ANALYSIS COMPLETE\n",
#     "========================================\n")
# cat("\nREGION ANALYSIS\n")
# cat("Regions evaluated:", nrow(region_diagnostics), "\n")
# cat("Regions completed:", sum(region_diagnostics$Status == "Completed"), "\n")
# cat("Regions skipped:", sum(region_diagnostics$Status == "Skipped"), "\n")
# cat("Regions with errors:", sum(region_diagnostics$Status == "Error"), "\n")
# cat("Significant Region indicators:",nrow(region_results),"\n")
# cat("\nPSU ANALYSIS\n")
# cat("PSUs evaluated:", nrow(psu_diagnostics), "\n")
# cat("PSUs completed:", sum(psu_diagnostics$Status == "Completed"), "\n")
# cat("PSUs skipped:",sum(psu_diagnostics$Status == "Skipped"), "\n")
# cat("PSUs with errors:", sum(psu_diagnostics$Status == "Error"), "\n")
# cat("Significant PSU indicators:", nrow(psu_results), "\n")
# 
# 
# # ================================================================
# # 30. DISPLAY DIAGNOSTIC TABLES
# # ================================================================
# cat("\n\nREGION DIAGNOSTICS\n")
# print(region_diagnostics, row.names = FALSE)
# cat("\n\nPSU DIAGNOSTICS\n")
# print(psu_diagnostics,row.names = FALSE)
# 
# ================================================================
# END
# ================================================================

