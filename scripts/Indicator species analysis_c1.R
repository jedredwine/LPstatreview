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

base_dir <- "C:/Users/druge/Dropbox/PROJECTS/FIU/PSU Work 2026/GITHUB"

species_file <- file.path(base_dir,
                          "c1spp.cover.csv")

region_file <- file.path(base_dir,
                         "LPstatreview/data/Raw Vegetation Data",
                         "1. PSU Sampled_C1, C2 & C3, Scheudled-C4.xlsx")

habitat_file <- file.path(base_dir,
                          "LPstatreview/data/processed",
                          "C123_Yr1_5_ALL Plots_locations_habs.csv")

output_dir <- file.path(base_dir,
                        "Results_Tables/C1")

# Create output folder if it does not already exist
dir.create(output_dir, recursive = TRUE,
           showWarnings = FALSE)

# ======================================================================
# 4. LOAD CYCLE 1 SPECIES DATA
# ======================================================================

c1spp.cover <- read.csv(species_file, header = TRUE)

# Examine data
str(c1spp.cover)
head(c1spp.cover)

# ======================================================================
# 5. CREATE SPECIES-BY-PLOT MATRIX
# ======================================================================

c1.spp.matrix <- dcast(c1spp.cover,
                       PSU + PlotID ~ Species,
                       value.var = "Cover",
                       fun.aggregate = sum)

# Check matrix
dim(c1.spp.matrix)
head(c1.spp.matrix)

# ======================================================================
# 6. CLEAN SPECIES NAMES
# ======================================================================

# Remove leading "X" added by R to some species names
names(c1.spp.matrix) <- sub("^X", "", names(c1.spp.matrix))

# ======================================================================
# 7. LOAD REGION INFORMATION
# ======================================================================

regions <- read_excel(region_file,
                      sheet = "PSU_C1-4 Sampled, C4-Scheduled",
                      col_types = "text")

# Keep only PSU and Region
regions <- regions %>% select(PSU = `PSU...1`,
                              Region) %>% distinct()

# Examine region information
table(regions$Region, useNA = "ifany")


# ======================================================================
# 8. ADD REGION TO SPECIES MATRIX
# ======================================================================

c1.spp.matrix <- c1.spp.matrix %>%
  left_join(
    regions,
    by = "PSU"
  )

# Check results
table(
  c1.spp.matrix$Region,
  useNA = "ifany"
)


# ======================================================================
# 9. LOAD HABITAT INFORMATION
# ======================================================================

habitat <- read.csv(
  habitat_file,
  header = TRUE
)


# ======================================================================
# 10. CREATE FINAL HABITAT CLASSIFICATION
# ======================================================================

habitat3 <- habitat %>%
  transmute(
    
    PlotID = PlotID_New,
    
    PSU = PSU,
    
    # Use hab_09 when available.
    # Otherwise use hab_25.
    habitat = if_else(
      is.na(hab_09) | trimws(hab_09) == "",
      hab_25,
      hab_09
    )
    
  ) %>%
  distinct()


# Examine habitat classifications
table(
  habitat3$habitat,
  useNA = "ifany"
)


# ======================================================================
# 11. MERGE SPECIES, REGION, AND HABITAT DATA
# ======================================================================

matrix2 <- habitat3 %>%
  inner_join(
    c1.spp.matrix,
    by = c("PlotID", "PSU")
  )


# ======================================================================
# 12. REMOVE INCOMPLETE METADATA
# ======================================================================

# Indicator species analysis requires PSU, habitat, and Region
matrix2 <- matrix2 %>%
  filter(
    !is.na(PlotID),
    !is.na(PSU),
    !is.na(habitat),
    !is.na(Region),
    trimws(habitat) != ""
  )


# ======================================================================
# 13. SET PLOT ID AS ROW NAMES
# ======================================================================

rownames(matrix2) <- matrix2$PlotID


# ======================================================================
# 14. SEPARATE METADATA AND SPECIES DATA
# ======================================================================

Plotdata <- matrix2 %>%
  select(
    PlotID,
    PSU,
    habitat,
    Region
  )

c1spp <- matrix2 %>%
  select(
    -PlotID,
    -PSU,
    -habitat,
    -Region
  )


# ======================================================================
# 15. MAKE SURE SPECIES COLUMNS ARE NUMERIC
# ======================================================================

c1spp[] <- lapply(
  c1spp,
  as.numeric
)


# ======================================================================
# 16. REPLACE SPECIES NAs WITH ZERO
# ======================================================================

# Missing species cover is treated as absence
c1spp[is.na(c1spp)] <- 0


# ======================================================================
# 17. REMOVE SPECIES ABSENT FROM ALL PLOTS
# ======================================================================

keep_species <- colSums(
  c1spp,
  na.rm = TRUE
) > 0

c1spp <- c1spp[
  ,
  keep_species,
  drop = FALSE
]


# ======================================================================
# 18. REMOVE PLOTS WITH NO SPECIES
# ======================================================================

keep_plots <- rowSums(
  c1spp,
  na.rm = TRUE
) > 0

c1spp <- c1spp[
  keep_plots,
  ,
  drop = FALSE
]

Plotdata <- Plotdata[
  keep_plots,
  ,
  drop = FALSE
]


# ======================================================================
# 19. VERIFY ALIGNMENT
# ======================================================================

stopifnot(
  nrow(c1spp) == nrow(Plotdata)
)

stopifnot(
  identical(
    rownames(c1spp),
    rownames(Plotdata)
  )
)


# ======================================================================
# 20. DATA SUMMARY BEFORE TRANSFORMATION
# ======================================================================

cat("\n")
cat("====================================================\n")
cat("CYCLE 1 DATA SUMMARY\n")
cat("====================================================\n")

cat(
  "Number of plots:",
  nrow(c1spp),
  "\n"
)

cat(
  "Number of species:",
  ncol(c1spp),
  "\n"
)

cat(
  "Number of PSUs:",
  length(unique(Plotdata$PSU)),
  "\n"
)

cat(
  "Number of Regions:",
  length(unique(Plotdata$Region)),
  "\n"
)

cat(
  "Number of habitats:",
  length(unique(Plotdata$habitat)),
  "\n"
)


# ======================================================================
# 21. HELLINGER TRANSFORMATION
# ======================================================================

# Hellinger transformation already includes the square-root operation.
# Therefore, DO NOT apply sqrt() again afterward.

c1spp <- decostand(
  c1spp,
  method = "hellinger"
)


# ======================================================================
# 22. GENERAL INDICATOR SPECIES FUNCTION
# ======================================================================

run_indval <- function(
    group_value,
    group_variable,
    metadata,
    spp,
    output_dir,
    cycle = "c1",
    alpha = 0.05,
    nperm = 9999) {
  
  
  # --------------------------------------------------------------------
  # A. Identify plots belonging to this group
  # --------------------------------------------------------------------
  
  keep <- !is.na(metadata[[group_variable]]) &
    metadata[[group_variable]] == group_value
  
  
  meta_sub <- metadata[
    keep,
    ,
    drop = FALSE
  ]
  
  spp_sub <- spp[
    keep,
    ,
    drop = FALSE
  ]
  
  
  # --------------------------------------------------------------------
  # B. Check number of plots
  # --------------------------------------------------------------------
  
  if (nrow(spp_sub) < 2) {
    
    message(
      "Skipping ",
      group_variable,
      " ",
      group_value,
      ": fewer than 2 plots."
    )
    
    return(NULL)
  }
  
  
  # --------------------------------------------------------------------
  # C. Remove plots containing no species
  # --------------------------------------------------------------------
  
  keep_rows <- rowSums(
    spp_sub,
    na.rm = TRUE
  ) > 0
  
  
  meta_sub <- meta_sub[
    keep_rows,
    ,
    drop = FALSE
  ]
  
  spp_sub <- spp_sub[
    keep_rows,
    ,
    drop = FALSE
  ]
  
  
  # Check again after removing empty plots
  if (nrow(spp_sub) < 2) {
    
    message(
      "Skipping ",
      group_variable,
      " ",
      group_value,
      ": fewer than 2 non-empty plots."
    )
    
    return(NULL)
  }
  
  
  # --------------------------------------------------------------------
  # D. Remove species absent from this group
  # --------------------------------------------------------------------
  
  keep_species <- colSums(
    spp_sub,
    na.rm = TRUE
  ) > 0
  
  
  spp_sub <- spp_sub[
    ,
    keep_species,
    drop = FALSE
  ]
  
  
  if (ncol(spp_sub) == 0) {
    
    message(
      "Skipping ",
      group_variable,
      " ",
      group_value,
      ": no species present."
    )
    
    return(NULL)
  }
  
  
  # --------------------------------------------------------------------
  # E. Clean habitat groups
  # --------------------------------------------------------------------
  
  meta_sub$habitat <- droplevels(
    factor(meta_sub$habitat)
  )
  
  
  n_habitats <- nlevels(
    meta_sub$habitat
  )
  
  
  # Indicator analysis requires at least two habitat groups
  if (n_habitats < 2) {
    
    message(
      "Skipping ",
      group_variable,
      " ",
      group_value,
      ": only one habitat represented."
    )
    
    return(NULL)
  }
  
  
  # --------------------------------------------------------------------
  # F. Check habitat replication
  # --------------------------------------------------------------------
  
  habitat_counts <- table(
    meta_sub$habitat
  )
  
  
  if (any(habitat_counts == 0)) {
    
    message(
      "Skipping ",
      group_variable,
      " ",
      group_value,
      ": empty habitat level."
    )
    
    return(NULL)
  }
  
  
  # --------------------------------------------------------------------
  # G. Run indicator species analysis
  # --------------------------------------------------------------------
  
  model <- tryCatch(
    
    {
      
      multipatt(
        spp_sub,
        meta_sub$habitat,
        func = "IndVal.g",
        control = how(
          nperm = nperm
        ),
        max.order = 1
      )
      
    },
    
    error = function(e) {
      
      message(
        "Could not analyze ",
        group_variable,
        " ",
        group_value,
        ": ",
        e$message
      )
      
      return(NULL)
      
    }
    
  )
  
  
  # If multipatt failed, stop processing this group
  if (is.null(model)) {
    
    return(NULL)
    
  }
  
  
  # --------------------------------------------------------------------
  # H. Check whether multipatt returned results
  # --------------------------------------------------------------------
  
  if (
    is.null(model$sign) ||
    nrow(model$sign) == 0
  ) {
    
    message(
      group_variable,
      " ",
      group_value,
      ": multipatt returned no species results."
    )
    
    return(NULL)
  }
  
  
  # --------------------------------------------------------------------
  # I. Convert results to dataframe
  # --------------------------------------------------------------------
  
  results <- data.frame(
    
    Species = rownames(model$sign),
    
    model$sign,
    
    row.names = NULL,
    
    check.names = FALSE
  )
  
  
  # --------------------------------------------------------------------
  # J. FIX FOR PSU108 / ZERO-ROW RESULTS
  # --------------------------------------------------------------------
  
  # This check prevents:
  #
  # Error in [[<-.data.frame:
  # replacement has 1 row, data has 0
  
  if (nrow(results) == 0) {
    
    message(
      group_variable,
      " ",
      group_value,
      ": no indicator species results."
    )
    
    return(NULL)
  }
  
  
  # --------------------------------------------------------------------
  # K. Keep statistically significant species
  # --------------------------------------------------------------------
  
  significant_results <- results %>%
    filter(
      !is.na(p.value),
      p.value <= alpha
    )
  
  
  # --------------------------------------------------------------------
  # L. Handle groups with no significant species
  # --------------------------------------------------------------------
  
  if (nrow(significant_results) == 0) {
    
    message(
      group_variable,
      " ",
      group_value,
      ": no significant indicator species at alpha = ",
      alpha
    )
    
    return(NULL)
  }
  
  
  # --------------------------------------------------------------------
  # M. Add group identifier
  # --------------------------------------------------------------------
  
  # rep() explicitly guarantees the replacement has the same
  # number of rows as the results dataframe.
  
  significant_results[[group_variable]] <- rep(
    group_value,
    nrow(significant_results)
  )
  
  
  # --------------------------------------------------------------------
  # N. Reorder columns
  # --------------------------------------------------------------------
  
  significant_results <- significant_results %>%
    select(
      Species,
      all_of(group_variable),
      everything()
    )
  
  
  # --------------------------------------------------------------------
  # O. Create output filename
  # --------------------------------------------------------------------
  
  filename <- paste0(
    cycle,
    "_",
    group_value,
    "_Indicator_Species_Results.csv"
  )
  
  
  # --------------------------------------------------------------------
  # P. Save individual result
  # --------------------------------------------------------------------
  
  write.csv(
    significant_results,
    file.path(
      output_dir,
      filename
    ),
    row.names = FALSE
  )
  
  
  # --------------------------------------------------------------------
  # Q. Report progress
  # --------------------------------------------------------------------
  
  message(
    group_variable,
    " ",
    group_value,
    ": ",
    nrow(significant_results),
    " significant indicator species."
  )
  
  
  # --------------------------------------------------------------------
  # R. Return results
  # --------------------------------------------------------------------
  
  return(
    significant_results
  )
}


# ======================================================================
# 23. REGION-LEVEL INDICATOR SPECIES ANALYSIS
# ======================================================================

cat("\n")
cat("====================================================\n")
cat("REGION INDICATOR SPECIES ANALYSIS\n")
cat("====================================================\n")


regions_to_analyze <- sort(
  unique(
    na.omit(
      Plotdata$Region
    )
  )
)


cat(
  "Regions to analyze:",
  length(regions_to_analyze),
  "\n\n"
)


region_results_list <- lapply(
  
  regions_to_analyze,
  
  function(x) {
    
    run_indval(
      
      group_value = x,
      
      group_variable = "Region",
      
      metadata = Plotdata,
      
      spp = c1spp,
      
      output_dir = output_dir,
      
      cycle = "c1",
      
      alpha = 0.05,
      
      nperm = 9999
    )
    
  }
  
)


# ======================================================================
# 24. COMBINE REGION RESULTS
# ======================================================================

region_results <- bind_rows(
  region_results_list
)


# Save combined Region results
write.csv(
  
  region_results,
  
  file.path(
    output_dir,
    "c1_All_Region_Indicator_Species_Results.csv"
  ),
  
  row.names = FALSE
)


cat(
  "\nTotal significant Region associations:",
  nrow(region_results),
  "\n"
)


# ======================================================================
# 25. PSU-LEVEL INDICATOR SPECIES ANALYSIS
# ======================================================================

cat("\n")
cat("====================================================\n")
cat("PSU INDICATOR SPECIES ANALYSIS\n")
cat("====================================================\n")


psus_to_analyze <- sort(
  unique(
    na.omit(
      Plotdata$PSU
    )
  )
)


cat(
  "PSUs to analyze:",
  length(psus_to_analyze),
  "\n\n"
)


psu_results_list <- lapply(
  
  psus_to_analyze,
  
  function(x) {
    
    run_indval(
      
      group_value = x,
      
      group_variable = "PSU",
      
      metadata = Plotdata,
      
      spp = c1spp,
      
      output_dir = output_dir,
      
      cycle = "c1",
      
      alpha = 0.05,
      
      nperm = 9999
    )
    
  }
  
)


# ======================================================================
# 26. COMBINE PSU RESULTS
# ======================================================================

psu_results <- bind_rows(
  psu_results_list
)


# Save combined PSU results
write.csv(
  
  psu_results,
  
  file.path(
    output_dir,
    "c1_All_PSU_Indicator_Species_Results.csv"
  ),
  
  row.names = FALSE
)


cat(
  "\nTotal significant PSU associations:",
  nrow(psu_results),
  "\n"
)


# ======================================================================
# 27. CREATE PSU DIAGNOSTIC TABLE
# ======================================================================

PSU_diagnostics <- Plotdata %>%
  
  group_by(PSU) %>%
  
  summarise(
    
    Number_of_Plots = n(),
    
    Number_of_Habitats = n_distinct(
      habitat,
      na.rm = TRUE
    ),
    
    Habitats = paste(
      sort(unique(habitat)),
      collapse = "; "
    ),
    
    .groups = "drop"
  )


# Add number of species represented within each PSU
PSU_species_counts <- lapply(
  
  PSU_diagnostics$PSU,
  
  function(x) {
    
    keep <- Plotdata$PSU == x
    
    spp_temp <- c1spp[
      keep,
      ,
      drop = FALSE
    ]
    
    sum(
      colSums(
        spp_temp,
        na.rm = TRUE
      ) > 0
    )
    
  }
  
)


PSU_diagnostics$Number_of_Species <- unlist(
  PSU_species_counts
)


# Determine whether PSU has enough habitat groups
PSU_diagnostics <- PSU_diagnostics %>%
  
  mutate(
    
    Analysis_Status = case_when(
      
      Number_of_Plots < 2 ~
        "Not analyzed: fewer than 2 plots",
      
      Number_of_Habitats < 2 ~
        "Not analyzed: only one habitat",
      
      Number_of_Species == 0 ~
        "Not analyzed: no species",
      
      TRUE ~
        "Eligible for analysis"
    )
    
  )


# Save PSU diagnostics
write.csv(
  
  PSU_diagnostics,
  
  file.path(
    output_dir,
    "c1_PSU_Analysis_Diagnostics.csv"
  ),
  
  row.names = FALSE
)


# ======================================================================
# 28. CREATE REGION DIAGNOSTIC TABLE
# ======================================================================

Region_diagnostics <- Plotdata %>%
  
  group_by(Region) %>%
  
  summarise(
    
    Number_of_Plots = n(),
    
    Number_of_PSUs = n_distinct(
      PSU
    ),
    
    Number_of_Habitats = n_distinct(
      habitat,
      na.rm = TRUE
    ),
    
    Habitats = paste(
      sort(unique(habitat)),
      collapse = "; "
    ),
    
    .groups = "drop"
  )


# Save Region diagnostics
write.csv(
  
  Region_diagnostics,
  
  file.path(
    output_dir,
    "c1_Region_Analysis_Diagnostics.csv"
  ),
  
  row.names = FALSE
)


# ======================================================================
# 29. FINAL SUMMARY
# ======================================================================

cat("\n")
cat("====================================================\n")
cat("CYCLE 1 INDICATOR SPECIES ANALYSIS COMPLETE\n")
cat("====================================================\n")

cat(
  "Plots analyzed:",
  nrow(c1spp),
  "\n"
)

cat(
  "Species analyzed:",
  ncol(c1spp),
  "\n"
)

cat(
  "Regions considered:",
  length(regions_to_analyze),
  "\n"
)

cat(
  "PSUs considered:",
  length(psus_to_analyze),
  "\n"
)

cat(
  "Significant Region indicator associations:",
  nrow(region_results),
  "\n"
)

cat(
  "Significant PSU indicator associations:",
  nrow(psu_results),
  "\n"
)

cat(
  "\nResults saved to:\n",
  output_dir,
  "\n"
)

cat("====================================================\n")

