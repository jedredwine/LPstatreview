# Load the nlme library
library(nlme) #

# -------------------------------------------------------------------------
# STEP 1: Prep the database to match desired structure for building candidate 
# models using Maximum Likelihood (method = "ML"). We test different fixed effects
#  structures for predicting cover of ridge vegetation.
# -------------------------------------------------------------------------
library(dplyr)

# 1. Define your species lists
ridge_species <- unique(veg_cur$SPCODE[grepl("ridge", veg_cur$FNCODE)]) 
slough_species <- unique(veg_cur$SPCODE[grepl("slough", veg_cur$FNCODE)]) 
trans_species <- unique (veg_cur$SPCODE[grepl("transition", veg_cur$FUNCODE)])


library(tidyverse)
as_tibble(veg_cur)


# Group the data by plot and functional group, not yet working!

vg_mdl <- veg_cur %>%
  ungroup() %>%
    group_by(PlotID, Cycle, FNCODE) %>%
  summarise(
    total_cover = sum(Cover, na.rm = TRUE),
        # ridge_cover  = sum(Cover[FNCODE %in% ridge_species], na.rm = TRUE),
    # slough_cover = sum(Cover[FNCODE %in% slough_species], na.rm = TRUE),
    .groups = "drop"
  )

head(vg_mdl)

# -------------------------------------------------------------------------
# STEP 2: Fit candidate models using Maximum Likelihood (method = "ML")
# We test different fixed effects structures for predicting cover of ridge vegetation.
# -------------------------------------------------------------------------

# Model 1: Simplest model - only 'age' as a fixed predictor
model_base <- lme(distance ~ age, 
                  random = ~ 1 | Subject, 
                  data = veg_all, 
                  method = "ML") #

# Model 2: Add main effect of 'Sex'
model_sex <- lme(distance ~ age + Sex, 
                 random = ~ 1 | Subject, 
                 data = Orthodont, 
                 method = "ML")

# Model 3: Add the interaction effect 'age * Sex'
model_interaction <- lme(distance ~ age * Sex, 
                         random = ~ 1 | Subject, 
                         data = Orthodont, 
                         method = "ML")

# -------------------------------------------------------------------------
# STEP 2: Compare models using the anova() function
# -------------------------------------------------------------------------
model_comparison <- anova(model_base, model_sex, model_interaction) #
print(model_comparison)

# -------------------------------------------------------------------------
# STEP 3: Refit the winning model using REML for unbiased estimates
# -------------------------------------------------------------------------
# Let's say model_sex is preferred because it significantly improves fit 
# over model_base, but the interaction in model_interaction is not significant.

final_model <- update(model_sex, method = "REML") #
summary(final_model)
