# File name: vegetation_summary.R
# Author: Dan Gann, with extensive modifications by Jed Redwine
# email: gannd@fiu.edu, jredwine@fiu.edu

# Required: aggregated vegetation data designed for modeling - vg_mdl.new.csv


library(glmmTMB)

## ---- 1. Configuration --------------------------------------------------------
#filter the dataset for repeated observations
library(dplyr)
vg_mdl.c3 <- read.csv('./data/processed/vg_mdl.new.csv') 
unique(dat$Cycle)                       # confirm the label is "C3"; edit below if it differs
c3_label <- "C3"

## plots with a usable observation in cycle 3
plots_c3 <- vg_mdl.c3 %>%
  dplyr::filter(as.character(Cycle) == c3_label, !is.na(p_b)) %>%
  dplyr::pull(PlotID) %>%
  as.character() %>%
  unique()

## keep all cycles for those plots
dat <- vg_mdl.c3 %>%
  dplyr::filter(as.character(PlotID) %in% plots_c3) %>%
  droplevels()


#configure files
groups    <- c("ridge", "slough", "transition")
x_level   <- "Cl_u"    # unit at which median ridge cover (X) is computed
slope_lvl <- "Region"   # unit carrying u0 and gamma; must be COARSER than x_level
# alternative: x_level <- "Cl_u"; slope_lvl <- "PSU_u"

need <- c("p", "FNCODE", "Region", "PSU_u", "Cl_u", "PlotID", "Cycle")
stopifnot(all(need %in% names(dat)), x_level %in% names(dat))
for (v in c("Cycle", "Region", "PSU_u", "Cl_u", "PlotID")) dat[[v]] <- factor(dat[[v]])

## ---- 2. Predictor X: median ridge cover (%) per x_level unit, z-scored -------
rid     <- dat[dat$FNCODE == "ridge", ]
x_med   <- sapply(split(rid$p * 100, rid[[x_level]]), median)
x_z     <- (x_med - mean(x_med, na.rm = TRUE)) / sd(x_med, na.rm = TRUE)
dat$x_z <- unname(x_z[as.character(dat[[x_level]])])
print(round(x_med, 1))

# Quick diagnostic box plot
boxplot(x_med, 
        main = "Diagnostic Boxplot of Custer median pct_cover", 
        ylab = "Values", 
        col = "lightblue")

## ---- 3. Model ladder (identical family, zero-inflation and precision throughout)
desc <- c(
  M0_null               = "Null: intercept only",
  M1_plot               = "+ plot random intercept (repeated measures only)",
  M2_cycle              = "+ Cycle (fixed)",
  M3_cluster            = "+ cluster random intercept",
  M4_psu                = "+ PSU random intercept",
  M5_region             = "+ Region (fixed)",
  M6_regionXcycle       = "+ Region x Cycle (fixed)",
  M7_slope_fixed        = "X fixed slope (beta1), replaces Region",
  M8_rand_int           = "+ random intercept u0i",
  M9_rand_slope         = "+ random slope gamma_i (correlated with u0i)",
  M10_rand_slope_uncorr = "+ random slope gamma_i (uncorrelated with u0i)"
)

build_ladder <- function(G) {
  ran_nested <- "(1 | PlotID) + (1 | Cl_u) + (1 | PSU_u)"
  ran_noG <- switch(G,
                    Region = ran_nested,                       # slope unit not yet in the model
                    PSU_u  = "(1 | PlotID) + (1 | Cl_u)",      # PSU intercept re-enters via the G terms
                    stop("slope_lvl must be 'Region' or 'PSU_u'"))
  c(
    M0_null         = "p_b ~ 1",
    M1_plot         = "p_b ~ 1 + (1 | PlotID)",
    M2_cycle        = "p_b ~ Cycle + (1 | PlotID)",
    M3_cluster      = "p_b ~ Cycle + (1 | PlotID) + (1 | Cl_u)",
    M4_psu          = paste("p_b ~ Cycle +", ran_nested),
    M5_region       = paste("p_b ~ Region + Cycle +", ran_nested),
    M6_regionXcycle = paste("p_b ~ Region * Cycle +", ran_nested),
    M7_slope_fixed  = paste("p_b ~ Cycle + x_z +", ran_noG),
    M8_rand_int     = paste0("p_b ~ Cycle + x_z + ", ran_noG, " + (1 | ", G, ")"),
    M9_rand_slope   = paste0("p_b ~ Cycle + x_z + ", ran_noG, " + (1 + x_z | ", G, ")"),
    M10_rand_slope_uncorr = paste0("p_b ~ Cycle + x_z + ", ran_noG, " + diag(1 + x_z | ", G, ")")
  )
}

fit_one <- function(fml, d) {
  tryCatch(
    glmmTMB(as.formula(fml), data = d,
            family = beta_family(link = "logit"), ziformula = ~ 1),
    error = function(e) { message("    failed: ", conditionMessage(e)); NULL })
}

aic_table <- function(mods) {
  tab <- data.frame(
    model       = names(mods),
    description = unname(desc[names(mods)]),
    n           = sapply(mods, nobs),
    k           = sapply(mods, function(m) attr(logLik(m), "df")),
    logLik      = sapply(mods, function(m) as.numeric(logLik(m))),
    AIC         = sapply(mods, AIC),
    pdHess      = sapply(mods, function(m) isTRUE(m$sdr$pdHess)),
    row.names   = NULL
  )
  stopifnot(length(unique(tab$n)) == 1)          # AIC needs identical data in every model
  ok   <- tab$pdHess
  best <- min(tab$AIC[ok], na.rm = TRUE)
  w    <- ifelse(ok, exp(-0.5 * (tab$AIC - best)), NA)
  tab$dAIC   <- ifelse(ok, tab$AIC - best, NA)
  tab$weight <- w / sum(w, na.rm = TRUE)
  tab$rank   <- ifelse(ok, rank(ifelse(ok, tab$AIC, Inf), ties.method = "min"), NA)
  tab[order(tab$pdHess, -tab$AIC), ]             # unreliable fits first, then worst -> best
}

fit_group <- function(g, G = slope_lvl) {
  d <- dat[dat$FNCODE == g, ]
  d <- d[complete.cases(d[, c("p_b", "Cycle", "Region", "PSU_u", "Cl_u", "PlotID", "x_z")]), ]
  d <- droplevels(d)                              # same rows for every model
  
  nx <- sapply(split(d$x_z, d[[G]]), function(v) length(unique(round(v, 8))))
  if (all(nx < 2)) stop("X is constant within every ", G, " unit: the random slope is ",
                        "not identifiable. Use a coarser slope_lvl or a finer x_level.")
  if (min(nx) < 3) warning(g, ": X has < 3 distinct values in some ", G,
                           " units; slope variance will be poorly estimated.")
  message(g, ": ", nrow(d), " obs; ", nlevels(d[[G]]), " ", G, " units")
  
  fmls <- build_ladder(G)
  mods <- lapply(names(fmls), function(nm) { message("  fitting ", nm); fit_one(fmls[[nm]], d) })
  names(mods) <- names(fmls)
  mods <- Filter(Negate(is.null), mods)
  list(data = d, formulas = fmls, models = mods, aic = aic_table(mods))
}

## ---- 4. Fit all groups -------------------------------------------------------
res <- lapply(groups, fit_group); names(res) <- groups

unique(dat$Region)

## ---- 5. AIC summary: worst (top) to best (bottom) ---------------------------
fmt <- function(t) {
  t$logLik <- round(t$logLik, 1); t$AIC <- round(t$AIC, 1)
  t$dAIC <- round(t$dAIC, 2);     t$weight <- round(t$weight, 3); t
}
for (g in groups) {
  cat("\n=====", g, "=====\n")
  print(fmt(res[[g]]$aic), row.names = FALSE)
}

aic_all <- do.call(rbind, Map(function(tab, g) cbind(FNCODE = g, tab),
                              lapply(res, `[[`, "aic"), groups))
rownames(aic_all) <- NULL

## ---- 6. Helpers --------------------------------------------------------------
best_model <- function(r) r$models[[ r$aic$model[which.min(r$aic$rank)] ]]
m_best <- best_model(res$ridge); summary(m_best)

# optional LRTs along the nested chain M0 -> M4
with(res$ridge$models, anova(M0_null, M1_plot, M2_cycle, M3_cluster, M4_psu))

# optional LRTs along the nested chain M5 -> M10
with(res$ridge$models, anova(M5_region, M6_regionXcycle, M7_slope_fixed, M8_rand_int, M9_rand_slope, M10_rand_slope_uncorr))


## ---- 7. Evaluate results --------------------------------------------------------------

VarCorr(res$ridge$models$M9_rand_slope)

res <- lapply(groups, fit_group); names(res) <- groups #this works 
lapply(res, function(r) anova(r$M10_rand_slope_uncorr, r$M0_null, r$M7_slope_fixed))      # do PSU and Cluster matter?
lapply(res, function(r) anova(r$m_noc, r$m_add, r$m2)) # Cycle effect, then Region x Cycle
lapply(res, function(r) VarCorr(r$m2))
# AIC(m0, m1, m2, m_add, M_noc)


#Reporting Change
library(emmeans)
emm <- emmeans(res$ridge$models$M10_rand_slope_uncorr, ~ Cycle, type = "response")
pairs(emm)                                          # C1 vs C2, C2 vs C3, C1 vs C3

emm_r <- emmeans(res$ridge$models$M6_regionXcycle, ~ Cycle | Region, type = "response")
pairs(emm_r)                                        # region-specific change results seem reasonable, only works when Region isn't a random effect


