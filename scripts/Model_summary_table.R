library(dplyr); library(gt)

model_tab <- data.frame(
  id = c("M0_null", "M1_plot", "M2_cycle", "M3_cluster", "M4_psu", "M5_region",
         "M6_regionXcycle", "M7_slope_fixed", "M8_rand_int", "M9_rand_slope",
         "M10_rand_slope_uncorr"),
  Group = c("A. Baseline",
            rep("B. Repeated measures, time and spatial grouping", 4),
            rep("C. Regional effects as fixed categories", 2),
            rep("D. Median ridge cover (X) as predictor", 4)),
  Description = c(
    "Null: intercept only",
    "+ plot random intercept (repeated measures only)",
    "+ Cycle (fixed)",
    "+ cluster random intercept",
    "+ PSU random intercept",
    "+ Region (fixed)",
    "+ Region x Cycle (fixed)",
    "X fixed slope (beta1), replaces Region",
    "+ random intercept u0i",
    "+ random slope gamma_i (correlated with u0i)",
    "+ random slope gamma_i (uncorrelated with u0i)"),
  Builds_on = c("—", "M0", "M1", "M2", "M3", "M4", "M5", "M4", "M7", "M8",
                "M9, correlation fixed at 0"),
  Fixed = c("β₀", "β₀", "β₀, Cycle", "β₀, Cycle", "β₀, Cycle",
            "β₀, Cycle, Region", "β₀, Cycle, Region, Region × Cycle",
            "β₀, Cycle, X", "β₀, Cycle, X", "β₀, Cycle, X", "β₀, Cycle, X"),
  Random = c("none", "plot", "plot", "plot, cluster", "plot, cluster, PSU",
             "plot, cluster, PSU", "plot, cluster, PSU",
             "plot, cluster, PSU", "plot, cluster, PSU, Region intercept",
             "plot, cluster, PSU, Region intercept and X slope (correlated)",
             "plot, cluster, PSU, Region intercept and X slope (uncorrelated)"),
  Predictor = c(
    "η = β₀",
    "η = β₀ + cⱼₖₗ",
    "η = β₀ + δₜ + cⱼₖₗ",
    "η = β₀ + δₜ + bⱼₖ + cⱼₖₗ",
    "η = β₀ + δₜ + aⱼ + bⱼₖ + cⱼₖₗ",
    "η = β₀ + αᵣ + δₜ + aⱼ + bⱼₖ + cⱼₖₗ",
    "η = β₀ + αᵣ + δₜ + (αδ)ᵣₜ + aⱼ + bⱼₖ + cⱼₖₗ",
    "η = β₀ + β₁Xⱼ + δₜ + aⱼ + bⱼₖ + cⱼₖₗ",
    "η = (β₀ + u₀ᵣ) + β₁Xⱼ + δₜ + aⱼ + bⱼₖ + cⱼₖₗ",
    "η = (β₀ + u₀ᵣ) + (β₁ + γᵣ)Xⱼ + δₜ + aⱼ + bⱼₖ + cⱼₖₗ",
    "η = (β₀ + u₀ᵣ) + (β₁ + γᵣ)Xⱼ + δₜ + aⱼ + bⱼₖ + cⱼₖₗ"),
  stringsAsFactors = FALSE
)
model_tab$Model <- sub("_.*", "", model_tab$id)

# parameter counts from the fitted models (same across functional groups)
k_vec <- if (exists("res")) {
  sapply(res$ridge$models, function(m) attr(logLik(m), "df"))
} else setNames(rep(NA_real_, nrow(model_tab)), model_tab$id)
model_tab$k <- unname(k_vec[model_tab$id])

tab_gt <- model_tab %>%
  dplyr::select(Group, Model, Description, Builds_on, Fixed, Random, Predictor, k) %>%
  gt(groupname_col = "Group", rowname_col = "Model") %>%
  tab_header(title = "Candidate models for functional-group cover",
             subtitle = "Ordered from simplest to most complex") %>%
  cols_label(Builds_on = "Builds on", Fixed = "Fixed effects",
             Random = "Random effects", Predictor = "Linear predictor",
             k = "Parameters (k)") %>%
  cols_align(align = "left", columns = -k) %>%
  cols_align(align = "center", columns = k) %>%
  cols_width(Description ~ px(200), Builds_on ~ px(95), Fixed ~ px(150),
             Random ~ px(170), Predictor ~ px(340), k ~ px(80)) %>%
  tab_style(style = cell_text(font = c("Cambria Math", default_fonts())),
            locations = cells_body(columns = Predictor)) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_row_groups()) %>%
  tab_source_note("Indices: r = region, j = PSU, k = cluster, l = plot, t = cycle. η is the logit of the conditional beta mean μ.") %>%
  tab_source_note("Response: cover proportion y. With probability π (logit π = ζ₀, constant) y = 0; otherwise y ~ Beta(μφ, (1 − μ)φ) with constant precision φ. Family and zero-inflation are identical in every model.") %>%
  tab_source_note("Fixed: Cycle (δₜ), Region (αᵣ), Region × Cycle ((αδ)ᵣₜ). Random: aⱼ ~ N(0, σ²PSU), bⱼₖ ~ N(0, σ²cluster), cⱼₖₗ ~ N(0, σ²plot), independent; (u₀ᵣ, γᵣ) ~ N(0, Σ), with Σ unstructured in M9 and diagonal in M10.") %>%
  tab_source_note("X: z-scored median ridge cover at the PSU level; β₁ is the logit change per SD of X. k counts all estimated parameters, including the zero-inflation intercept and precision.") %>%
  tab_options(table.font.size = 11, data_row.padding = px(3), heading.align = "left",
              table.border.top.style = "none", column_labels.font.weight = "bold",
              source_notes.font.size = 9)
tab_gt

out_dir <- "./tables"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
gtsave(tab_gt, file.path(out_dir, "model_summary.html"))
gtsave(tab_gt, file.path(out_dir, "model_summary.docx"))
# gtsave(tab_gt, file.path(out_dir, "model_summary.docx"))   # needs a recent gt version
# gtsave(tab_gt, file.path(out_dir, "model_summary.png"))    # needs webshot2 and Chrome