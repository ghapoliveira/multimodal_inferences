# do_plot_emojis.r
# This script depends on the functions in plot_rERP.r and benjamini-hochberg.r,
# both by Christoph Aurnhammer (github.com/caurnhammer/psyp23rerps).
# It assumes that the outputs of do_rERP.ipynb are in ./rERP_outputs/
# It plots the images in the 4th chapter of the Dissertation (rERP section) — and so part of it deals with renaming
# Conditions A, B, and C to the more intuitive names given, bridged, and new, respectively.
#
# See requirements_R.txt

# Load the libraries
library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)
library(ggtext)

# Force ggplot2 to use Times New Roman
theme_set(theme_get() + theme(text = element_text(family = "Times New Roman")))

# Force base R graphics to use Times New Roman
par(family = "Times New Roman")

# 1: Coordinate preparation
prepare_loc_from_ced <- function(input_file, output_file = "standard_30_clean.loc") {
  ced <- fread(input_file, fill = TRUE)
  ced[, radius := radius * 0.95]
  loc_data <- ced[, .(Number, theta, radius, labels)]
  fwrite(loc_data, output_file, sep = "\t", col.names = FALSE)
  cat("Success: Coordinate file created ->", output_file, "\n")
}

# 2: Load toolboxes
source("plot_rERP.r")
source("benjamini-hochberg.r")

# Recodes raw predictor variable names into clean labels.
# Used for both the full-model title string and the per-row Spec
# column of the combined-estimate plots.
clean_predictor_names <- function(x) {
  x <- gsub("Semantic_Score", "Coherence", x)
  x <- gsub("Info_Score", "Information Addition", x)
  x <- gsub("Mean_Visual_Error", "Visual Clarity", x)
  x
}

# Runs plot_topo() over every (time window x condition) combination.
run_topos <- function(data, file_prefix, mans, base, time_windows,
                      subtitle_fn, title_fn = NULL,
                      omit_legend = FALSE, save_legend = FALSE) {
  for (tw in time_windows) {
    for (man in mans) {
      args <- list(data = data, file = file_prefix, tw = tw,
                  cond_man = man, cond_base = base,
                  omit_legend = omit_legend, save_legend = save_legend,
                  subtitle = subtitle_fn(man))
      if (!is.null(title_fn)) args$add_title <- title_fn(man, tw)
      do.call(plot_topo, args)
    }
  }
}

# 3: Main plotting function
make_plots_emojis <- function(
    desc_file,                 # WITHIN-subjects: coefficients, estimates, residuals, observed, topoplots
    infer_file,                # ACROSS-subjects: t-value grid only
    elec_all,
    out_file  = desc_file,     # where plots are written
    elec_nine = c("F3", "Fz", "F4", "C3", "Cz", "C4", "P3", "Pz", "P4"),
    predictor = c("Intercept", "Semantic_Score", "Info_Score", "Mean_Visual_Error")
) {

  # Output folders + short path helpers (replace repeated paste0(out_file, "_plots/...") calls)
  plots_dir <- paste0(out_file, "_plots")
  for (sub in c("Waveforms", "Topoplots")) dir.create(file.path(plots_dir, sub), recursive = TRUE, showWarnings = FALSE)
  wf   <- function(name) file.path(plots_dir, "Waveforms", name)
  topo <- function(name) file.path(plots_dir, "Topoplots", name)

  clean_predictors <- c("Intercept", "Coherence", "Information Addition", "Visual Clarity")
  model_labs <- clean_predictors
  model_vals <- c("black", "#004488", "#BB5566", "#228833")
  time_windows <- list(c(250, 300), c(300, 500), c(500, 1000))
  to_clean_spec <- function(x) factor(x, levels = predictor, labels = clean_predictors)

  # Description  (WITHIN file: real between-subjects CIs)

  mod <- fread(paste0(desc_file, "_models.csv"))
  mod$Spec <- to_clean_spec(mod$Spec)

  coef <- mod[Type == "Coefficient", ]
  coef$Condition <- coef$Spec

  # A. Coefficients (3x3 grid + single-electrode focus plots)
  plot_nine_elec(data = coef, e = elec_nine, file = wf("Coefficients_Grid.pdf"),
                 title = "rERP Coefficients (3x3 Grid)",
                 modus = "Coefficient", ylims = c(12, -8), ci = TRUE,
                 leg_labs = model_labs, leg_vals = model_vals)

  for (e_focus in c("Cz", "Fz", "Pz")) {
    plot_single_elec(data = coef, e = e_focus, file = wf(paste0("Fig12_Coefficients_", e_focus, ".pdf")),
                     modus = "Coefficient", ylims = c(12, -8), ci = TRUE,
                     leg_labs = model_labs, leg_vals = model_vals,
                     title = "Regression Coefficients")
  }

  # B. Coefficient topoplots (each predictor vs. Intercept)
  run_topos(coef, topo("Topo"), mans = clean_predictors[-1], base = "Intercept",
           time_windows = time_windows, subtitle_fn = identity)

  # INFERENCE  (ACROSS file: pooled t-values / p-values)

  mod_infer <- fread(paste0(infer_file, "_models.csv"))
  mod_infer$Spec <- to_clean_spec(mod_infer$Spec)

  tval <- mod_infer[Type == "t-value" & Spec != "Intercept", ]
  sig  <- mod_infer[Type == "p-value" & Spec != "Intercept", ]
  colnames(sig) <- gsub("_CI", "_sig", colnames(sig))

  sig_corr <- bh_apply_wide(sig, elec_all, alpha = 0.05, tws = time_windows)
  sigcols  <- grepl("_sig", colnames(sig_corr))
  tval <- cbind(tval, sig_corr[, ..sigcols])
  tval$Condition <- factor(tval$Spec, levels = clean_predictors)

  plot_nine_elec(tval, elec_nine, file = wf("t-values_Grid.pdf"),
               title = expression(paste("Inferential statistics (FDR-corrected ", italic(t), "-values)")),
               modus = "t-value", ylims = c(12, -8), tws = time_windows,
               ci = FALSE,
               leg_labs = model_labs[2:4], leg_vals = model_vals[2:4])

  # DATA SECTION  (WITHIN file: estimates, residuals, observed)

  eeg <- fread(paste0(desc_file, "_data.csv"))

  # Set dataframe conditions as plain text so the string-matching logic in plot_topo works
  eeg$Condition <- factor(eeg$Condition, levels = c(1, 2, 3), labels = c("Given", "Bridged", "New"))

  obs <- eeg[Type == "EEG", ]

  # Set the plot labels to use mathematical italics
  data_labs <- expression(italic("Given"), italic("Bridged"), italic("New"))
  data_vals <- c("black", "red", "blue")

  # D. Observed ERPs (grids + topoplots vs. Given)
  plot_nine_elec(data = obs, e = elec_nine, file = wf("Observed_Grid.pdf"),
                 title = "Observed ERPs (3x3 Grid)", modus = "Condition",
                 ylims = c(12, -8), ci = TRUE, leg_labs = data_labs, leg_vals = data_vals)

  plot_full_elec(data = obs, e = elec_all, file = wf("Observed_FullGrid.pdf"),
                 title = "Observed ERPs (Full Grid)", modus = "Condition",
                 ylims = c(14, -10), ci = TRUE, leg_labs = data_labs, leg_vals = data_vals)

  run_topos(obs, topo("Observed"), mans = c("Bridged", "New"), base = "Given",
           time_windows = time_windows,
           subtitle_fn = function(man) "Observed data",
           title_fn = function(man, tw) paste0("\nObserved ", man, " – Given (", tw[1], "-", tw[2], " ms)"))

  # E. Full-model estimated ERPs at Pz
  est <- eeg[Type == "est", ]
  full_model_spec <- unique(est$Spec)[length(unique(est$Spec))]
  est_full <- est[Spec == full_model_spec, ]

  clean_name_full <- full_model_spec
  clean_name_full <- gsub("\\[|\\]|:| ", "", clean_name_full)
  clean_name_full <- gsub(",", "+", clean_name_full)
  clean_name_full <- clean_predictor_names(clean_name_full)

  # NOTE: this call passes `file=`, which makes plot_single_elec() return a specially
  # composed legend layout (small keys, wrapped to 2 rows) sized for a 3-inch-wide save.
  # It is NOT the same object as the one used in the combined Fig11 figure below —
  # that one omits `file=` on purpose to get ggplot's raw/default legend, which only
  # fits because Fig11 is saved much wider (9in for two panels). Do not consolidate
  # these two calls: they use different titles too.
  plot_single_elec(est_full, "Pz", file = wf("Estimated_Pz_FullModel.pdf"),
                   modus = "Condition", ylims = c(12, -8), ci = TRUE,
                   leg_labs = data_labs, leg_vals = data_vals, title = "Estimated ERPs: Full Model")

  # F. Estimated topoplots (full model, vs. Given)
  full_model_desc <- "Intercept + Coherence + Information Addition + Visual Clarity"
  run_topos(est_full, topo("Estimated_FullModel"), mans = c("Bridged", "New"), base = "Given",
           time_windows = time_windows,
           subtitle_fn = function(man) full_model_desc,
           title_fn = function(man, tw) paste("\nEstimate", man, "– Given", clean_name_full),
           omit_legend = TRUE)

  # G. Residuals + combined (estimated | residuals, side by side)
  res <- eeg[Type == "res", ]
  res_set <- res[Spec == full_model_spec, ]

  # Same note as above: file= here gives the properly composed legend for the 3-inch save.
  plot_single_elec(res_set, "Pz", file = wf("Residuals_Pz_FullModel.pdf"),
                   modus = "Condition", ylims = c(4, -4), ci = TRUE,
                   leg_labs = data_labs, leg_vals = data_vals,
                   title = "Residuals (Observed – Estimated)",
                   omit_legend = TRUE, save_legend = FALSE)

  # Combined figure: deliberately calls plot_single_elec() again without `file=` to get
  # the raw ggplot objects (default legend, has a legend on both panels), sized for the
  # wider 9-inch combined save.
  p_est <- plot_single_elec(est_full, "Pz", modus = "Condition", ylims = c(12, -8), ci = TRUE,
                            leg_labs = data_labs, leg_vals = data_vals, title = "Estimated ERPs (Full Model)")
  p_res <- plot_single_elec(res_set, "Pz", modus = "Condition", ylims = c(4, -4), ci = TRUE,
                            leg_labs = data_labs, leg_vals = data_vals, title = "Residuals")

  ggsave(wf("Fig11_Pz_Combined_Est_Res.pdf"), grid.arrange(p_est, p_res, ncol = 2),
         device = cairo_pdf, width = 9, height = 4)

  # H. Isolated predictor estimates at Fz/Cz/Pz, per condition
  specs_to_overlay <- c("[:Intercept, :Semantic_Score]", "[:Intercept, :Info_Score]", "[:Intercept, :Mean_Visual_Error]")
  overlay_labs <- c("Coherence", "Information Addition", "Visual Clarity")
  overlay_vals <- c("#004488", "#BB5566", "#228833")

  for (cond_focus in c("Given", "Bridged", "New")) {
    est_combined <- est[Spec %in% specs_to_overlay & Condition == cond_focus, ]
    est_combined$Spec <- gsub("\\[|\\]|:| ", "", est_combined$Spec)
    est_combined$Spec <- gsub("Intercept,", "", est_combined$Spec)
    est_combined$Spec <- clean_predictor_names(est_combined$Spec)
    est_combined$Spec <- factor(est_combined$Spec, levels = overlay_labs)

    for (e_focus in c("Fz", "Cz", "Pz")) {
      plot_single_elec(est_combined, e_focus,
                       file = wf(paste0("Fig13_Combined_Estimates_", e_focus, "_", cond_focus, ".pdf")),
                       modus = "Coefficient", ylims = c(12, -8), ci = TRUE,
                       leg_labs = overlay_labs, leg_vals = overlay_vals,
                       title = bquote("Isolated Predictor Estimates (" * italic(.(cond_focus)) * ")"))
    }
  }

  cat("\nProcess Complete! Results are in:", plots_dir, "\n")
}

# 4: Execution
# The electrode coordinate file (standard_30_clean.loc) was generated once from the
# EEGLAB .ced (https://sccn.ucsd.edu/download/locfiles/eeglab/Standard-10-20-Cap81.ced)
# with the line below, and is committed to the repo alongside the original file — no need to rerun:

#   prepare_loc_from_ced(input_file = "Standard-10-20-Cap81.ced.txt")

my_30_electrodes <- c("Fp1", "Fp2", "F7", "F3", "Fz", "F4", "F8", "FT9", "FC5", "FC1",
                      "FC2", "FC6", "FT10", "T7", "C3", "Cz", "C4", "T8", "CP5", "CP1",
                      "CP2", "CP6", "P7", "P3", "Pz", "P4", "P8", "O1", "Oz", "O2")

make_plots_emojis(
  desc_file  = "rERP_outputs/rERPs_Emojis_Within",
  infer_file = "rERP_outputs/rERPs_Emojis_Across",
  out_file   = "rERPs_Emojis",
  elec_all   = my_30_electrodes
)