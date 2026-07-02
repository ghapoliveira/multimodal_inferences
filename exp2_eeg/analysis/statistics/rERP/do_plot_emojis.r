# do_plot_emojis.r
# This script depends on the functions in plot_rERP.r, benjamini-hochberg.r,
# both by Christoph Aurnhammer (github.com/caurnhammer/psyp23rerps).
# It assumes that the outputs of do_rERP.ipynb are in ./rERP_outputs/
# It plots the images in the 4th chapter of the Dissertation (rERP section)
#
# See requirements_R.txt

# Load the libraries
library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)

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

# 3: Main plotting function
make_plots_emojis <- function(
    desc_file,                 # WITHIN-subjects: coefficients, estimates, residuals, observed, topoplots
    infer_file,                # ACROSS-subjects: t-value grid only
    elec_all,
    out_file  = desc_file,     # where plots are written
    elec_nine = c("F3", "Fz", "F4", "C3", "Cz", "C4", "P3", "Pz", "P4"),
    predictor = c("Intercept", "Semantic_Score", "Info_Score", "Mean_Visual_Error")
) {

  # Create output folders
  dir.create(paste0(out_file, "_plots"), showWarnings = FALSE)
  dir.create(paste0(out_file, "_plots/Waveforms"), showWarnings = FALSE)
  dir.create(paste0(out_file, "_plots/Topoplots"), showWarnings = FALSE)

  clean_predictors <- c("Intercept", "Meaning", "Info", "VError")
  model_labs <- c("Intercept", "Meaning", "Info", "VError")
  model_vals <- c("black", "#004488", "#BB5566", "#228833")
  time_windows <- list(c(250, 350), c(300, 500), c(500, 1000))

  # Description  (WITHIN file: real between-subjects CIs)

  mod <- fread(paste0(desc_file, "_models.csv"))
  mod$Spec <- factor(mod$Spec, levels = predictor, labels = clean_predictors)

  coef <- mod[Type == "Coefficient", ]
  coef$Condition <- coef$Spec

  # A. Coefficients (3x3 grid)
  plot_nine_elec(data = coef, e = elec_nine,
                 file = paste0(out_file, "_plots/Waveforms/Coefficients_Grid.pdf"),
                 title = "rERP Coefficients (3x3 Grid)",
                 modus = "Coefficient", ylims = c(12, -8), ci = TRUE,
                 leg_labs = model_labs, leg_vals = model_vals)

  # Coefficients at Cz and Pz
  for (e_focus in c("Cz", "Fz", "Pz")) {
    plot_single_elec(data = coef, e = e_focus,
                     file = paste0(out_file, "_plots/Waveforms/Fig12_Coefficients_", e_focus, ".pdf"),
                     modus = "Coefficient", ylims = c(12, -8), ci = TRUE,
                     leg_labs = model_labs, leg_vals = model_vals,
                     title = paste("Regression Coefficients at", e_focus))
  }

  # B. Coefficient topoplots
  for (tw in time_windows) {
    plot_topo(data = coef, file = paste0(out_file, "_plots/Topoplots/Topo"),
              tw = tw, cond_man = "Meaning", cond_base = "Intercept",
              subtitle = "Meaning coefficient")
    plot_topo(data = coef, file = paste0(out_file, "_plots/Topoplots/Topo"),
              tw = tw, cond_man = "Info", cond_base = "Intercept",
              subtitle = "Info coefficient")
    plot_topo(data = coef, file = paste0(out_file, "_plots/Topoplots/Topo"),
              tw = tw, cond_man = "VError", cond_base = "Intercept",
              subtitle = "VError coefficient")
  }

  # INFERENCE  (ACROSS file: pooled t-values / p-values)

  mod_infer <- fread(paste0(infer_file, "_models.csv"))
  mod_infer$Spec <- factor(mod_infer$Spec, levels = predictor, labels = clean_predictors)

  tval <- mod_infer[Type == "t-value" & Spec != "Intercept", ]
  sig  <- mod_infer[Type == "p-value" & Spec != "Intercept", ]
  colnames(sig) <- gsub("_CI", "_sig", colnames(sig))

  sig_corr <- bh_apply_wide(sig, elec_all, alpha = 0.05, tws = time_windows)
  sigcols  <- grepl("_sig", colnames(sig_corr))
  tval <- cbind(tval, sig_corr[, ..sigcols])
  tval$Condition <- factor(tval$Spec, levels = clean_predictors)

  plot_nine_elec(tval, elec_nine,
                 file = paste0(out_file, "_plots/Waveforms/t-values_Grid.pdf"),
                 title = "Inferential statistics (FDR-corrected T-values)",
                 modus = "t-value", ylims = c(12, -8), tws = time_windows,
                 ci = FALSE,
                 leg_labs = model_labs[2:4], leg_vals = model_vals[2:4])

  # DATA SECTION  (WITHIN file: estimates, residuals, observed)

  eeg <- fread(paste0(desc_file, "_data.csv"))
  eeg$Condition <- factor(eeg$Condition, levels = c(1, 2, 3),
                          labels = c("Cond_A", "Cond_B", "Cond_C"))

  obs <- eeg[Type == "EEG", ]
  data_labs <- c("Cond_A", "Cond_B", "Cond_C")
  data_vals <- c("black", "red", "blue")

  # D. Observed ERPs
  plot_nine_elec(data = obs, e = elec_nine,
                 file = paste0(out_file, "_plots/Waveforms/Observed_Grid.pdf"),
                 title = "Observed ERPs (3x3 Grid)", modus = "Condition",
                 ylims = c(12, -8), ci = TRUE, leg_labs = data_labs, leg_vals = data_vals)

  plot_full_elec(data = obs, e = elec_all,
                 file = paste0(out_file, "_plots/Waveforms/Observed_FullGrid.pdf"),
                 title = "Observed ERPs (Full Grid)", modus = "Condition",
                 ylims = c(14, -10), ci = TRUE, leg_labs = data_labs, leg_vals = data_vals)

  for (tw_idx in time_windows) {
    tw_str <- paste0(tw_idx[1], "-", tw_idx[2])
    plot_topo(obs, file = paste0(out_file, "_plots/Topoplots/Observed"),
              tw = tw_idx, cond_man = "Cond_B", cond_base = "Cond_A",
              add_title = paste0("\nObserved B-A (", tw_str, " ms)"),
              subtitle = "Observed data")
    plot_topo(obs, file = paste0(out_file, "_plots/Topoplots/Observed"),
              tw = tw_idx, cond_man = "Cond_C", cond_base = "Cond_A",
              add_title = paste0("\nObserved C-A (", tw_str, " ms)"),
              subtitle = "Observed data")
  }

  # E. Full-model estimated ERPs at Pz
  est <- eeg[Type == "est", ]
  full_model_spec <- unique(est$Spec)[length(unique(est$Spec))]
  est_full <- est[Spec == full_model_spec, ]

  clean_name_full <- full_model_spec
  clean_name_full <- gsub("\\[|\\]|:| ", "", clean_name_full)
  clean_name_full <- gsub(",", "+", clean_name_full)
  clean_name_full <- gsub("Semantic_Score", "Meaning", clean_name_full)
  clean_name_full <- gsub("Info_Score", "Info", clean_name_full)
  clean_name_full <- gsub("Mean_Visual_Error", "VError", clean_name_full)

  plot_single_elec(est_full, "Pz",
                   file = paste0(out_file, "_plots/Waveforms/Estimated_Pz_FullModel.pdf"),
                   modus = "Condition", ylims = c(12, -8), ci = TRUE,
                   leg_labs = data_labs, leg_vals = data_vals,
                   title = "Estimated ERPs: Full Model")

  # F. Estimated topoplots (full model)
  for (tw_est in time_windows) {
    plot_topo(est_full, file = paste0(out_file, "_plots/Topoplots/Estimated_FullModel"),
              tw = tw_est, cond_man = "Cond_B", cond_base = "Cond_A",
              add_title = paste("\nEstimate B-A", clean_name_full), omit_legend = TRUE,
              subtitle = "Intercept + Meaning + Info + VError")
    plot_topo(est_full, file = paste0(out_file, "_plots/Topoplots/Estimated_FullModel"),
              tw = tw_est, cond_man = "Cond_C", cond_base = "Cond_A",
              add_title = paste("\nEstimate C-A", clean_name_full), omit_legend = TRUE,
              subtitle = "Intercept + Meaning + Info + VError")
  }

  # G. Residuals + combined (estimated | residuals, side by side)
  res <- eeg[Type == "res", ]
  res_set <- res[Spec == full_model_spec, ]

  plot_single_elec(res_set, "Pz",
                   file = paste0(out_file, "_plots/Waveforms/Residuals_Pz_FullModel.pdf"),
                   modus = "Condition", ylims = c(4, -4), ci = TRUE,
                   leg_labs = data_labs, leg_vals = data_vals,
                   title = "Residuals (Observed - Estimated)",
                   omit_legend = TRUE, save_legend = FALSE)

  p_est <- plot_single_elec(est_full, "Pz", modus = "Condition",
                            ylims = c(12, -8), ci = TRUE,
                            leg_labs = data_labs, leg_vals = data_vals,
                            title = "Estimated ERPs (Full Model)")
  p_res <- plot_single_elec(res_set, "Pz", modus = "Condition", ylims = c(4, -4),
                            ci = TRUE, leg_labs = data_labs, leg_vals = data_vals,
                            title = "Residuals")

  ggsave(paste0(out_file, "_plots/Waveforms/Fig11_Pz_Combined_Est_Res.pdf"),
         grid.arrange(p_est, p_res, ncol = 2), device = cairo_pdf, width = 9, height = 4)

  # H. Isolated predictor estimates at Fz/Cz/Pz, per condition
  specs_to_overlay <- c("[:Intercept, :Semantic_Score]",
                        "[:Intercept, :Info_Score]",
                        "[:Intercept, :Mean_Visual_Error]")

  for (cond_focus in c("Cond_A", "Cond_B", "Cond_C")) {
    est_combined <- est[Spec %in% specs_to_overlay & Condition == cond_focus, ]
    est_combined$Spec <- gsub("\\[|\\]|:| ", "", est_combined$Spec)
    est_combined$Spec <- gsub("Intercept,", "", est_combined$Spec)
    est_combined$Spec <- gsub("Semantic_Score", "Meaning", est_combined$Spec)
    est_combined$Spec <- gsub("Info_Score", "Info", est_combined$Spec)
    est_combined$Spec <- gsub("Mean_Visual_Error", "VError", est_combined$Spec)
    est_combined$Spec <- factor(est_combined$Spec, levels = c("Meaning", "Info", "VError"))

    for (e_focus in c("Fz", "Cz", "Pz")) {
      plot_single_elec(est_combined, e_focus,
                       file = paste0(out_file, "_plots/Waveforms/Fig13_Combined_Estimates_", e_focus, "_", cond_focus, ".pdf"),
                       modus = "Coefficient", ylims = c(12, -8), ci = TRUE,
                       leg_labs = c("Semantics Only", "Information Only", "VError Only"),
                       leg_vals = c("#004488", "#BB5566", "#228833"),
                       title = paste("Isolated Predictor Estimates at", e_focus, "(", cond_focus, ")"))
    }
  }

  cat("\nProcess Complete! Results are in:", paste0(out_file, "_plots"), "\n")
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
