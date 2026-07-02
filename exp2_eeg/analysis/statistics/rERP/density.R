# density.R
# Plots Density Scores for two of the predictors (Information and Meaning) — Chapter 4 (Dissertation) plots.
#
# See requirements_R.txt

# Load libraries
library(data.table)
library(ggplot2)
source("plot_rERP.r")

dir.create("Density_Scores", showWarnings = FALSE)

# 1. Load the data containing the scores
stimuli_full <- fread("./rERP_outputs/eeg_continuous_for_julia.csv")

# 2. Isolate unique values (1 row = 1 trial) to avoid skewing the density
stimuli_unique <- unique(stimuli_full[, .(Item, Condition, Semantic_Score, Info_Score)])

# 3. Format the Condition variable for plotting
stimuli_unique$Condition <- factor(tolower(stimuli_unique$Condition), 
                                   levels = c("a", "b", "c"), 
                                   labels = c("Cond_A", "Cond_B", "Cond_C"))

# Colors and labels corresponding to the ERPs
cond_labs <- c("Cond_A", "Cond_B", "Cond_C")
cond_vals <- c("black", "red", "blue")


# PLOT 1: Semantic Score Density

# Calculate means per item and per condition
sem_stats <- items_and_means(stimuli_unique, "Semantic_Score")

p_sem <- plot_density(data = sem_stats[[1]], 
                      data_means = sem_stats[[2]], 
                      ylab = "Density", 
                      xlab = "Semantic Score (Likert)", 
                      predictor = "Semantic_Score",
                      leg_labs = cond_labs, 
                      leg_vals = cond_vals, 
                      ylimits = c(0, 1.5),  # Adjust if the curve is cut off at the top
                      xbreaks = seq(1, 7, 1)) # Assumes a 1 to 7 scale

ggsave("./Density_Scores/Density_Semantic_Score.pdf", p_sem, width = 5, height = 4)


# PLOT 2: Info Score Density

info_stats <- items_and_means(stimuli_unique, "Info_Score")

p_info <- plot_density(data = info_stats[[1]], 
                       data_means = info_stats[[2]], 
                       ylab = "Density", 
                       xlab = "Information Score (Likert)", 
                       predictor = "Info_Score",
                       leg_labs = cond_labs, 
                       leg_vals = cond_vals, 
                       ylimits = c(0, 1.5), 
                       xbreaks = seq(1, 7, 1))

ggsave("./Density_Scores/Density_Info_Score.pdf", p_info, width = 5, height = 4)

cat("Density plots successfully generated!\n")