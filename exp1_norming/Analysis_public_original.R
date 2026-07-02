# Analysis_public.r — Public analysis script
# Norming study for sentence–emoji pairs (PCIBex)

# Data: responses.csv and demographics.xlsx (see data/ folder)
# These files were anonymized from the raw PCIBex output using anonymize_original.R (in data/anonymization_scripts/)
# Participants gave informed consent; data were collected
# in compliance with UFRJ ethics guidelines.
# 
# See requirements_R.txt.

library(dplyr)
library(stringr)
library(writexl)
library(ggplot2)
library(tidyr)
library(readxl)

# 1. Load anonymized data
#    responses.csv : one row per Likert response, PII removed
#    demographics  : aggregate summaries only (no row-level personal data)

results_likert <- read.csv("data/responses.csv", encoding = "UTF-8")
demographics   <- read_xlsx("data/demographics.xlsx", sheet = "summary")
dir.create("results/analysis_original", recursive = TRUE, showWarnings = FALSE)

# All participants in this file already passed inclusion criteria:
#   - Age 18–40
#   - Emoji familiarity above minimum threshold
#   - Valid e-mail.

message("Participants: ", n_distinct(results_likert$Participant))
message("Total responses: ", nrow(results_likert))

# 2. Separate training from main experiment responses

results_likert_w_training <- results_likert
results_likert             <- results_likert %>% filter(Label != "training")

# 3. Participant demographics (from pre-aggregated file)

print(demographics)

participants_per_group <- results_likert %>%
  group_by(Group) %>%
  summarise(n = n_distinct(Participant), .groups = "drop") %>%
  mutate(Group = as.character(Group)) %>%
  bind_rows(summarise(., Group = "Total", n = sum(n)))

print(participants_per_group)

write.csv(participants_per_group, file = "results/analysis_original/participants_per_group.csv", row.names = FALSE)

# 4. Stimulus selection pipeline

# 4.1 Visual clarity filter: exclude items where > 30% of ratings are ≤ 4
visual_stats <- results_likert %>%
  filter(Task == "likert1") %>%
  group_by(Id) %>%
  summarise(
    Total_Prop_Visual_Error = mean(answer_likert <= 4, na.rm = TRUE),
    Prop_Visual_Error_A     = mean(answer_likert[Cond == "a"] <= 4, na.rm = TRUE),
    Prop_Visual_Error_B     = mean(answer_likert[Cond == "b"] <= 4, na.rm = TRUE),
    Prop_Visual_Error_C     = mean(answer_likert[Cond == "c"] <= 4, na.rm = TRUE),
    .groups = "drop"
  )

ids_visual_ok <- visual_stats %>%
  filter(Total_Prop_Visual_Error < 0.30) %>%
  pull(Id)

# 4.2 Compute per-item statistics and assign exclusion/inclusion decision
full_diagnosis <- results_likert %>%
  group_by(Id) %>%
  summarise(
    Prop_Sens_AB_OK    = mean(answer_likert[Task == "likert2" & Cond %in% c("a","b")] > 4, na.rm = TRUE),
    Prop_Sens_C_Reject = mean(answer_likert[Task == "likert2" & Cond == "c"] < 3,          na.rm = TRUE),
    Prop_Info_B_OK     = mean(answer_likert[Task == "likert3" & Cond == "b"] > 4,          na.rm = TRUE),
    Prop_Info_A_Reject = mean(answer_likert[Task == "likert3" & Cond == "a"] < 4,          na.rm = TRUE),
    Mean_Sens_A        = mean(answer_likert[Task == "likert2" & Cond == "a"],               na.rm = TRUE),
    Mean_Sens_B        = mean(answer_likert[Task == "likert2" & Cond == "b"],               na.rm = TRUE),
    Mean_Sens_AB       = mean(answer_likert[Task == "likert2" & Cond %in% c("a","b")],     na.rm = TRUE),
    Mean_Sens_C        = mean(answer_likert[Task == "likert2" & Cond == "c"],               na.rm = TRUE),
    Mean_Info_A        = mean(answer_likert[Task == "likert3" & Cond == "a"],               na.rm = TRUE),
    Mean_Info_B        = mean(answer_likert[Task == "likert3" & Cond == "b"],               na.rm = TRUE),
    Mean_Info_C        = mean(answer_likert[Task == "likert3" & Cond == "c"],               na.rm = TRUE),
    Information_Contrast_B_A = Mean_Info_B - Mean_Info_A,
    Semantic_Contrast_AB_C    = Mean_Sens_AB - Mean_Sens_C,
    .groups = "drop"
  ) %>%
  left_join(visual_stats, by = "Id") %>%
  mutate(
    Decision = case_when(
      !Id %in% ids_visual_ok       ~ "Excluded: Visual",
      Prop_Sens_AB_OK  < 0.60       ~ "Excluded: Low_AB_Sens Consensus",
      Prop_Sens_C_Reject < 0.60    ~ "Excluded: Nonsense Fail",
      Information_Contrast_B_A < 1.0   ~ "Excluded: Weak Information Contrast",
      TRUE                         ~ "Valid"
    )
  )

# 4.3 Rank valid items and assign tiers
manual_ids <- c(6, 61, 2, 19, 66) # Five items added by hand to reach the 120 used (documented in Chapter 3)

corpus_ranked <- full_diagnosis %>%
  arrange(desc(Decision == "Valid"), desc(Information_Contrast_B_A)) %>%
  mutate(
    # Count only non-manual valid items sequentially
    is_auto_valid = (Decision == "Valid" & !(Id %in% manual_ids)),
    Rank_Auto     = ifelse(is_auto_valid, cumsum(is_auto_valid), NA),
    
    # Assign status
    Status_Final  = case_when(
      Id %in% manual_ids  ~ "Selected_Manual",
      Rank_Auto <= 40     ~ "Selected_Tier_1",
      Rank_Auto <= 80     ~ "Selected_Tier_2",
      Rank_Auto <= 120    ~ "Selected_Tier_3",
      Decision == "Valid" ~ "Reserve",
      TRUE                ~ Decision
    )
  ) %>%
  mutate(
    max_auto_rank = max(Rank_Auto[str_detect(Status_Final, "Selected_Tier")], na.rm = TRUE),
    
    Rank_Valid = case_when(
      Status_Final == "Selected_Manual" ~ max_auto_rank + match(Id, manual_ids),
      TRUE ~ Rank_Auto
    )
  ) %>%
  select(Id, Status_Final, Rank_Valid, everything(), -Decision, -is_auto_valid, -Rank_Auto, -max_auto_rank)

print(table(corpus_ranked$Status_Final))

# Save the full ordered corpus
write.csv(corpus_ranked %>% rename(id = Id), "results/analysis_original/corpus_ranked.csv", row.names = FALSE)

# Create the final selected subset and save it separately
targets_selected <- corpus_ranked %>%
  filter(str_detect(Status_Final, "Selected")) %>%
  arrange(Rank_Valid)

write.csv(targets_selected %>% rename(id = Id), "results/analysis_original/targets_selected.csv", row.names = FALSE)

# 5. Threshold sensitivity analysis
#    Tests how many items survive at different consensus cutoffs

item_stats <- results_likert %>%
  filter(Id %in% ids_visual_ok) %>%
  group_by(Id) %>%
  summarise(
    Pct_Visual_OK     = mean(answer_likert[Task == "likert1"] > 4,                         na.rm = TRUE),
    Pct_Sens_A_OK     = mean(answer_likert[Task == "likert2" & Cond == "a"] > 4,           na.rm = TRUE),
    Pct_Sens_B_OK     = mean(answer_likert[Task == "likert2" & Cond == "b"] > 4,           na.rm = TRUE),
    Pct_Sens_AB_OK    = mean(answer_likert[Task == "likert2" & Cond %in% c("a","b")] > 4,  na.rm = TRUE),
    Pct_Sens_C_Reject = mean(answer_likert[Task == "likert2" & Cond == "c"] < 3,           na.rm = TRUE),
    Pct_Info_B_OK     = mean(answer_likert[Task == "likert3" & Cond == "b"] > 4,           na.rm = TRUE),
    Pct_Info_A_Reject = mean(answer_likert[Task == "likert3" & Cond == "a"] < 4,           na.rm = TRUE),
    Mean_Info_A       = mean(answer_likert[Task == "likert3" & Cond == "a"],               na.rm = TRUE),
    .groups = "drop"
  )

check_thresholds <- function(data, thresholds) {
  bind_rows(lapply(thresholds, function(t) {
    data.frame(
      Threshold     = paste0(t * 100, "%"),
      Visual_OK     = sum(data$Pct_Visual_OK     >= t),
      Sens_AB_OK    = sum(data$Pct_Sens_AB_OK    >= t),
      Sens_C_Reject = sum(data$Pct_Sens_C_Reject >= t),
      Info_B_OK     = sum(data$Pct_Info_B_OK     >= t),
      Info_A_Reject = sum(data$Pct_Info_A_Reject >= t),
      TOTAL_VALID   = sum(
        data$Pct_Visual_OK     >= t &
          data$Pct_Sens_AB_OK    >= t &
          data$Pct_Sens_C_Reject >= t &
          data$Pct_Info_B_OK     >= t &
          data$Pct_Info_A_Reject >= t      )
    )
  }))
}

threshold_table <- check_thresholds(item_stats, seq(0.50, 0.80, by = 0.05))
print(threshold_table)

write.csv(threshold_table, "results/analysis_original/threshold_sensitivity.csv", row.names = FALSE)

# 6. Training phase statistics
#    Each participant has exactly 6 rows (2 items × 3 scales)

training_stats <- results_likert_w_training %>%
  filter(Label == "training") %>%
  arrange(Participant, EventTime) %>%
  group_by(Participant) %>%
  filter(n() == 6) %>%
  mutate(Sent = rep(c("Training 1 (Door)", "Training 2 (Wool)"), each = 3)) %>%
  ungroup() %>%
  group_by(Sent, Task) %>%
  summarise(Mean = mean(answer_likert, na.rm = TRUE),
            SD   = sd(answer_likert,   na.rm = TRUE),
            .groups = "drop")

print(training_stats)
write.csv(training_stats, "results/analysis_original/training_stats.csv", row.names = FALSE)

# 7. Scale intercorrelations (Pearson)
#    Checks that the three questions measure distinct constructs

scale_cors <- results_likert %>%
  select(Participant, Id, Cond, Task, answer_likert) %>%
  pivot_wider(names_from = Task, values_from = answer_likert) %>%
  select(likert1, likert2, likert3) %>%
  cor(use = "complete.obs", method = "pearson")

labs <- c("Visual clarity", "Coherence", "Information")
dimnames(scale_cors) <- list(labs, labs)

print(round(scale_cors, 3))
saveRDS(scale_cors, "results/analysis_original/scale_cors.rds")

# 8. Plots

# Plot 1: Response distributions by condition and scale
p1 <- ggplot(results_likert, aes(x = answer_likert, fill = Cond)) +
  geom_bar(aes(y = after_stat(count / sum(count))), color = "black") +
  facet_wrap(~ Cond * Task) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Response distribution by condition and scale",
       x = "Likert (1–7)", y = "Proportion") +
  theme_minimal()

ggsave("results/analysis_original/plot1_distributions.png", plot = p1, width = 8, height = 6)

# Plot 2: Consensus proportions
p2 <- full_diagnosis %>%
  pivot_longer(
    cols      = c(Prop_Sens_AB_OK, Prop_Sens_C_Reject,
                  Prop_Info_B_OK, Prop_Info_A_Reject),
    names_to  = "Criterion",
    values_to = "Proportion"
  ) %>%
  ggplot(aes(x = Criterion, y = Proportion, fill = Criterion)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.8) +
  geom_hline(yintercept = 0.70, color = "red", linetype = "dashed") +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
  scale_x_discrete(labels = c(
    "Prop_Sens_AB_OK"     = "A and B make sense",
    "Prop_Sens_C_Reject" = "C makes no sense",
    "Prop_Info_B_OK"     = "B adds\ninformation",
    "Prop_Info_A_Reject" = "A adds no\ninformation"
  )) +
  labs(title = "Consensus by criterion",
       y = "Consensus proportion", x = "") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 10, face = "bold"))

ggsave("results/analysis_original/plot2_consensus.png", plot = p2, width = 8, height = 6)

# Plot 3: Mean scores by condition
p3 <- full_diagnosis %>%
  pivot_longer(
    cols      = c(Mean_Info_A, Mean_Info_B, Mean_Sens_C, Mean_Sens_A, Mean_Sens_B),
    names_to  = "Condition",
    values_to = "Mean_Score"
  ) %>%
  mutate(Condition = factor(Condition, levels = c(
    "Mean_Info_A", "Mean_Info_B", "Mean_Sens_C", "Mean_Sens_A", "Mean_Sens_B"
  ))) %>%
  ggplot(aes(x = Condition, y = Mean_Score, fill = Condition)) +
  geom_boxplot(alpha = 0.8, outlier.shape = 1, outlier.alpha = 0.5) +
  scale_fill_manual(values = c(
    "Mean_Info_A" = "#E74C3C", "Mean_Info_B" = "#2ECC71",
    "Mean_Sens_C" = "#95A5A6", "Mean_Sens_A" = "#3498DB",
    "Mean_Sens_B" = "#6633FF"
  )) +
  scale_y_continuous(breaks = 1:7, limits = c(1, 7)) +
  scale_x_discrete(labels = c(
    "Mean_Info_A" = "Info A\n(Repetition)", "Mean_Info_B" = "Info B\n(Addition)",
    "Mean_Sens_C" = "Sense C\n(Absurd)",    "Mean_Sens_A" = "Sense A\n(Coherent)",
    "Mean_Sens_B" = "Sense B\n(Coherent)"
  )) +
  labs(title = "Mean scores by condition",
       subtitle = "Contrast between conditions",
       y = "Mean score (1–7)", x = "") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("results/analysis_original/plot3_means.png", plot = p3, width = 8, height = 6)