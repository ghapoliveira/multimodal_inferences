# analysis_ph.R — Public analysis script (Post-hoc version)
# Norming study for sentence–emoji pairs (PCIBex)

# Data: responses_ph.csv and demographics_ph.xlsx (see data/ folder)
# These files were anonymized from the raw PCIBex output using anonymize_ph.R (in data/anonymization_scripts/)
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
library(ucminf)
library(ordinal)
library(emmeans)
library(lmerTest)

# 1. Load anonymized data

results_likert_ph <- read.csv("data/responses_ph.csv", encoding = "UTF-8")
demographics_ph   <- read_xlsx("data/demographics_ph.xlsx", sheet = "summary")
dir.create("results/analysis_ph", recursive = TRUE, showWarnings = FALSE)

message("Participants: ", n_distinct(results_likert_ph$Participant))
message("Total responses: ", nrow(results_likert_ph))

# 2. Separate training from main experiment responses

results_likert_w_training_ph <- results_likert_ph
results_likert_ph             <- results_likert_ph %>% filter(Label != "training")

# 3. Participant demographics (from pre-aggregated file)

print(demographics_ph)

participants_per_group_ph <- results_likert_ph %>%
  group_by(Group) %>%
  summarise(n = n_distinct(Participant), .groups = "drop") %>%
  mutate(Group = as.character(Group)) %>%
  bind_rows(summarise(., Group = "Total", n = sum(n)))

print(participants_per_group_ph)

write.csv(participants_per_group_ph, file = "results/analysis_ph/participants_per_group_ph.csv", row.names = FALSE)

# 4. Stimulus selection pipeline

# 4.1 Visual clarity filter: exclude items where >30% of ratings are ≤ 4
visual_stats_ph <- results_likert_ph %>%
  filter(Task == "likert1") %>%
  group_by(Id) %>%
  summarise(
    Total_Prop_Visual_Error = mean(answer_likert <= 4, na.rm = TRUE),
    Prop_Visual_Error_A     = mean(answer_likert[Cond == "a"] <= 4, na.rm = TRUE),
    Prop_Visual_Error_B     = mean(answer_likert[Cond == "b"] <= 4, na.rm = TRUE),
    Prop_Visual_Error_C     = mean(answer_likert[Cond == "c"] <= 4, na.rm = TRUE),
    Mean_Visual_A = mean(answer_likert[Cond == "a"], na.rm = TRUE),
    Mean_Visual_B = mean(answer_likert[Cond == "b"], na.rm = TRUE),
    Mean_Visual_C = mean(answer_likert[Cond == "c"], na.rm = TRUE),
    .groups = "drop"
  )

ids_visual_ok_ph <- visual_stats_ph %>%
  filter(Total_Prop_Visual_Error < 0.30) %>%
  pull(Id)

# 4.2 Compute per-item statistics and determine exclusion/inclusion (different parameters from the original)
full_diagnosis_ph <- results_likert_ph %>%
  group_by(Id) %>%
  summarise(
    Prop_Sens_A_OK     = mean(answer_likert[Task == "likert2" & Cond == "a"] > 3,          na.rm = TRUE),
    Prop_Sens_B_OK     = mean(answer_likert[Task == "likert2" & Cond == "b"] > 3,          na.rm = TRUE),
    Prop_Sens_AB_OK    = mean(answer_likert[Task == "likert2" & Cond %in% c("a","b")] > 3, na.rm = TRUE),
    Prop_Sens_C_Reject = mean(answer_likert[Task == "likert2" & Cond == "c"] < 3,          na.rm = TRUE),
    Prop_Info_B_OK     = mean(answer_likert[Task == "likert3" & Cond == "b"] > 4,          na.rm = TRUE),
    Prop_Info_A_Reject = mean(answer_likert[Task == "likert3" & Cond == "a"] < 4,          na.rm = TRUE),
    Mean_Sens_A        = mean(answer_likert[Task == "likert2" & Cond == "a"],               na.rm = TRUE),
    Mean_Sens_B        = mean(answer_likert[Task == "likert2" & Cond == "b"],               na.rm = TRUE),
    Mean_Sens_AB       = mean(answer_likert[Task == "likert2" & Cond %in% c("a","b")],      na.rm = TRUE),
    Mean_Sens_C        = mean(answer_likert[Task == "likert2" & Cond == "c"],               na.rm = TRUE),
    Mean_Info_A        = mean(answer_likert[Task == "likert3" & Cond == "a"],               na.rm = TRUE),
    Mean_Info_B        = mean(answer_likert[Task == "likert3" & Cond == "b"],               na.rm = TRUE),
    Mean_Info_C        = mean(answer_likert[Task == "likert3" & Cond == "c"],               na.rm = TRUE),
    Information_Contrast_B_A = Mean_Info_B - Mean_Info_A,
    Semantic_Contrast_AB_C   = Mean_Sens_AB - Mean_Sens_C,
    .groups = "drop"
  ) %>%
  left_join(visual_stats_ph, by = "Id") %>%
  mutate(
    Decision = case_when(
      !Id %in% ids_visual_ok_ph      ~ "Excluded: Visual",
      Prop_Sens_A_OK  < 0.60        ~ "Excluded: Low_A_Sens Consensus",
      Prop_Sens_B_OK  < 0.60        ~ "Excluded: Low_B_Sens Consensus",
      Prop_Sens_C_Reject < 0.60     ~ "Excluded: Nonsense Fail",
      Information_Contrast_B_A < 1.0    ~ "Excluded: Weak Information Contrast",
      TRUE                          ~ "Valid"
    )
  )

# 4.3 Rank valid items and assign tiers; tiebreak by Semantic_Contrast_AB_C
manual_ids <- c(6, 61, 2, 102, 19, 62, 149, 66) # Add the missing items (this table generates 112, and not 115, as explained in Chapter 3)
                                                # This is necessary because it is targets_selected_ph that will feed the rERP

corpus_ranked <- full_diagnosis_ph %>%
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
write.csv(corpus_ranked %>% rename(id = Id), "results/analysis_ph/corpus_ranked_ph.csv", row.names = FALSE)

# Create the final selected subset and save it separately
targets_selected <- corpus_ranked %>%
  filter(str_detect(Status_Final, "Selected")) %>%
  arrange(Rank_Valid)

write.csv(targets_selected %>% rename(id = Id), "results/analysis_ph/targets_selected_ph.csv", row.names = FALSE)

# 5. Threshold sensitivity analysis

item_stats_ph <- results_likert_ph %>%
  filter(Id %in% ids_visual_ok_ph) %>%
  group_by(Id) %>%
  summarise(
    Pct_Visual_OK     = mean(answer_likert[Task == "likert1"] > 4,                        na.rm = TRUE),
    Pct_Sens_A_OK     = mean(answer_likert[Task == "likert2" & Cond == "a"] > 3,           na.rm = TRUE),
    Pct_Sens_B_OK     = mean(answer_likert[Task == "likert2" & Cond == "b"] > 3,           na.rm = TRUE),
    Pct_Sens_AB_OK    = mean(answer_likert[Task == "likert2" & Cond %in% c("a","b")] > 3,  na.rm = TRUE),
    Pct_Sens_C_Reject = mean(answer_likert[Task == "likert2" & Cond == "c"] < 3,           na.rm = TRUE),
    Pct_Info_B_OK     = mean(answer_likert[Task == "likert3" & Cond == "b"] > 4,           na.rm = TRUE),
    Pct_Info_A_Reject = mean(answer_likert[Task == "likert3" & Cond == "a"] < 4,           na.rm = TRUE),
    Mean_Info_A       = mean(answer_likert[Task == "likert3" & Cond == "a"],               na.rm = TRUE),
    .groups = "drop"
  )

check_thresholds_ph <- function(data, thresholds) {
  bind_rows(lapply(thresholds, function(t) {
    data.frame(
      Threshold     = paste0(t * 100, "%"),
      Visual_OK     = sum(data$Pct_Visual_OK     >= t),
      Sens_A_OK     = sum(data$Pct_Sens_A_OK     >= t),
      Sens_B_OK     = sum(data$Pct_Sens_B_OK     >= t),
      Sens_AB_OK    = sum(data$Pct_Sens_AB_OK    >= t),
      Sens_C_Reject = sum(data$Pct_Sens_C_Reject >= t),
      Info_B_OK     = sum(data$Pct_Info_B_OK     >= t),
      Info_A_Reject = sum(data$Pct_Info_A_Reject >= t),
      TOTAL_VALID   = sum(
        data$Pct_Visual_OK     >= t &
          data$Pct_Sens_A_OK     >= t &
          data$Pct_Sens_B_OK     >= t &
          data$Pct_Sens_C_Reject >= t &
          data$Pct_Info_B_OK     >= t &
          data$Pct_Info_A_Reject >= t      )
    )
  }))
}

threshold_table_ph <- check_thresholds_ph(item_stats_ph, seq(0.50, 0.80, by = 0.05))
print(threshold_table_ph)

write.csv(threshold_table_ph, "results/analysis_ph/threshold_sensitivity_ph.csv", row.names = FALSE)

# 6. Training phase statistics

training_stats_ph <- results_likert_w_training_ph %>%
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

print(training_stats_ph)
write.csv(training_stats_ph, "results/analysis_ph/training_stats_ph.csv", row.names = FALSE)

# 7. Scale intercorrelations (Pearson)

scale_cors_ph <- results_likert_ph %>%
  select(Participant, Id, Cond, Task, answer_likert) %>%
  pivot_wider(names_from = Task, values_from = answer_likert) %>%
  select(likert1, likert2, likert3) %>%
  cor(use = "complete.obs", method = "pearson")

labs <- c("Visual clarity", "Coherence", "Information")
dimnames(scale_cors_ph) <- list(labs, labs)

print(round(scale_cors_ph, 3))
saveRDS(scale_cors_ph, "results/analysis_ph/scale_cors.rds")

# 8. Plots

# Plot 1: Response distributions by condition and scale
p1 <- ggplot(results_likert_ph, aes(x = answer_likert, fill = Cond)) +
  geom_bar(aes(y = after_stat(count / sum(count))), color = "black") +
  facet_wrap(~ Cond * Task) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Response distribution by condition and scale",
       x = "Likert (1–7)", y = "Proportion") +
  theme_minimal()

ggsave("results/analysis_ph/plot1_distributions_ph.png", plot = p1, width = 8, height = 6)

# Plot 2: Consensus proportions
p2 <- full_diagnosis_ph %>%
  pivot_longer(
    cols      = c(Prop_Sens_A_OK, Prop_Sens_B_OK, Prop_Sens_C_Reject,
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
    "Prop_Sens_A_OK"     = "A makes sense",
    "Prop_Sens_B_OK"     = "B makes sense",
    "Prop_Sens_C_Reject" = "C makes no sense",
    "Prop_Info_B_OK"     = "B adds\ninformation",
    "Prop_Info_A_Reject" = "A adds no\ninformation"
  )) +
  labs(title = "Consensus by criterion",
       y = "Consensus proportion", x = "") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 10, face = "bold"))

ggsave("results/analysis_ph/plot2_consensus_ph.png", plot = p2, width = 8, height = 6)

# Plot 3: Mean scores by condition
p3 <- full_diagnosis_ph %>%
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

ggsave("results/analysis_ph/plot3_means_ph.png", plot = p3, width = 8, height = 6)

# Statistical tests

# Use the full dataset
results_likert_models <- results_likert_ph

# 1. Create an ordered factor for CLMMs (Ordinal)
# Assuming a 1-7 scale based on your plot labels
results_likert_models$answer_likert_ord <- ordered(results_likert_models$answer_likert, 
                                                   levels = 1:7)

# 2. Create a numeric column for LMERs (Continuous)
results_likert_models$answer_likert_num <- as.numeric(as.character(results_likert_models$answer_likert))

# Quick look at the data
boxplot(answer_likert_num ~ Task + Cond, data = results_likert_models, 
        main="Likert Scores by Task and Condition (Full Ensemble)")


# 1: Ordinal Models (CLMM)

# Q. 1 (Likert 1 - Visual Error)
results_models_perg1 <- results_likert_models %>%
  filter(Task == "likert1") %>%
  droplevels()

mod_likert1 <- clmm(answer_likert_ord ~ Cond + (1|Id) + (1|Participant), data=results_models_perg1, Hess=TRUE)
mod_zero1   <- clmm(answer_likert_ord ~ 1 + (1|Id) + (1|Participant), data=results_models_perg1, Hess=TRUE)

anova(mod_zero1, mod_likert1)
summary(mod_likert1)
emmeans(mod_likert1, pairwise ~ Cond, adjust = "bonferroni")


# Q. 2 (Likert 2 - Coherence)
results_models_perg2 <- results_likert_models %>%
  filter(Task == "likert2") %>%
  droplevels()

mod_likert2 <- clmm(answer_likert_ord ~ Cond + (1|Id) + (1|Participant), data=results_models_perg2, Hess=TRUE)
mod_zero2   <- clmm(answer_likert_ord ~ 1 + (1|Id) + (1|Participant), data=results_models_perg2, Hess=TRUE)

anova(mod_zero2, mod_likert2)
summary(mod_likert2)
emmeans(mod_likert2, pairwise ~ Cond, adjust = "bonferroni")


# Q. 3 (Likert 3 - Information)
results_models_perg3 <- results_likert_models %>%
  filter(Task == "likert3") %>%
  droplevels()

mod_likert3 <- clmm(answer_likert_ord ~ Cond + (1|Id) + (1|Participant), data=results_models_perg3, Hess=TRUE)
mod_zero3   <- clmm(answer_likert_ord ~ 1 + (1|Id) + (1|Participant), data=results_models_perg3, Hess=TRUE)

anova(mod_zero3, mod_likert3)
summary(mod_likert3)
emmeans(mod_likert3, pairwise ~ Cond, adjust = "bonferroni")


# 2: Continuous Models (LMER)

# Q. 1 (Numeric)
mean_num_perg1 <- results_models_perg1 %>%
  group_by(Cond) %>%
  summarise(mean = mean(answer_likert_num, na.rm = TRUE))
print(mean_num_perg1)

mod1_lmer <- lmer(answer_likert_num ~ Cond + (1|Id) + (1|Participant), data = results_models_perg1)
summary(mod1_lmer)
anova(mod1_lmer) # Main effect of Cond
emmeans(mod1_lmer, pairwise ~ Cond, adjust = "bonferroni")


# Q. 2 (Numeric)
mean_num_perg2 <- results_models_perg2 %>%
  group_by(Cond) %>%
  summarise(mean = mean(answer_likert_num, na.rm = TRUE))
print(mean_num_perg2)

mod2_lmer <- lmer(answer_likert_num ~ Cond + (1|Id) + (1|Participant), data = results_models_perg2)
summary(mod2_lmer)
anova(mod2_lmer)
emmeans(mod2_lmer, pairwise ~ Cond, adjust = "bonferroni")


# Q. 3 (Numeric)
mean_num_perg3 <- results_models_perg3 %>%
  group_by(Cond) %>%
  summarise(mean = mean(answer_likert_num, na.rm = TRUE))
print(mean_num_perg3)

mod3_lmer <- lmer(answer_likert_num ~ Cond + (1|Id) + (1|Participant), data = results_models_perg3)
summary(mod3_lmer)
anova(mod3_lmer)
emmeans(mod3_lmer, pairwise ~ Cond, adjust = "bonferroni")

clmm_row <- function(m, m0, scale) {
  lr_stat <- as.numeric(2 * (logLik(m) - logLik(m0)))
  lr_df   <- attr(logLik(m), "df") - attr(logLik(m0), "df")
  lr_p    <- pchisq(lr_stat, lr_df, lower.tail = FALSE)
  as.data.frame(emmeans(m, pairwise ~ Cond, adjust = "bonferroni")$contrasts) |>
    transmute(Scale = scale, Contrast = contrast,
              Estimate = round(estimate, 2), SE = round(SE, 3),
              z = round(z.ratio, 2), p = format.pval(p.value, eps = .0001),
              `χ²(2)` = round(lr_stat, 1),
              `LRT p` = format.pval(lr_p, eps = .001))
}

lmer_row <- function(m, scale) {
  as.data.frame(emmeans(m, pairwise ~ Cond, adjust = "bonferroni")$contrasts) |>
    transmute(Scale = scale, Contrast = contrast,
              Estimate = round(estimate, 2), SE = round(SE, 3),
              z = round(z.ratio, 2), p = format.pval(p.value, eps = .0001))
}

clmm_table <- bind_rows(
  clmm_row(mod_likert1, mod_zero1, "Visual clarity"),
  clmm_row(mod_likert2, mod_zero2, "Coherence"),
  clmm_row(mod_likert3, mod_zero3, "Information")
)

lmer_table <- bind_rows(
  lmer_row(mod1_lmer, "Visual clarity"),
  lmer_row(mod2_lmer, "Coherence"),
  lmer_row(mod3_lmer, "Information")
)

saveRDS(clmm_table, "results/analysis_ph/clmm_table_ph.rds")
saveRDS(lmer_table, "results/analysis_ph/lmer_table_ph.rds")
