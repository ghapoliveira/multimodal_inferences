# behavioral_stats.R — regenerates every behavioral number cited in §sec-eegquestions
#
# Run from exp2_eeg/analysis/
#
# Input
#  ../data/behavioral/behavioral_data.xlsx   (merged per-trial E-Prime export)
#
# Output
#  statistics/behavioral/{accuracy_by_subject,accuracy_by_condition,accuracy_by_item}.csv
#
# See requirements_R.txt

library(readxl)
library(dplyr)
library(readr)

SHEET    <- "behavioral_data"
ITEM_COL <- "Sentence"
OUTDIR   <- "statistics/behavioral"

dat <- read_excel("../data/behavioral/behavioral_data.xlsx", sheet = SHEET)

q <- dat %>%
  filter(!is.na(`QuestionScreen.ACC`)) %>%
  mutate(
    Subject   = as.integer(Subject),
    Condition = tolower(trimws(Condition)),
    acc       = as.numeric(`QuestionScreen.ACC`)   # 1 = correct, 0 = wrong
  )

dir.create(OUTDIR, recursive = TRUE, showWarnings = FALSE)

# 1. Per subject  ->  "M = 94%, min = 85%, SD = 3%", none below 85%
acc_subj <- q %>%
  group_by(Subject) %>%
  summarise(n_questions = n(), accuracy = mean(acc), .groups = "drop") %>%
  arrange(accuracy)
write_csv(acc_subj, file.path(OUTDIR, "accuracy_by_subject.csv"))

cat(sprintf("Per-subject accuracy:  M = %.0f%%, min = %.0f%%, SD = %.0f%%  | below 85%%: %s\n",
            100*mean(acc_subj$accuracy), 100*min(acc_subj$accuracy), 100*sd(acc_subj$accuracy),
            { b <- acc_subj$Subject[acc_subj$accuracy < 0.85]; if (length(b)) paste(b, collapse=", ") else "none" }))

# 2. Per condition  ->  a 279/299, b 276/302, c 278/298, fillers 832/870
acc_cond <- q %>%
  group_by(Condition) %>%
  summarise(n_questions = n(), n_correct = sum(acc), n_wrong = sum(acc == 0),
            accuracy = mean(acc), accuracy_sd = 100*sd(acc), .groups = "drop")
write_csv(acc_cond, file.path(OUTDIR, "accuracy_by_condition.csv"))
cat("\nPer-condition:\n"); print(as.data.frame(acc_cond))

# 3. Per item 
acc_item <- q %>%
  group_by(.data[[ITEM_COL]], Emoji, Condition) %>%
  summarise(n = n(), n_correct = sum(acc), accuracy = mean(acc), .groups = "drop") %>%
  arrange(desc(accuracy))
write_csv(acc_item, file.path(OUTDIR, "accuracy_by_item.csv"))

# 4. Distribution summaries (mean/sd/min/max/median) at each level
dist_summary <- function(df, col)
  summarise(df,
            n      = dplyr::n(),
            mean   = mean(.data[[col]]), sd = sd(.data[[col]]),
            min    = min(.data[[col]]),  max = max(.data[[col]]),
            median = median(.data[[col]]))

write_csv(dist_summary(acc_subj, "accuracy"),
          file.path(OUTDIR, "summary_by_subject.csv"))

# 5. Conjoint-meaning questions — accuracy in condition B (the bridging probe)
questions <- read_excel("../materials/fillers_and_questions.xlsx", sheet = "questions")
conjoint_qs <- questions %>%
  filter(tolower(trimws(conjoint_meaning)) %in% c("yes","y","true","1")) %>%
  pull(questions_pt) %>% trimws() %>% unique()

acc_conjoint <- q %>%
  filter(Condition == "b", trimws(Questions) %in% conjoint_qs) %>%
  group_by(Emoji, Questions) %>%
  summarise(n = n(), n_correct = sum(acc), accuracy = mean(acc), .groups = "drop") %>%
  arrange(accuracy)
write_csv(acc_conjoint, file.path(OUTDIR, "accuracy_conjoint.csv"))

message("\nWrote behavioral tables to ", OUTDIR, "/")