# 01_corpus_tables.R
#
# Generates the descriptive count tables for the sentence-emoji corpus used in
# the norming study (Experiment 1). Each table summarizes one annotated property
# of the corpus across all 150 triads (450 sentence-emoji pairs).
#
# These tables describe the full corpus (before stimulus selection). The
# equivalent tables restricted to the 120 selected targets are produced by
# analysis_ph.R, together with the selection itself.
#
# Input
#   materials/corpus.xlsx   (sheet "semantics")
#   results/analysis_original/targets_selected.csv
#
# Output  (written to results/tables/)
#   reference_form_counts.csv      reference form in Condition A (iconicity vs. association)
#   semantic_relation_counts.csv   discourse relation in Condition B
#   syntactic_role_counts.csv      syntactic role of the referred element
#   word_count_distribution.csv    number of sentences by word count
#   char_count_distribution.csv    number of sentences by character count
#   sentence_length_summary.csv    sentence length (words & characters): min, max, mean, SD, median
#
#   All show before and after selection.
# Usage
#   From the exp1_norming/ directory:
#     Rscript 01_corpus_tables.R
#
#   See requirements_R.txt.

library(readxl)
library(dplyr)
library(readr)

stimuli <- read_excel("materials/corpus.xlsx", sheet = "semantics")
dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)

# If the analysis has produced the selection, add an "after selection" column.
sel_path <- "results/analysis_original/targets_selected.csv"
selected_ids <- if (file.exists(sel_path)) {
  read_csv(sel_path, show_col_types = FALSE) |>
    filter(grepl("Selected", Status_Final)) |>
    pull(id) |> unique()
} else {
  message("No selection at ", sel_path, " - full-corpus columns only.")
  NULL
}

# Count the non-missing values of one column and write them to a CSV.
count_to_csv <- function(df, col, file, label = col, sort_alpha = FALSE) {
  out <- df |> filter(!is.na(.data[[col]])) |> count(.data[[col]], name = "n_full")
  if (!is.null(selected_ids)) {
    sel <- df |> filter(id %in% selected_ids, !is.na(.data[[col]])) |>
                 count(.data[[col]], name = "n_selected")
    out <- out |> left_join(sel, by = col) |> mutate(n_selected = coalesce(n_selected, 0L))
  }
  out <- if (sort_alpha) arrange(out, .data[[col]]) else arrange(out, desc(n_full))

  rename_map <- c(Total = "n_full", Selected = "n_selected")
  rename_map <- rename_map[rename_map %in% names(out)]
  out <- out |> rename(!!!rename_map)
  out <- out |> rename(!!label := all_of(col))

  write_csv(out, file.path("results/tables", file))
  out
}

# Annotation count tables

# Reference form (Condition A: iconicity vs. semantic association)
count_to_csv(stimuli, "reference_form",     "reference_form_counts.csv", label = "Reference Form")

# Discourse relation (Condition B)
count_to_csv(stimuli, "discourse_relation", "semantic_relation_counts.csv", label = "Discourse Relation")

# Syntactic role of the referred element (sorted alphabetically)
count_to_csv(stimuli, "referred_role",      "syntactic_role_counts.csv", sort_alpha = TRUE, label = "Referred Role")

# Sentence-length tables (before and after selection)

# Distributions: how many sentences have each word / character count
count_to_csv(stimuli, "n_words", "word_count_distribution.csv", label = "Word Count", sort_alpha = TRUE)
count_to_csv(stimuli, "n_chars", "char_count_distribution.csv", label = "Character Count", sort_alpha = TRUE)

# Summary: min, max, mean, SD, median for words and characters
len_summary <- function(df, set_label) data.frame(
  set     = set_label,
  measure = c("words", "characters"),
  min     = c(min(df$n_words),    min(df$n_chars)),
  max     = c(max(df$n_words),    max(df$n_chars)),
  mean    = round(c(mean(df$n_words), mean(df$n_chars)), 2),
  sd      = round(c(sd(df$n_words),   sd(df$n_chars)),   2),
  median  = c(median(df$n_words), median(df$n_chars))
)

length_summary <- len_summary(stimuli, "full")
if (!is.null(selected_ids)) {
  length_summary <- rbind(
    length_summary,
    len_summary(filter(stimuli, id %in% selected_ids), "selected")
  )
}
write_csv(length_summary, "results/tables/sentence_length_summary.csv")