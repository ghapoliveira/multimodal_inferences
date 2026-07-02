# question_summary.r
# Generates a plot that summarizes questions and answers per condition, per list.
#
# See requirements_R.txt

library(readxl)
library(dplyr)
library(readr)
library(tidyr)

questions <- read_excel(
  "fillers_and_questions.xlsx",
  sheet = "questions"
)

design_table <- questions %>%
  filter(cond %in% c("a", "b", "c", "filler")) %>%
  mutate(cond = toupper(cond)) %>%
  group_by(cond) %>%
  summarise(
    `List 1` = sprintf(
      "%d (%d No / %d Yes)",
      sum(!is.na(list_1)),
      sum(tolower(list_1) == "no", na.rm = TRUE),
      sum(tolower(list_1) == "yes", na.rm = TRUE)
    ),
    `List 2` = sprintf(
      "%d (%d No / %d Yes)",
      sum(!is.na(list_2)),
      sum(tolower(list_2) == "no", na.rm = TRUE),
      sum(tolower(list_2) == "yes", na.rm = TRUE)
    ),
    `List 3` = sprintf(
      "%d (%d No / %d Yes)",
      sum(!is.na(list_3)),
      sum(tolower(list_3) == "no", na.rm = TRUE),
      sum(tolower(list_3) == "yes", na.rm = TRUE)
    ),
    Total = n(),
    .groups = "drop"
  ) %>%
  rename(Condition = cond)

question_distribution <- bind_rows(
  design_table,
  tibble(
    Condition = "Fillers",
    `List 1` = "30",
    `List 2` = "30",
    `List 3` = "30",
    Total = 90
  ),
  tibble(
    Condition = "Condition questions",
    `List 1` = "31",
    `List 2` = "31",
    `List 3` = "31",
    Total = 93
  ),
  tibble(
    Condition = "Total questions",
    `List 1` = "61",
    `List 2` = "61",
    `List 3` = "61",
    Total = 183
  )
)

write_csv(
  question_distribution,
  "question_distribution.csv"
)