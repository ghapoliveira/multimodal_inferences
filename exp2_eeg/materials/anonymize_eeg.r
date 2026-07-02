# anonymize_eeg.R — Run locally. Do NOT publish the raw data.
#
# See requirements_R.txt

library(dplyr)
library(readr)
library(writexl)

participants_raw <- read_csv("participants.csv", col_types = cols(.default = "c"))

participants_clean <- participants_raw %>%
  mutate(
    Age    = as.numeric(`Age at the time of the experiment`),
    Gender = `Gender`,
    Hand   = `Dominant hand`
  )

# Age summary
demographics_eeg_summary <- participants_clean %>%
  summarise(N = n(), Age_M = mean(Age, na.rm = TRUE), Age_SD = sd(Age, na.rm = TRUE),
            Age_Min = min(Age, na.rm = TRUE), Age_Max = max(Age, na.rm = TRUE))

# Gender counts
gender_counts_eeg <- participants_clean %>%
  count(Gender, name = "n")

# Handedness counts
handedness_counts_eeg <- participants_clean %>%
  count(Hand, name = "n")

print(demographics_eeg_summary)
print(gender_counts_eeg)
print(handedness_counts_eeg)

write_xlsx(
  list(
    summary    = demographics_eeg_summary,
    gender     = gender_counts_eeg,
    handedness = handedness_counts_eeg
  ),
  "demographics_eeg.xlsx"
)

message("Done. Publish demographics_eeg.xlsx.")
message("Do NOT publish participants.csv (contains experiment dates, a quasi-identifier).")