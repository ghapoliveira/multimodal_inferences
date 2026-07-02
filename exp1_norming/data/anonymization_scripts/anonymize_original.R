# anonymize.R — Run locally. Do NOT publish the raw data.
#
# This script takes the locally produced results_likert_w_training.csv and
# metadata.xlsx, strips all personally identifiable information (PII),
# and writes two anonymized files ready for public release on GitHub:
#    - responses.csv      (Likert responses, no PII)
#    - demographics.xlsx  (aggregate demographics only)

library(dplyr)
library(readxl)
library(writexl)
library(tidyr)
library(stringr)

# 1. Load locally produced files

results_likert <- read.csv("results_likert_w_training.csv", encoding = "UTF-8")
metadata       <- read_xlsx("metadata.xlsx")

# 2. Anonymize response data
#    Remove: Location (participant's city — not needed for analysis)
#    Keep:   Participant (integer ID), Age, School, FamEmoji,
#            and all experimental/response columns

responses_public <- results_likert %>%
  select(-Location)

write.csv(responses_public, "responses.csv", row.names = FALSE)

# 3. Aggregate demographics (no row-level data)

demographics_public <- metadata %>%
  filter(Participant %in% unique(responses_public$Participant)) %>%
  mutate(Age = as.numeric(Age)) %>%
  summarise(
    N          = n(),
    Age_M      = mean(Age,  na.rm = TRUE),
    Age_SD     = sd(Age,    na.rm = TRUE),
    Age_Min    = min(Age,   na.rm = TRUE),
    Age_Max    = max(Age,   na.rm = TRUE)
  )

# 3a. Gender (translated to English)

gender_lookup <- c(
  "Feminino"  = "Female",
  "Masculino" = "Male",
  "Outro"     = "Other"
)

gender_counts <- metadata %>%
  filter(Participant %in% unique(responses_public$Participant)) %>%
  mutate(Gender_en = recode(Gender, !!!gender_lookup)) %>%
  count(Gender_en, name = "n") %>%
  mutate(Percentage = round((n / sum(n)) * 100, 2)) %>%
  rename(Gender = Gender_en)

# 3b. Education (translated to English)

school_lookup <- c(
  "Ensino Fundamental Incompleto" = "Incomplete Lower Education",
  "Ensino Médio Completo"          = "Complete High School",
  "Ensino Superior Completo"       = "Complete Undergraduate Studies",
  "Ensino Superior Incompleto"     = "Incomplete Undergraduate Studies"
)

school_counts <- metadata %>%
  filter(Participant %in% unique(responses_public$Participant)) %>%
  mutate(School_en = recode(School, !!!school_lookup)) %>%
  count(School_en, name = "n") %>%
  rename(School = School_en)

# 3c. Emoji familiarity (translated to English)

famemoji_lookup <- c(
  "Uso e recebo diariamente."                                       = "I use and receive them daily.",
  "Uso e recebo algumas vezes na semana."                           = "I use and receive them a few times a week.",
  "Uso e recebo de vez em quando."                                  = "I use and receive them sometimes.",
  "Uso raramente ou nunca, mas recebo diariamente"                  = "I rarely or never use them, but I receive them daily.",
  "Uso raramente ou nunca, mas recebo algumas vezes na semana"      = "I rarely or never use them, but I receive them a few times a week.",
  "Uso raramente ou nunca, mas recebo de vez em quando"             = "I rarely or never use them, but I receive them sometimes.",
  "Nunca recebo ou uso."                                            = "I never receive or use them."
)

famemoji_counts <- metadata %>%
  filter(Participant %in% unique(responses_public$Participant)) %>%
  mutate(FamEmoji_decoded = gsub("%2C", ",", FamEmoji, fixed = TRUE),
         FamEmoji_en = recode(FamEmoji_decoded, !!!famemoji_lookup)) %>%
  count(FamEmoji_en, name = "n") %>%
  arrange(desc(n)) %>%
  rename(FamEmoji = FamEmoji_en)

# 3d. Exclusion summary

excluded_famemoji <- c(
  "Uso e recebo de vez em quando.",
  "Uso raramente ou nunca%2C mas recebo diariamente"
)

exclusion_summary <- metadata %>%
  mutate(
    Age_numeric   = as.numeric(Age),
    excl_age      = is.na(Age_numeric) | Age_numeric < 18 | Age_numeric > 40,
    excl_email    = !str_detect(replace_na(Email, ""), "@"),
    excl_famemoji = FamEmoji %in% excluded_famemoji,
    Excl_Reason   = case_when(
      excl_age      ~ "Age out of range",
      excl_email    ~ "Invalid email",
      excl_famemoji ~ "Insufficient emoji familiarity",
      TRUE          ~ "Valid"
    )
  ) %>%
  count(Excl_Reason, name = "n")

print(exclusion_summary)

# 3e. Location — normalized and split into State / City

normalize_text <- function(x) {
  x <- trimws(x)
  x <- gsub("%2C", ",", x, fixed = TRUE)   # decode stray URL-encoded commas
  x <- tolower(x)
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- gsub("\\s*-\\s*", " ", x)
  x <- gsub("/", " ", x)
  x <- trimws(gsub("\\s+", " ", x))
  x
}

location_lookup <- list(
  # Rio de Janeiro (city)
  "rio de janeiro"                       = c("RJ", "Rio de Janeiro"),
  "cidade do rio de janeiro"             = c("RJ", "Rio de Janeiro"),
  "rio de janeiro rj"                    = c("RJ", "Rio de Janeiro"),
  "rio de janeiro rio de janeiro"        = c("RJ", "Rio de Janeiro"),
  
  # Brasília (city proper)
  "brasilia"                             = c("DF", "Brasília"),
  "brasilia df"                          = c("DF", "Brasília"),
  "brasilia distrito federal"            = c("DF", "Brasília"),
  "distrito federal"                     = c("DF", "Brasília"),
  
  # DF administrative regions
  "brasilia df, santa maria"             = c("DF", "Santa Maria"),
  "santa maria df"                       = c("DF", "Santa Maria"),
  "gama"                                 = c("DF", "Gama"),
  "gama df"                              = c("DF", "Gama"),
  "ceilandia"                            = c("DF", "Ceilândia"),
  "cidade estrutural df"                 = c("DF", "Cidade Estrutural"),
  "samambaia df"                         = c("DF", "Samambaia"),
  "samamambaia df"                       = c("DF", "Samambaia"),
  "sobradinho df"                        = c("DF", "Sobradinho"),
  "sol nascente"                         = c("DF", "Sol Nascente"),
  "sol nascente df"                      = c("DF", "Sol Nascente"),
  "aguas claras df"                      = c("DF", "Águas Claras"),
  
  # Belo Horizonte (MG)
  "belo horizonte"                       = c("MG", "Belo Horizonte"),
  
  # São João del-Rei (MG)
  "sao joao del rei"                     = c("MG", "São João del-Rei"),
  "sao joao del-rei"                     = c("MG", "São João del-Rei"),
  "sao joao del rey"                     = c("MG", "São João del-Rei"),
  "sao joao dei rei mg"                  = c("MG", "São João del-Rei"),
  "sao joao del rei, mg"                 = c("MG", "São João del-Rei"),   
  "sao joao del rei, minas gerais"       = c("MG", "São João del-Rei"),   
  
  # Campinas (SP)
  "campinas"                             = c("SP", "Campinas"),
  "campinas sp"                          = c("SP", "Campinas"),
  
  # Sete Lagoas (MG)
  "sete lagoas"                          = c("MG", "Sete Lagoas"),
  
  # Duque de Caxias (RJ)
  "duque de caxias"                      = c("RJ", "Duque de Caxias"),
  
  # Nova Iguaçu (RJ)
  "nova iguacu"                          = c("RJ", "Nova Iguaçu"),
  
  # Porto Alegre (RS)
  "porto alegre"                         = c("RS", "Porto Alegre"),
  
  # São Gonçalo (RJ)
  "sao goncalo"                          = c("RJ", "São Gonçalo"),
  
  # São João de Meriti (RJ)
  "sao joao de meriti"                   = c("RJ", "São João de Meriti"),
  
  # Itaberaba (BA)
  "itaberaba"                            = c("BA", "Itaberaba"),
  "itaberaba bahia"                      = c("BA", "Itaberaba"),
  
  # Magé (RJ)
  "mage"                                 = c("RJ", "Magé"),
  "mage, rio de janeiro"                 = c("RJ", "Magé"),
  
  # Nilópolis (RJ)
  "nilopolis"                            = c("RJ", "Nilópolis"),
  
  # São Paulo (city)
  "sao paulo"                            = c("SP", "São Paulo"),
  
  # Single-occurrence, unambiguous
  "arapiraca"                            = c("AL", "Arapiraca"),
  "belford roxo"                         = c("RJ", "Belford Roxo"),
  "belford roxo rj"                      = c("RJ", "Belford Roxo"),
  "belem pa"                             = c("PA", "Belém"),
  "campo belo, mg"                       = c("MG", "Campo Belo"),
  "florianopolis"                        = c("SC", "Florianópolis"),
  "ibirataia"                            = c("BA", "Ibirataia"),
  "ipiau"                                = c("BA", "Ipiaú"),
  "ipiau ba"                             = c("BA", "Ipiaú"),
  "itabira"                              = c("MG", "Itabira"),
  "itaborai"                             = c("RJ", "Itaboraí"),
  "itapagipe"                            = c("MG", "Itapagipe"),
  "januaria mg"                          = c("MG", "Januária"),
  "lagoa dourada"                        = c("MG", "Lagoa Dourada"),
  "niteroi"                              = c("RJ", "Niterói"),
  "niteroi, rj"                          = c("RJ", "Niterói"),
  "paragominas"                          = c("PA", "Paragominas"),
  "patos de minas"                       = c("MG", "Patos de Minas"),
  "paulinia"                             = c("SP", "Paulínia"),
  "santos"                               = c("SP", "Santos"),
  "sao carlos"                           = c("SP", "São Carlos"),
  "valparaiso"                           = c("GO", "Valparaíso de Goiás"),
  "varginha"                             = c("MG", "Varginha")
)

split_location <- function(raw) {
  norm <- normalize_text(raw)
  hit  <- location_lookup[[norm]]
  if (is.null(hit)) c(NA_character_, raw) else hit
}

location_split <- metadata %>%
  filter(Participant %in% unique(responses_public$Participant)) %>%
  rowwise() %>%
  mutate(
    .split = list(split_location(Location)),
    State  = .split[[1]],
    City   = .split[[2]]
  ) %>%
  ungroup() %>%
  select(-.split)

location_counts <- location_split %>%
  count(State, City, name = "n") %>%
  arrange(desc(n))

unmapped <- location_counts %>% filter(is.na(State))
if (nrow(unmapped) > 0) {
  message("\n Unmapped locations (add to location_lookup):")
  print(unmapped)
}

# 3f. Proportion from Rio de Janeiro or Brasília (city proper)

rj_bsb_pct <- round(
  100 * sum(location_counts$n[location_counts$City %in% c("Rio de Janeiro", "Brasília")]) /
    sum(location_counts$n),
  2
)

message("\n Percent of participants from Rio de Janeiro or Brasília (city proper): ", rj_bsb_pct, "%")

# 4. Write anonymized workbook

write_xlsx(
  list(
    summary  = demographics_public,
    gender   = gender_counts,
    school   = school_counts,
    famemoji = famemoji_counts,
    location = location_counts,
    exclusion = exclusion_summary
  ),
  "demographics.xlsx"
)

message("Done. Publish responses.csv and demographics.xlsx.")
message("Do NOT publish: metadata.xlsx, results_clean.csv, or the raw PCIBex file.")