library(tidyverse)

# Load cleaned datasets
data_2016 <- read_rds("../data/2016/cleaned/data_2016_cleaned.rds")
data_2021 <- read_rds("../data/2021/cleaned/data_2021_cleaned.rds")

#Filter CDMX
data_2016_cdmx <- data_2016 %>%
  filter(NOM_ENT == "CIUDAD DE MÉXICO") %>%
  mutate(year = 2016)

data_2021_cdmx <- data_2021 %>%
  filter(NOM_ENT == "CIUDAD DE MÉXICO") %>%
  mutate(year = 2021)

# Create NAS
data_2016_cdmx <- data_2016_cdmx %>%
  mutate(acceso_digital = NA_integer_)  # si no existe en 2016

# Combine two datasets
data_combined_cleaned_CDMX <- bind_rows(data_2016_cdmx, data_2021_cdmx)

# Save
write_rds(data_combined_cleaned_CDMX, "../data/combined/data_combined_cleaned_CDMX.rds")
write_csv(data_combined_cleaned_CDMX, "../data/combined/data_combined_cleaned_CDMX.csv")

