library(tidyverse)

# Load cleaned datasets
data_2016 <- read_rds("../data/2016/cleaned/data_2016_cleaned.rds")
data_2021 <- read_rds("../data/2021/cleaned/data_2021_cleaned.rds")


data_combined_cleaned <- bind_rows(
  data_2016 %>% mutate(year = 2016),
  data_2021%>% mutate(year = 2021)
)


# Save
write_rds(data_combined_cleaned, "../data/combined/data_combined_cleaned.rds")
write_csv(data_combined_cleaned, "../data/combined/data_combined_cleaned.csv")

