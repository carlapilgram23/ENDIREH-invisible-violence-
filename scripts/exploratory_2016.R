# ===================================
# Exploratory Analysis – ENDIREH 2016
# ===================================

library(tidyverse)
library(readr)
library(ggpubr)

# ==== 1. Load cleaned dataset ====

data_2016 <- read_rds("../data/2016/cleaned/data_2016_cleaned.rds")

# ==== 2. Overall proportions ====

summary_table <- tibble(
  type = c("Emotional", "Physical"),
  experienced = c(
    sum(data_2016$violencia_emocional_vivida, na.rm = TRUE),
    sum(data_2016$violencia_fisica_vivida, na.rm = TRUE)
  ),
  normalized = c(
    sum(data_2016$violencia_emocional_normalizada & data_2016$violencia_emocional_vivida, na.rm = TRUE),
    sum(data_2016$violencia_fisica_normalizada & data_2016$violencia_fisica_vivida, na.rm = TRUE)
  )
) %>%
  mutate(share_normalized = round(100 * normalized / experienced, 1))

print(summary_table)

# ==== 3. Group by age ====

by_age <- data_2016 %>%
  group_by(grupo_edad) %>%
  summarise(
    total = n(),
    emotional = sum(violencia_emocional_vivida, na.rm = TRUE),
    normalized = sum(violencia_emocional_normalizada & violencia_emocional_vivida, na.rm = TRUE)
  ) %>%
  mutate(percent_normalized = round(100 * normalized / emotional, 1))

print(by_age)

# ==== 4 Group by education level (GRA) ====

by_education <- data_2016 %>%
  filter(!is.na(GRA)) %>%
  group_by(GRA) %>%
  summarise(
    total = n(),
    emotional = sum(violencia_emocional_vivida, na.rm = TRUE),
    normalized = sum(violencia_emocional_normalizada & violencia_emocional_vivida, na.rm = TRUE)
  ) %>%
  mutate(percent_normalized = round(100 * normalized / emotional, 1))

print(by_education)

# ==== 5 Group by digital access index (acceso_digital) ====

by_digital_access <- data_2016 %>%
  filter(!is.na(acceso_digital)) %>%
  group_by(acceso_digital) %>%
  summarise(
    total = n(),
    emotional = sum(violencia_emocional_vivida, na.rm = TRUE),
    normalized = sum(violencia_emocional_normalizada & violencia_emocional_vivida, na.rm = TRUE)
  ) %>%
  mutate(percent_normalized = round(100 * normalized / emotional, 1)) %>%
  arrange(acceso_digital)

print(by_digital_access)


# ==== 6 Group by state ====

by_state <- data_2016 %>%
  filter(violencia_emocional_vivida == TRUE & !is.na(NOM_ENT)) %>%
  group_by(NOM_ENT) %>%
  summarise(
    total = n(),
    normalized = sum(violencia_emocional_normalizada, na.rm = TRUE),
    percent_normalized = round(100 * normalized / total, 1)
  ) %>%
  arrange(desc(percent_normalized))

print(head(by_state, 10))

# ==== 7. Save tables ====

if (!dir.exists("../results/2016/tables")) {
  dir.create("../results/2016/tables", recursive = TRUE)
}

write_csv(summary_table, "../results/2016/tables/summary_emotional_physical.csv")
write_csv(by_age, "../results/2016/tables/emotional_by_age.csv")
write_csv(by_state, "../results/2016/tables/emotional_by_state.csv")
write_csv(by_education, "../results/2016/tables/emotional_by_education.csv")
write_csv(by_digital_access, "../results/2016/tables/emotional_by_digital_access.csv")


# ==== 8. Plot by age group ====
by_age <- by_age %>%
  mutate(grupo_edad = factor(grupo_edad, levels = c("<20", "20-29", "30-39", "40-49", "50-59", "60-80", ">80")))

age_plot <- ggplot(by_age, aes(x = grupo_edad, y = percent_normalized)) +
  geom_line(group = 1, color = "#B22222", size = 1.2) +
  geom_point(size = 2, color = "#B22222") +
  labs(
    title = "Normalized emotional violence by age group",
    subtitle = "Women who experienced emotional violence – ENDIREH 2016",
    x = "Age group",
    y = "% who considered it unimportant"
  ) +
  theme_minimal()

# ==== 9. Plot by state ====

state_plot <- ggplot(by_state, aes(x = reorder(NOM_ENT, percent_normalized), y = percent_normalized)) +
  geom_col(fill = "#984ea3") +
  coord_flip() +
  labs(
    title = "Normalized emotional violence by state",
    subtitle = "Among women who experienced emotional violence – ENDIREH 2016",
    x = "State",
    y = "% who considered it unimportant"
  ) +
  theme_minimal()

# ==== 10. Plot by education ====
edu_plot <- ggplot(by_education, aes(x = factor(GRA), y = percent_normalized)) +
  geom_col(fill = "#2c7fb8") +
  labs(title = "Normalized emotional violence by education level (GRA)",
       x = "Education level (GRA)",
       y = "% who considered it unimportant") +
  theme_minimal()



# ==== 11. Plot by digital access ====
digital_plot <- ggplot(by_digital_access, aes(x = factor(acceso_digital), y = percent_normalized)) +
  geom_col(fill = "#7fcdbb") +
  labs(title = "Normalized emotional violence by digital access index",
       x = "Digital access index",
       y = "% who considered it unimportant") +
  theme_minimal()




# ==== 12. Save plots ====

if (!dir.exists("../results/2016/plots")) {
  dir.create("../results/2016/plots", recursive = TRUE)
}

ggsave("../results/2016/plots/emotional_by_age.png", age_plot, width = 7, height = 5, dpi = 300)
ggsave("../results/2016/plots/emotional_by_state.png", state_plot, width = 8, height = 7, dpi = 300)
ggsave("../results/2016/plots/emotional_by_education.png", edu_plot, width = 7, height = 5, dpi = 300)
ggsave("../results/2016/plots/emotional_by_digital_access.png", digital_plot, width = 7, height = 5, dpi = 300)

cat("✅ Exploratory analysis complete. Tables and plots saved in results/2016/\n")
