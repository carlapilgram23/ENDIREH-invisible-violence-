# ==========================================
# Exploratory Analysis – ENDIREH 2016 & 2021
# ==========================================
# Load Libraries

library(tidyverse)
library(dplyr)

# Load data
data <- read_csv("../data/combined/data_combined_cleaned.csv")

# By Age
#============

# Choose the variables
resumen_edad_filtrado <- data %>%
  filter(
    !is.na(violencia_emocional_normalizada),
    violencia_emocional_vivida == TRUE
  ) %>%
  group_by(year, grupo_edad) %>%
  summarise(
    total = n(),
    normalizaron = sum(violencia_emocional_normalizada, na.rm = TRUE),
    porcentaje = round(100 * normalizaron / total, 1),
    .groups = "drop"
  ) %>%
  mutate(grupo_edad = factor(
    grupo_edad,
    levels = c("<20", "20-29", "30-39", "40-49", "50-59", "60-80", ">80")
  ))

# Plot

library(ggplot2)

age_plot_ <- ggplot(resumen_edad_filtrado, aes(x = grupo_edad, y = porcentaje, color = as.factor(year), group = year)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Normalized Emotional Violence Among Women Who Experienced It",
    subtitle = "By age group – Comparison between 2016 and 2021",
    x = "Age group",
    y = "% who considered emotional violence unimportant",
    color = "Year"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10),
    axis.text = element_text(size = 9),
    axis.title = element_text(size = 10),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 9)
  )


# Save the plot
ggsave("../results/combined/plots/age_plot_.png", plot = age_plot_, width = 8, height = 5, dpi = 300)
