# ==========================================
# DiD Analysis – ENDIREH 2016 & 2021
# ==========================================
## Difference in differences

# Load files and libraries
library(tidyverse)
library(ggplot2)
data_combined <- read_rds("../data/combined/data_combined_cleaned.rds")

# Clean dataset
data_combined <- data_combined %>%
  mutate(NOM_ENT = str_to_upper(NOM_ENT))  # pone todos los nombres en mayúsculas

# Make the same name for both ¨2016 and 2021"
data_combined <- data_combined %>%
  mutate(
    treated = ifelse(NOM_ENT %in% c("CIUDAD DE MEXICO", "CIUDAD DE MÉXICO"), 1, 0),
    post = ifelse(year == 2021, 1, 0)
  )

modelo_did <- lm(violencia_emocional_normalizada ~ treated * post,
                 data = data_combined %>% filter(!is.na(violencia_emocional_normalizada)))
# Print model
summary(modelo_did)


# Ggplot

# Agrupar y calcular promedio por grupo y año
grafica_did <- data_combined %>%
  filter(!is.na(violencia_emocional_normalizada)) %>%
  group_by(year, treated) %>%
  summarise(
    promedio = mean(violencia_emocional_normalizada),
    .groups = "drop"
  ) %>%
  mutate(
    grupo = ifelse(treated == 1, "Mexico City", "Other states")
  )

# Asegurar orden de año
grafica_did$year <- as.numeric(as.character(grafica_did$year))

# Create plot
ggplot(grafica_did, aes(x = year, y = promedio, color = grupo, group = grupo)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Mexico City" = "#0072B2", "Other states" = "#999999")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Differences in differences: Normalized emotional violence",
    subtitle = "Comparison between Mexico City and other Mexican states (2016 vs 2021)",
    x = "Year",
    y = "Share that normalized emotional violence",
    color = "Group"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "bottom"
  )

# Save plot
ggsave("../results/combined/plots/did_plot.png", width = 8, height = 5, dpi = 300)

# ========================
# Difference-in-Differences
# Solo mujeres que vivieron violencia emocional
# ========================

library(tidyverse)

# Load dataset
data_combined <- read_rds("../data/combined/data_combined_cleaned.rds")

# Homogeneizar nombres de estado
data_combined <- data_combined %>%
  mutate(NOM_ENT = str_to_upper(NOM_ENT))

# Crear variables DID
data_combined <- data_combined %>%
  mutate(
    treated = ifelse(NOM_ENT %in% c("CIUDAD DE MEXICO", "CIUDAD DE MÉXICO"), 1, 0),
    post = ifelse(year == 2021, 1, 0)
  )

# === Filtrar solo mujeres que vivieron violencia emocional ===
data_did_vivieron <- data_combined %>%
  filter(violencia_emocional_vivida == TRUE, !is.na(violencia_emocional_normalizada))

# === Modelo DID solo con quienes vivieron violencia emocional ===
modelo_did_vivieron <- lm(violencia_emocional_normalizada ~ treated * post, data = data_did_vivieron)
summary(modelo_did_vivieron)

# === Gráfica ===
grafica_did_vivieron <- data_did_vivieron %>%
  group_by(year, treated) %>%
  summarise(
    promedio = mean(violencia_emocional_normalizada),
    .groups = "drop"
  ) %>%
  mutate(
    grupo = ifelse(treated == 1, "Mexico City", "Other states"),
    year = as.numeric(as.character(year))
  )

ggplot(grafica_did_vivieron, aes(x = year, y = promedio, color = grupo, group = grupo)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Mexico City" = "#0072B2", "Other states" = "#999999")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Differences in differences: Normalized emotional violence",
    subtitle = "Among women who experienced it – CDMX vs other states (2016 vs 2021)",
    x = "Year",
    y = "Share that normalized emotional violence",
    color = "Group"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "bottom"
  )

# Save plot
ggsave("../results/combined/plots/did_plot_vivieron.png", width = 8, height = 5, dpi = 300)

### SECOND MODEL

# Asegúrate de que grupo_edad sea factor con orden correcto
data_combined <- data_combined %>%
  mutate(
    grupo_edad = factor(grupo_edad, levels = c("<20", "20-29", "30-39", "40-49", "50-59", "60-80", ">80"))
  )

# Modelo DID controlando por grupo de edad
modelo_did_edad <- lm(
  violencia_emocional_normalizada ~ treated * post + grupo_edad,
  data = data_combined %>% filter(!is.na(violencia_emocional_normalizada), !is.na(grupo_edad))
)

# Ver resultados
summary(modelo_did_edad)

### THIRD MODEL

modelo_did_edad_extendido <- lm(
  violencia_emocional_normalizada ~ treated * post + grupo_edad + GRA + acceso_digital,
  data = data_combined %>% 
    filter(!is.na(violencia_emocional_normalizada), !is.na(grupo_edad), !is.na(GRA), !is.na(acceso_digital))
)

summary(modelo_did_edad_extendido)

