# ==========================================
# Exploratory Analysis – ENDIREH 2016 & 2021
# ==========================================
# Load Libraries

library(tidyverse)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)

library(tidyverse)

# 1) Cargar combinado (mejor .rds para no perder tipos)
data <- read_rds("../data/combined/data_combined.rds")

# 2) Asegurar binarios como 0/1 enteros
viol_vars <- c(
  "violencia_emocional_vivida",
  "violencia_emocional_normalizada",
  "violencia_emocional_reconocida",
  "violencia_fisica_vivida",
  "violencia_fisica_normalizada",
  "violencia_vivida",
  "violencia_normalizada",
  "violencia_reconocida"
)

data <- data %>%
  mutate(across(any_of(viol_vars), ~ as.integer(.)))  # deja 0/1 claros

# 3) Resumen anual (cuentas y porcentajes)
resumen <- data %>%
  group_by(year) %>%
  summarise(
    total_mujeres = n(),
    violencia_emocional_vivida = sum(violencia_emocional_vivida, na.rm = TRUE),
    violencia_fisica_vivida    = sum(violencia_fisica_vivida,    na.rm = TRUE),
    violencia_emocional_normalizada = sum(violencia_emocional_normalizada, na.rm = TRUE),
    violencia_fisica_normalizada    = sum(violencia_fisica_normalizada,    na.rm = TRUE),
    violencia_emocional_reconocida  = sum(violencia_emocional_reconocida,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_emocional_vivida       = 100 * violencia_emocional_vivida       / total_mujeres,
    pct_fisica_vivida          = 100 * violencia_fisica_vivida          / total_mujeres,
    pct_emocional_normalizada  = 100 * violencia_emocional_normalizada  / pmax(violencia_emocional_vivida, 1),
    pct_fisica_normalizada     = 100 * violencia_fisica_normalizada     / pmax(violencia_fisica_vivida, 1),
    pct_emocional_reconocida   = 100 * violencia_emocional_reconocida   / pmax(violencia_emocional_vivida, 1)
  ) %>%
  mutate(across(starts_with("pct_"), ~ round(., 2)))

print(resumen)

# 4) Guardar
dir.create("../results/combined/tables", recursive = TRUE, showWarnings = FALSE)
write_csv(resumen, "../results/combined/tables/resumen_general.csv")



# By emotional violence
#======================

library(tidyverse)

# data debe ser tu combinado con year y columnas P14_1_* y P14_2_*
# 1 = experiencia, 2 = percepción

# 1) Long por ítem (10–22) usando las P14 comunes, preservando ID_PER
dl <- data %>%
  select(ID_PER, year, matches("^P14_[12]_(1[0-9]|2[0-2])$")) %>%
  pivot_longer(
    cols = -c(ID_PER, year),
    names_to = c("serie","item"),
    names_pattern = "^P14_([12])_(\\d+)$",
    values_to = "val"
  ) %>%
  mutate(serie = if_else(serie == "1", "exp", "per")) %>%
  pivot_wider(
    id_cols   = c(ID_PER, year, item),  # <- clave única por persona-ítem-año
    names_from = serie,
    values_from = val
  )


# 2) Indicadores por persona-ítem
dl <- dl %>%
  mutate(
    vivio       = exp %in% 1:3,
    normalizo   = vivio & per == 3,
    reconocio   = vivio & per %in% c(1, 2)
  )

# 3) Resumen por año e ítem
by_item <- dl %>%
  group_by(year, item) %>%
  summarise(
    n_mujeres    = n(),
    n_vivio      = sum(vivio, na.rm = TRUE),
    n_normalizo  = sum(normalizo, na.rm = TRUE),
    n_reconocio  = sum(reconocio, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_normalizo = 100 * n_normalizo / pmax(n_vivio, 1),
    pct_reconocio = 100 * n_reconocio / pmax(n_vivio, 1)
  )

# 4) Etiquetas
labels_en <- c(
  "10" = "Shaming, insulting, or humiliating",
  "11" = "Emotional neglect or lack of affection",
  "12" = "Statements suggesting infidelity",
  "13" = "Inducing fear",
  "14" = "Threats",
  "15" = "Locking in or restricting movement",
  "16" = "Stalking or surveillance",
  "17" = "Controlling your actions",
  "18" = "Threatening with weapons or burning",
  "19" = "Threats of murder, suicide, or harm to children",
  "20" = "Destroying or hiding belongings",
  "21" = "Ignoring or silent treatment",
  "22" = "Monitoring phone or email"
)

by_item <- by_item %>%
  mutate(tipo_violencia = factor(item, levels = names(labels_en), labels = labels_en[names(labels_en)]))

# 5) Gráfico: número de mujeres que lo vivieron (por ítem y año)
plot_viv <- ggplot(by_item, aes(x = reorder(tipo_violencia, -n_vivio), y = n_vivio, fill = factor(year))) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("2016" = "#1f77b4", "2021" = "#ff7f0e"), name = "Year") +
  labs(title = "Emotional violence by type and year",
       x = "Type of emotional violence", y = "Women who experienced it") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 6) Gráfico: % que la normalizaron entre quienes la vivieron
plot_norm <- ggplot(by_item, aes(x = tipo_violencia, y = pct_normalizo, fill = factor(year))) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("2016" = "#1f77b4", "2021" = "#ff7f0e"), name = "Year") +
  labs(title = "Normalised emotional violence by type",
       x = "Type of emotional violence",
       y = "% of women who experienced it and marked 'not important'") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 7) Gráfico: % que la reconocieron entre quienes la vivieron
plot_reco <- ggplot(by_item, aes(x = tipo_violencia, y = pct_reconocio, fill = factor(year))) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("2016" = "#1f77b4", "2021" = "#ff7f0e"), name = "Year") +
  labs(title = "Recognised emotional violence by type",
       x = "Type of emotional violence",
       y = "% of women who experienced it and marked 1 or 2") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 8) Guardar
dir.create("../results/combined/plots", recursive = TRUE, showWarnings = FALSE)
ggsave("../results/combined/plots/emotional_by_item_experienced.png", plot_viv,  width = 11, height = 6, dpi = 300)
ggsave("../results/combined/plots/emotional_by_item_normalised.png", plot_norm, width = 11, height = 6, dpi = 300)
ggsave("../results/combined/plots/emotional_by_item_recognised.png", plot_reco,  width = 11, height = 6, dpi = 300)

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
    levels = c("<20", "20-29", "30-39", "40-49", "50-59", "60-79", ">80")
  ))

# Plot

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

library(tidyverse)

# Si tus binarios están como lógicos o enteros, esto funciona igual
resumen_edad_reco <- data %>%
  filter(
    !is.na(violencia_emocional_reconocida),
    violencia_emocional_vivida == 1  # si la tienes lógica, usa == TRUE
  ) %>%
  group_by(year, grupo_edad) %>%
  summarise(
    total_vivieron = n(),
    reconocieron   = sum(violencia_emocional_reconocida, na.rm = TRUE),
    porcentaje     = round(100 * reconocieron / pmax(total_vivieron, 1), 1),
    .groups = "drop"
  ) %>%
  mutate(grupo_edad = factor(
    grupo_edad,
    levels = c("<20", "20-29", "30-39", "40-49", "50-59", "60-79", ">80")
  ))

# Plot
age_plot_rec <- ggplot(resumen_edad_reco, aes(x = grupo_edad, y = porcentaje, color = as.factor(year), group = year)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Recognised Emotional Violence Among Women Who Experienced It",
    subtitle = "By age group – Comparison between 2016 and 2021",
    x = "Age group",
    y = "% who recognised experienced cases as serious",
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

# Save
dir.create("../results/combined/plots", recursive = TRUE, showWarnings = FALSE)
ggsave("../results/combined/plots/age_plot_recognised.png", plot = age_plot_rec, width = 8, height = 5, dpi = 300)

### Prove H2

library(tidyverse)
library(broom)
# opcional para SE cluster:
# library(fixest); library(modelsummary)

data_viv <- data %>%
  filter(violencia_emocional_vivida == TRUE,
         !is.na(violencia_emocional_normalizada)) %>%
  mutate(
    post = as.integer(year == 2021),
    grupo_edad = factor(grupo_edad, levels = c("<20","20-29","30-39","40-49","50-59","60-80",">80"))
  )
# Edad
h2_desc_edad <- data_viv %>%
  group_by(year, grupo_edad) %>%
  summarise(n = n(),
            norm = sum(violencia_emocional_normalizada, na.rm=TRUE),
            pct = 100*norm/n, .groups="drop")
write_csv(h2_desc_edad, "../results/combined/tables/H2_desc_edad.csv")

# Educación (usa tu codificación de GRA tal como está)
h2_desc_edu <- data_viv %>%
  group_by(year, GRA) %>%
  summarise(n = n(),
            norm = sum(violencia_emocional_normalizada, na.rm=TRUE),
            pct = 100*norm/n, .groups="drop")
write_csv(h2_desc_edu, "../results/combined/tables/H2_desc_edu.csv")

# Acceso digital (usa tu conteo tal cual; si quieres binario en la gráfica, se crea al vuelo)
h2_desc_dig <- data_viv %>%
  group_by(year, acceso_digital) %>%
  summarise(n = n(),
            norm = sum(violencia_emocional_normalizada, na.rm=TRUE),
            pct = 100*norm/n, .groups="drop")
write_csv(h2_desc_dig, "../results/combined/tables/H2_desc_digital.csv")

# Graphs
# Edad
p_h2_edad <- ggplot(h2_desc_edad, aes(x=grupo_edad, y=pct, color=factor(year), group=year)) +
  geom_line(linewidth=1) + geom_point(size=2) +
  labs(title="H2 – Normalization among victims", subtitle="By age group (2016 vs 2021)",
       x="Age group", y="% normalized", color="Year") +
  theme_minimal()
ggsave("../results/combined/plots/H2_desc_edad.png", p_h2_edad, width=8, height=5, dpi=300)

# Educación (si prefieres etiquetas, haz un recode antes; aquí va directo con tu GRA)
p_h2_edu <- ggplot(h2_desc_edu, aes(x=factor(GRA), y=pct, fill=factor(year))) +
  geom_col(position="dodge") +
  labs(title="H2 – Normalization among victims", subtitle="By education (2016 vs 2021)",
       x="Education (GRA)", y="% normalized", fill="Year") +
  theme_minimal()
ggsave("../results/combined/plots/H2_desc_edu.png", p_h2_edu, width=8, height=5, dpi=300)

# Acceso digital (conteo tal cual)
p_h2_dig <- ggplot(h2_desc_dig, aes(x=factor(acceso_digital), y=pct, fill=factor(year))) +
  geom_col(position="dodge") +
  labs(title="H2 – Normalization among victims", subtitle="By digital access (2016 vs 2021)",
       x="# digital devices (household)", y="% normalized", fill="Year") +
  theme_minimal()
ggsave("../results/combined/plots/H2_desc_digital.png", p_h2_dig, width=8, height=5, dpi=300)

# Models
data_viv_16 <- filter(data_viv, year == 2016)
data_viv_21 <- filter(data_viv, year == 2021)

m16 <- glm(violencia_emocional_normalizada ~ grupo_edad + GRA + acceso_digital,
           data = data_viv_16, family = binomial)
m21 <- glm(violencia_emocional_normalizada ~ grupo_edad + GRA + acceso_digital,
           data = data_viv_21, family = binomial)

broom::tidy(m16) %>% write_csv("../results/combined/tables/H2_model_2016.csv")
broom::tidy(m21) %>% write_csv("../results/combined/tables/H2_model_2021.csv")

# Model
m_int <- glm(
  violencia_emocional_normalizada ~ post +
    grupo_edad*post + GRA*post + acceso_digital*post,
  data = data_viv, family = binomial
)
broom::tidy(m_int) %>% write_csv("../results/combined/tables/H2_model_interacciones.csv")

exp(coef(m_int))

