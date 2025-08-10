# ==========================================
# Exploratory Analysis – ENDIREH 2016 & 2021
# ==========================================
# Load Libraries

library(tidyverse)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)

# Load data
data <- read_csv("../data/combined/data_combined_cleaned.csv")

# Physical violence
#============

# Make sure the columns are ok
data <- data %>%
  mutate(
    violencia_emocional_vivida = as.logical(violencia_emocional_vivida),
    violencia_fisica_vivida = as.logical(violencia_fisica_vivida),
    violencia_emocional_normalizada = as.logical(violencia_emocional_normalizada),
    violencia_fisica_normalizada = as.logical(violencia_fisica_normalizada)
  )

# Yearly resume
# Resume con porcentajes
resumen <- data %>%
  group_by(year) %>%
  summarise(
    total_mujeres = n(),
    violencia_emocional_vivida = sum(violencia_emocional_vivida, na.rm = TRUE),
    violencia_fisica_vivida = sum(violencia_fisica_vivida, na.rm = TRUE),
    violencia_emocional_normalizada = sum(violencia_emocional_normalizada, na.rm = TRUE),
    violencia_fisica_normalizada = sum(violencia_fisica_normalizada, na.rm = TRUE)
  ) %>%
  mutate(
    pct_emocional_vivida = 100 * violencia_emocional_vivida / total_mujeres,
    pct_fisica_vivida = 100 * violencia_fisica_vivida / total_mujeres,
    pct_emocional_normalizada = 100 * violencia_emocional_normalizada / violencia_emocional_vivida,
    pct_fisica_normalizada = 100 * violencia_fisica_normalizada / violencia_fisica_vivida
  )

# Mostrar
print(resumen)

# Guardar como CSV
write_csv(resumen, "../results/combined/tables/resumen_general.csv")


# By emotional violence
#======================


# === 1. Definir columnas por año ===
emocional_2016 <- paste0("P13_1_", 10:22)
emocional_2021 <- paste0("P14_1_", 10:22)

# === 2. Etiquetas comunes en inglés ===
labels_en <- c(
  "10" = "Shaming, insulting, 
  or humiliating",
  "11" = "Emotional neglect or lack of affection",
  "12" = "Statements suggesting infidelity",
  "13" = "Inducing fear",
  "14" = "Threats",
  "15" = "Locking in or restricting movement",
  "16" = "Stalking or surveillance",
  "17" = "Controlling your actions",
  "18" = "Threatening with weapons or burning",
  "19" = "Threats of murder, suicide, 
  or harm to children",
  "20" = "Destroying or hiding belongings",
  "21" = "Ignoring or silent treatment",
  "22" = "Monitoring phone or email"
)

# === 3. Contar para 2016 ===
conteo_2016 <- data %>%
  filter(year == 2016) %>%
  select(all_of(emocional_2016)) %>%
  mutate(across(everything(), ~ . %in% 1:3)) %>%
  summarise(across(everything(), ~ sum(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "var", values_to = "casos") %>%
  mutate(
    year = 2016,
    tipo = str_extract(var, "\\d+$"),
    tipo_violencia = labels_en[tipo]
  )

# === 4. Contar para 2021 ===
conteo_2021 <- data %>%
  filter(year == 2021) %>%
  select(all_of(emocional_2021)) %>%
  mutate(across(everything(), ~ . %in% 1:3)) %>%
  summarise(across(everything(), ~ sum(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "var", values_to = "casos") %>%
  mutate(
    year = 2021,
    tipo = str_extract(var, "\\d+$"),
    tipo_violencia = labels_en[tipo]
  )

# === 5. Unir ambos ===
conteo_total <- bind_rows(conteo_2016, conteo_2021)

# === 6. Graficar ===
specific_total <- ggplot(conteo_total, aes(x = reorder(tipo_violencia, -casos), y = casos, fill = factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("2016" = "#1f77b4", "2021" = "#ff7f0e"),
                    name = "Year") +
  labs(
    title = "Emotional Violence by Type and Year",
    x = "Type of Emotional Violence",
    y = "Number of Women Who Experienced It"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Only for women who experienced violence
# Take the names for each year
for (i in 10:22) {
  exp_2016 <- paste0("P13_1_", i)
  per_2016 <- paste0("P13_2_", i)
  exp_2021 <- paste0("P14_1_", i)
  per_2021 <- paste0("P14_2_", i)
  
  # Lo vivió
  data <- data %>%
    mutate(!!paste0("viv_", i) := case_when(
      year == 2016 ~ get(exp_2016) %in% 1:3,
      year == 2021 ~ get(exp_2021) %in% 1:3,
      TRUE ~ FALSE
    ))
  
  # Lo normalizó (vivió y dijo que fue sin importancia)
  data <- data %>%
    mutate(!!paste0("norm_", i) := case_when(
      year == 2016 & get(exp_2016) %in% 1:3 & get(per_2016) == 3 ~ TRUE,
      year == 2021 & get(exp_2021) %in% 1:3 & get(per_2021) == 3 ~ TRUE,
      TRUE ~ FALSE
    ))
}

# 2. Contar vivencias y normalizaciones
result <- data %>%
  select(year, starts_with("viv_"), starts_with("norm_")) %>%
  group_by(year) %>%
  summarise(across(starts_with("viv_"), ~ sum(.x, na.rm = TRUE), .names = "viv_{.col}"),
            across(starts_with("norm_"), ~ sum(.x, na.rm = TRUE), .names = "norm_{.col}"))

# 3. Reorganizar en formato largo y calcular % de normalización
viv_cols <- grep("^viv_viv_", names(result), value = TRUE)
norm_cols <- grep("^norm_norm_", names(result), value = TRUE)

df_viv <- result %>%
  select(year, all_of(viv_cols)) %>%
  pivot_longer(cols = -year, names_to = "tipo", values_to = "vividas") %>%
  mutate(tipo = gsub("viv_viv_", "", tipo))

df_norm <- result %>%
  select(year, all_of(norm_cols)) %>%
  pivot_longer(cols = -year, names_to = "tipo", values_to = "normalizadas") %>%
  mutate(tipo = gsub("norm_norm_", "", tipo))

df_porcentajes <- left_join(df_viv, df_norm, by = c("year", "tipo")) %>%
  mutate(porcentaje = 100 * normalizadas / vividas)

df_porcentajes$tipo_violencia <- factor(
  df_porcentajes$tipo,
  levels = names(labels_en),
  labels = labels_en[names(labels_en)]
)

# 5. Graficar
specific_plot <- ggplot(df_porcentajes, aes(x = tipo_violencia, y = porcentaje, fill = factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("2016" = "#1f77b4", "2021" = "#ff7f0e"),
                    name = "Year") +
  labs(
    title = "FIGURE 1. Normalized Emotional Violence: % of Women Who Did Not Consider It Serious",
    x = "Type of Emotional Violence",
    y = "% of Women Who Experienced It and Said It Was 'Not Important'"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Save the plot
ggsave("../results/combined/plots/specific_plot.png", plot = specific_plot, width = 11, height = 6, dpi = 300)

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
    levels = c("<20", "20-29", "30-39", "40-49", "50-59", " ", ">80")
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

