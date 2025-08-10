
library(tidyverse)
# Load data
data <- read_csv("../data/combined/data_combined_cleaned.csv")

library(tidyverse)

# 1) Ver problemas de parseo
problems(data) %>% 
  as_tibble() %>% 
  count(col, sort = TRUE) %>% 
  print(n = 50)   # te dice qué columnas tuvieron errores


# 1) Saber el nombre de la columna 45
names(data)[45]

# 2) Ver ejemplos de los valores con problemas
unique(data[[45]])

# 3) Filtrar solo las filas con problemas de parseo
problems(data) %>%
  as_tibble() %>%
  select(row, col, expected, actual) %>%
  arrange(row) %>%
  left_join(
    tibble(row = rownames(data) %>% as.integer(), 
           value = data[[45]]),
    by = "row"
  )


# Revisa rangos razonables
summary(data$acceso_digital)
table(data$GRA_bin, useNA = "ifany")
table(data$grupo_edad, useNA = "ifany")

# 4) Congela una muestra única y consistente para el análisis
analysis_df <- data %>%
  mutate(
    post = if_else(year == 2021L, 1L, 0L),
    reconocimiento = if_else(violencia_emocional_normalizada, 0L, 1L) # 1= reconoce, 0= normaliza
  ) %>%
  filter(
    violencia_emocional_vivida == TRUE,
    !is.na(violencia_emocional_normalizada),
    !is.na(grupo_edad), !is.na(GRA_bin), !is.na(acceso_digital),
    !is.na(year), !is.na(post)
  ) %>%
  mutate(grupo_edad = fct_relevel(grupo_edad, "<20")) %>%
  select(year, post, reconocimiento, grupo_edad, GRA_bin, acceso_digital)

dim(analysis_df)
table(analysis_df$year)
table(analysis_df$grupo_edad, analysis_df$post)

# 5) (Opcional fuerte) Evita problemas de CSV: guarda/lee como RDS para que no cambien tipos
write_rds(analysis_df, "../results/analysis_freeze.rds")


# Modelo para 2016
m16_cont <- glm(
  violencia_emocional_normalizada ~ grupo_edad + GRA + acceso_digital,
  data = data_viv_16,
  family = binomial
)

# Modelo para 2021
m21_cont <- glm(
  violencia_emocional_normalizada ~ grupo_edad + GRA + acceso_digital,
  data = data_viv_21,
  family = binomial
)

# Modelo conjunto con interacción (para ver cambio 2016-2021)
m_int_cont <- glm(
  violencia_emocional_normalizada ~ post + grupo_edad * post + GRA * post + acceso_digital * post,
  data = data_viv,
  family = binomial
)

# Resultados en Odds Ratios
exp(coef(m16_cont))
exp(coef(m21_cont))
exp(coef(m_int_cont))

# Resumen con p-valores y estrellas de significancia
summary(m16_cont)
summary(m21_cont)
summary(m_int_cont)

# Para ver cuántas hay por grupo de edad y año
table(data_viv$grupo_edad, data_viv$post)

# Si quieres solo las >80
table(data_viv$grupo_edad == ">80", data_viv$post)

# O con proporciones
prop.table(table(data_viv$grupo_edad, data_viv$post), margin = 2)

## WITH BINARY EDUCATION
data <- data %>%
  mutate(
    post = if_else(year == 2021, 1, 0)
  )



# Modelo 2016
m16_bin <- glm(
  violencia_emocional_normalizada ~ grupo_edad + GRA_bin + acceso_digital,
  data = filter(data, post == 0),
  family = binomial
)

# Modelo 2021
m21_bin <- glm(
  violencia_emocional_normalizada ~ grupo_edad + GRA_bin + acceso_digital,
  data = filter(data, post == 1),
  family = binomial
)

# Modelo conjunto con interacción
m_int_bin <- glm(
  violencia_emocional_normalizada ~ post + grupo_edad * post + GRA_bin * post + acceso_digital * post,
  data = data,
  family = binomial
)

# Resultados
summary(m16_bin)
summary(m21_bin)
summary(m_int_bin)

# Ver resultados en Odds Ratios
exp(coef(m16_bin))
exp(coef(m21_bin))
exp(coef(m_int_bin))

# Ver resúmenes con significancia
summary(m16_bin)
summary(m21_bin)
summary(m_int_bin)

###### Medir reconocimiento y no normalizacion#####
#==================================================
data <- data %>%
  mutate(reconocimiento = if_else(violencia_emocional_normalizada == 1, 0, 1))

data <- data %>%
  mutate(grupo_edad = relevel(factor(grupo_edad), ref = "<20"))

m16_rec <- glm(
  reconocimiento ~ grupo_edad + GRA_bin + acceso_digital,
  data = filter(data, post == 0),
  family = binomial
)

m21_rec <- glm(
  reconocimiento ~ grupo_edad + GRA_bin + acceso_digital,
  data = filter(data, post == 1),
  family = binomial
)

m_int_rec <- glm(
  reconocimiento ~ post + grupo_edad * post + GRA_bin * post + acceso_digital * post,
  data = data,
  family = binomial
)

# Odds Ratios
exp(coef(m16_rec))
exp(coef(m21_rec))
exp(coef(m_int_rec))


