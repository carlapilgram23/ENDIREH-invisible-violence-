# ===============================
# Logistic Regression – ENDIREH 2016
# ===============================

library(tidyverse)
library(broom)
library(readr)
library(dplyr)


# ==== 1. Load cleaned data ====

data_2016 <- read_rds("../data/2016/cleaned/data_2016_cleaned.rds")

# ==== 2. Prepare factors ====

data_2016 <- data_2016 %>%
  mutate(
    age_group = factor(grupo_edad, levels = c("<20", "20-29", "30-39", "40-49", "50-59", "60-80", ">80")),
    education = factor(GRA),
    state = factor(NOM_ENT)
  )

# ==== 3. Fit models ====

# Model 1: Emotional violence normalized ~ age + education
model1 <- glm(violencia_emocional_normalizada ~ age_group + education,
              family = binomial(link = "logit"),
              data = data_2016,
              subset = violencia_emocional_vivida == TRUE)

# Model 2: Emotional violence normalized ~ age + education + state
model2 <- glm(violencia_emocional_normalizada ~ age_group + education + state,
              family = binomial(link = "logit"),
              data = data_2016,
              subset = violencia_emocional_vivida == TRUE)


# Model 3: Emotional violence normalized ~ age + education + digital access
model3 <- glm(violencia_emocional_normalizada ~ age_group + education + acceso_digital,
              family = binomial(link = "logit"),
              data = data_2016,
              subset = violencia_emocional_vivida == TRUE)

# Model 4: Emotional violence normalized ~ age + education + state + digital access
model4 <- glm(violencia_emocional_normalizada ~ age_group + education + state + acceso_digital,
              family = binomial(link = "logit"),
              data = data_2016,
              subset = violencia_emocional_vivida == TRUE)


# ==== 4. Save tidy outputs ====

tidy1 <- tidy(model1, exponentiate = TRUE, conf.int = TRUE)
tidy2 <- tidy(model2, exponentiate = TRUE, conf.int = TRUE)
tidy3 <- tidy(model3, exponentiate = TRUE, conf.int = TRUE)
tidy4 <- tidy(model4, exponentiate = TRUE, conf.int = TRUE)

if (!dir.exists("../results/2016/models")) {
  dir.create("../results/2016/models", recursive = TRUE)
}

write_csv(tidy1, "../results/2016/models/logit_emotional_model1.csv")
write_csv(tidy2, "../results/2016/models/logit_emotional_model2.csv")
write_csv(tidy3, "../results/2016/models/logit_emotional_model3.csv")
write_csv(tidy4, "../results/2016/models/logit_emotional_model4.csv")

# ==== 5. Print model summaries ====

cat("✅ Model 1 (without states):\n")
print(tidy1 %>% select(term, estimate, conf.low, conf.high, p.value),
      n = Inf)

cat("\n✅ Model 2 (with states):\n")
print(tidy2 %>% select(term, estimate, conf.low, conf.high, p.value),
      n= Inf)

cat("\n✅ Model 3 (with digital access, no states):\n")
print(tidy3 %>% select(term, estimate, conf.low, conf.high, p.value),
      n = Inf)

cat("\n✅ Model 4 (with digital access and states):\n")
print(tidy4 %>% select(term, estimate, conf.low, conf.high, p.value), 
      n = Inf)

# ==== 6. Compare models by AIC ====
aic_values <- AIC(model1, model2, model3, model4)
print(aic_values)

# ==== 6. Compare models by R2 ====
#install.packages("pscl")
library(dplyr)
library(pscl)

# 1. Create a df for only women who experienced emotional violence
df_model <- data_2016 %>%
  filter(violencia_emocional_vivida == TRUE)

# 2. Adjust the models without subset
m1 <- glm(violencia_emocional_normalizada ~ age_group + education,
          family = binomial, data = df_model)
m2 <- glm(violencia_emocional_normalizada ~ age_group + education + state,
          family = binomial, data = df_model)
m3 <- glm(violencia_emocional_normalizada ~ age_group + education + acceso_digital,
          family = binomial, data = df_model)
m4 <- glm(violencia_emocional_normalizada ~ age_group + education + state + acceso_digital,
          family = binomial, data = df_model)

# Calculate R2
r2_table <- tibble(
  model       = c("m1","m2","m3","m4"),
  McFadden_R2 = c(pR2(m1)["McFadden"],
                  pR2(m2)["McFadden"],
                  pR2(m3)["McFadden"],
                  pR2(m4)["McFadden"])
)

print(r2_table)

# ROC/AUC Curve
library(pROC)
df_mod <- data_2016 %>% filter(violencia_emocional_vivida)
roc2 <- roc(df_mod$violencia_emocional_normalizada, predict(m2, df_mod, type="response"))
roc4 <- roc(df_mod$violencia_emocional_normalizada, predict(m4, df_mod, type="response"))
cat("AUC m2:", auc(roc2), "\nAUC m4:", auc(roc4), "\n")

# More Models


library(lme4)

glmer_m1 <- glmer(
  violencia_emocional_normalizada ~ age_group + education + acceso_digital + (1 | NOM_ENT),
  family = binomial,
  data = df_model
)
summary(glmer_m1)

#  Penalized (LASSO/Ridge) logistic regression
library(glmnet)
X  <- model.matrix(violencia_emocional_normalizada ~ age_group + education + acceso_digital, data = df_model)[,-1]
y  <- df_model$violencia_emocional_normalizada

cv  <- cv.glmnet(X, y, family = "binomial", alpha = 1)  # alpha=1 → LASSO
plot(cv)
coef(cv, s = "lambda.min")


# ==== Logistic Regression – adding physical-violence normalization ====

# Prepara df_model (solo quienes vivieron al menos emocional)
df_model <- data_2016 %>% 
  filter(violencia_emocional_vivida)

# Modelo base (edad + educación + acceso digital + estado)
m4 <- glm(
  violencia_emocional_normalizada ~ age_group + education + state + acceso_digital,
  family = binomial,
  data   = df_model
)

# Modelo extendido con violencia_fisica_normalizada
m5 <- glm(
  violencia_emocional_normalizada ~ age_group 
  + education 
  + state 
  + acceso_digital 
  + violencia_fisica_normalizada,
  family = binomial,
  data   = df_model
)

# Saca resultados
library(broom)
tidy_m4 <- tidy(m4, exponentiate = TRUE, conf.int = TRUE)
tidy_m5 <- tidy(m5, exponentiate = TRUE, conf.int = TRUE)

# Compara AIC y pseudo-R²
library(pscl)
aic_vals <- AIC(m4, m5)
r2_vals  <- tibble(
  model       = c("m4","m5"),
  McFadden_R2 = c(pR2(m4)["McFadden"], pR2(m5)["McFadden"])
)

# Imprime tablas
cat("✅ Model 4 (sin física):\n")
print(tidy_m4 %>% select(term, estimate, conf.low, conf.high, p.value), n=Inf)

cat("\n✅ Model 5 (+ física normalizada):\n")
print(tidy_m5 %>% select(term, estimate, conf.low, conf.high, p.value), n=Inf)

cat("\nAIC comparison:\n"); print(aic_vals)
cat("\nMcFadden's pseudo-R²:\n"); print(r2_vals)

# ==== 8. Save tidy output of Model 5 ====
tidy_m5 <- broom::tidy(m5, exponentiate = TRUE, conf.int = TRUE)

# (Ya debías tener la carpeta creada, pero por si acaso)
if (!dir.exists("../results/2016/models")) {
  dir.create("../results/2016/models", recursive = TRUE)
}

# Escribe el CSV de Model 5
readr::write_csv(
  tidy_m5,
  "../results/2016/models/logit_emotional_model5.csv"
)



