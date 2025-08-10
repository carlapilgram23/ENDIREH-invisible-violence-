
library(tidyverse)


# ============================
# 1) Paquetes
# ============================
library(tidyverse)
library(forcats)
library(broom)
library(ggplot2)
library(ggeffects)


data <- read_csv("../data/combined/data_combined.csv")



# 0) Make sure factors are set BEFORE fitting the model
data <- data %>%
  mutate(
    year       = factor(year),
    grupo_edad = forcats::fct_relevel(grupo_edad, "60-79",
                                      "<20","20-29","30-39","40-49","50-59",">80"),
    GRA_bin    = factor(GRA_bin, levels = c(0,1), labels = c("No","Yes")),
    P1_4_9     = factor(P1_4_9, levels = c(2,1), labels = c("No","Yes"))
  )

# 1) Refit interaction model (if needed)
model_interactions_oldref <- glm(
  violencia_emocional_reconocida ~ year + grupo_edad * P1_4_9 + GRA_bin * P1_4_9,
  data = data, family = binomial()
)

# 2) Extract ORs
extract_OR <- function(model) {
  broom::tidy(model, exponentiate = TRUE, conf.int = TRUE, conf.method = "wald") |>
    dplyr::select(term, estimate, conf.low, conf.high, p.value) |>
    dplyr::rename(OR = estimate, CI_low = conf.low, CI_high = conf.high, p_value = p.value)
}

plot_df <- extract_OR(model_interactions_oldref) |>
  dplyr::filter(term != "(Intercept)")

# 3) Pretty labels (match ENGLISH term names from the model)
pretty_terms <- c(
  "year2021" = "Year 2021",
  "GRA_binYes" = "Higher education",
  "P1_4_9Yes"  = "Internet access",
  "grupo_edad<20"   = "Age <20",
  "grupo_edad20-29" = "Age 20–29",
  "grupo_edad30-39" = "Age 30–39",
  "grupo_edad40-49" = "Age 40–49",
  "grupo_edad50-59" = "Age 50–59",
  "grupo_edad>80"   = "Age >80",
  "grupo_edad<20:P1_4_9Yes"   = "Age <20 × Internet",
  "grupo_edad20-29:P1_4_9Yes" = "Age 20–29 × Internet",
  "grupo_edad30-39:P1_4_9Yes" = "Age 30–39 × Internet",
  "grupo_edad40-49:P1_4_9Yes" = "Age 40–49 × Internet",
  "grupo_edad50-59:P1_4_9Yes" = "Age 50–59 × Internet",
  "grupo_edad>80:P1_4_9Yes"   = "Age >80 × Internet",
  "P1_4_9Yes:GRA_binYes"      = "Internet × Higher education"
)

plot_df <- plot_df |>
  dplyr::mutate(label = dplyr::recode(term, !!!pretty_terms, .default = term))

# 4) Order labels (do NOT include the reference age—there’s no coef for 60–79)
desired_order <- c(
  "Age <20","Age 20–29","Age 30–39","Age 40–49","Age 50–59","Age >80",
  "Higher education","Internet access","Year 2021",
  "Age <20 × Internet","Age 20–29 × Internet","Age 30–39 × Internet",
  "Age 40–49 × Internet","Age 50–59 × Internet","Age >80 × Internet",
  "Internet × Higher education"
)

present_order <- desired_order[desired_order %in% plot_df$label]

plot_df <- plot_df |>
  dplyr::filter(label %in% present_order) |>
  dplyr::mutate(label = factor(label, levels = present_order))

# 5) Forest plot
ggplot(plot_df, aes(x = label, y = OR)) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.18) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "red") +
  coord_flip() +
  labs(
    title = "Odds Ratios for Recognizing Emotional Violence",
    subtitle = "Reference: Age 60–79, lower education, no internet, year 2016",
    x = "", y = "Odds Ratio (95% CI)"
  ) +
  theme_minimal(base_size = 13)

# ============================
# 6) Predicted probabilities plot
# ============================
preds_oldref <- ggpredict(
  model_interactions_oldref,
  terms = c("grupo_edad","P1_4_9"),
  condition = list(year = "2021")  # Cambia a "2016" si quieres
)

ggplot(preds_oldref, aes(x = x, y = predicted, color = group, group = group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .15) +
  labs(
    title = "Predicted Probability of Recognising Emotional Violence",
    subtitle = "Interaction: Age group × Internet access (Reference: Age 60–79)",
    x = "Age group", y = "Predicted probability", color = "Internet access"
  ) +
  theme_minimal(base_size = 12)
#========================================
#=========================================
library(dplyr)
library(forcats)

data <- data %>%
  mutate(
    year       = factor(year, levels = c(2016, 2021)),
    grupo_edad = fct_relevel(grupo_edad, "60-79",
                             "<20","20-29","30-39","40-49","50-59",">80"),
    GRA_bin    = factor(GRA_bin, levels = c(0,1), labels = c("No","Yes")),
    P1_4_9     = factor(P1_4_9, levels = c(2,1), labels = c("No","Yes"))
  )

# tu modelo con interacciones edad×internet y educación×internet (sin estados)
modelo <- glm(
  violencia_emocional_reconocida ~ year + grupo_edad * P1_4_9 + GRA_bin * P1_4_9,
  family = binomial(), data = data
)
library(emmeans)

# Predicción ajustada por año (promedia sobre la distribución observada del resto)
emm_year <- emmeans(modelo, ~ year, type = "response")
summary(emm_year)        # Probabilidad ajustada en 2016 y 2021

# Diferencia 2021−2016 en puntos porcentuales (pp)
diff_pp <- pairs(emm_year) %>% summary(infer = TRUE)  # type="response" ⇒ diferencia en prob.
diff_pp$estimate_pp <- 100 * diff_pp$estimate
diff_pp$lower_pp    <- 100 * diff_pp$lower.CL
diff_pp$upper_pp    <- 100 * diff_pp$upper.CL
diff_pp
# En la escala del enlace logit (por defecto), el contraste es Δ logit ⇒ exp() = OR
or_year <- contrast(emmeans(modelo, ~ year), "revpairwise") %>%
  summary(infer = TRUE) %>%
  mutate(OR = exp(estimate), CI_low = exp(lower.CL), CI_high = exp(upper.CL))
or_year[, c("contrast","OR","CI_low","CI_high","p.value")]
# Probabilidades ajustadas por año, condicionando por edad e internet
emm_sub <- emmeans(
  modelo, ~ year | grupo_edad + P1_4_9,
  type = "response"
)

# Diferencia 2021−2016 dentro de cada (edad × internet), en probabilidad
dif_sub <- pairs(emm_sub) %>%
  summary(infer = TRUE) %>%
  as.data.frame() %>%
  mutate(
    delta_pp = 100 * estimate,
    lower_pp = 100 * lower.CL,
    upper_pp = 100 * upper.CL
  )
head(dif_sub)
library(ggplot2)

ggplot(dif_sub,
       aes(x = grupo_edad, y = delta_pp, fill = P1_4_9)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_col(position = position_dodge(width = .7), width = .6) +
  geom_errorbar(aes(ymin = lower_pp, ymax = upper_pp),
                position = position_dodge(width = .7), width = .2) +
  labs(
    title = "Change in Recognising Emotional Violence (2021 − 2016)",
    subtitle = "Adjusted difference in percentage points, by age group and internet access",
    x = "Age group", y = "Δ percentage points", fill = "Internet access"
  ) +
  theme_minimal(base_size = 12)



####========================
# ============================
# Paquetes
# ============================
library(tidyverse)
library(forcats)
library(emmeans)
library(broom)

# ============================
# Datos (factores y referencias)
# ============================
data <- readr::read_csv("../data/combined/data_combined.csv", show_col_types = FALSE) %>%
  mutate(
    year       = factor(year, levels = c(2016, 2021)),
    grupo_edad = fct_relevel(grupo_edad, "60-79",
                             "<20","20-29","30-39","40-49","50-59",">80"),
    GRA_bin    = factor(GRA_bin, levels = c(0,1), labels = c("No","Yes")),
    P1_4_9     = factor(P1_4_9, levels = c(2,1), labels = c("No","Yes"))
  )
# Si tu análisis es solo entre quienes vivieron violencia emocional, descomenta:
# data <- data %>% filter(violencia_emocional_vivida == 1)

# ============================
# Modelos (base y con interacciones con YEAR)
# ============================
model_base <- glm(
  violencia_emocional_reconocida ~ year + grupo_edad * P1_4_9 + GRA_bin * P1_4_9,
  data = data, family = binomial()
)

model_timeint <- glm(
  violencia_emocional_reconocida ~ 
    year * (grupo_edad + P1_4_9 + GRA_bin) + 
    grupo_edad * P1_4_9 + GRA_bin * P1_4_9,
  data = data, family = binomial()
)

# (Opcional) Comparación de ajuste
print(anova(model_base, model_timeint, test = "Chisq"))

# ============================
# 1) Efecto global del tiempo (2016 → 2021)
# ============================
# Emmeans por año en link y a respuesta
emm_year      <- emmeans(model_timeint, ~ year)
emm_year_resp <- regrid(emm_year, transform = "response")

# Probabilidades ajustadas (robusto al nombre de la col de prob)
s <- as.data.frame(summary(emm_year_resp))
val_col <- intersect(c("response","prob","emmean",".response"), names(s))[1]
low_col <- if ("lower.CL" %in% names(s)) "lower.CL" else "asymp.LCL"
up_col  <- if ("upper.CL" %in% names(s)) "upper.CL" else "asymp.UCL"

overall_pp <- s |>
  dplyr::mutate(
    prob     = .data[[val_col]],
    prob_pct = 100 * prob,
    lower_ci = .data[[low_col]],
    upper_ci = .data[[up_col]]
  ) |>
  dplyr::select(year, prob, prob_pct, SE, lower_ci, upper_ci)
print(overall_pp)

# Δ pp (2021 − 2016) — usar reverse=TRUE para asegurar el orden
d <- as.data.frame(summary(pairs(emm_year_resp, reverse = TRUE), infer = TRUE))
# detectar nombres de columnas de CI
low_col <- if ("lower.CL" %in% names(d)) "lower.CL" else "asymp.LCL"
up_col  <- if ("upper.CL" %in% names(d)) "upper.CL"  else "asymp.UCL"

overall_diff <- d |>
  dplyr::transmute(
    contrast,
    delta_pp = 100 * estimate,
    lower_pp = 100 * .data[[low_col]],
    upper_pp = 100 * .data[[up_col]],
    p.value
  )
print(overall_diff)
# OR for 2021 vs 2016 (on logit scale), robust to CI column names
emm_year <- emmeans(model_timeint, ~ year)  # link scale

or_raw <- as.data.frame(
  summary(
    contrast(emm_year, method = list("2021 vs 2016" = c(-1, 1))),
    infer = TRUE
  )
)

low_col <- if ("lower.CL" %in% names(or_raw)) "lower.CL" else "asymp.LCL"
up_col  <- if ("upper.CL" %in% names(or_raw)) "upper.CL"  else "asymp.UCL"

or_year <- or_raw %>%
  dplyr::transmute(
    contrast,
    OR     = exp(estimate),
    CI_low = exp(.data[[low_col]]),
    CI_high= exp(.data[[up_col]]),
    p.value
  )

print(or_year)



# ============================
# 2) Efecto del tiempo por subgrupos (Edad × Internet × Educación)
# ============================
# Emmeans por año condicionado a subgrupos
# Emmeans por año condicionado a subgrupos (link) y pasar a respuesta
emm_sub      <- emmeans(model_timeint, ~ year | grupo_edad + P1_4_9 + GRA_bin)
emm_sub_resp <- regrid(emm_sub, transform = "response")

# Pares (2021 − 2016) en escala de respuesta
ds <- as.data.frame(summary(pairs(emm_sub_resp, reverse = TRUE), infer = TRUE))

# Detectar cómo se llaman las columnas de IC en tu versión
ci_low_col  <- if ("lower.CL" %in% names(ds)) "lower.CL" else "asymp.LCL"
ci_high_col <- if ("upper.CL" %in% names(ds)) "upper.CL" else "asymp.UCL"

# Asegurar que tomamos las columnas de agrupación que sí existen en 'ds'
by_cols <- intersect(c("grupo_edad","P1_4_9","GRA_bin"), names(ds))

# Construir tabla final (Δ pp con IC95% y BH)
dif_sub <- ds %>%
  mutate(
    delta_pp = 100 * estimate,
    lower_pp = 100 * .data[[ci_low_col]],
    upper_pp = 100 * .data[[ci_high_col]]
  ) %>%
  select(all_of(by_cols), delta_pp, lower_pp, upper_pp, p.value) %>%
  arrange(GRA_bin, grupo_edad, P1_4_9) %>%
  mutate(p_adj_BH = p.adjust(p.value, method = "BH"))

print(head(dif_sub, 10))

# Guardar
dir.create("../results/combined/tables", recursive = TRUE, showWarnings = FALSE)
readr::write_csv(dif_sub, "../results/combined/tables/delta_pp_by_age_internet_education.csv")

# ============================
# 3) Gráfico: Δ pp por Edad × Internet (facetas por Educación)
# ============================
# Reorder age just for plotting + significance symbols
dif_sub_plot <- dif_sub %>%
  mutate(
    grupo_edad_plot = factor(grupo_edad,
                             levels = c("<20","20-29","30-39","40-49","50-59","60-79",">80")),
    sig = case_when(
      p_adj_BH < 0.001 ~ "***",
      p_adj_BH < 0.01  ~ "**",
      p_adj_BH < 0.05  ~ "*",
      p_adj_BH < 0.10  ~ "•",
      TRUE ~ ""
    ),
    y_pos = if_else(delta_pp >= 0, upper_pp + 0.35, lower_pp - 0.35) # label offset
  )

# (Optional) Ns per subgroup to show in caption
Ns <- data %>%
  count(year, grupo_edad, P1_4_9, GRA_bin) %>%
  group_by(grupo_edad, P1_4_9, GRA_bin) %>%
  summarise(N_2016 = n[year == 2016], N_2021 = n[year == 2021], .groups = "drop")

# Plot with ordered age and significance marks
p_change <- ggplot(dif_sub_plot,
                   aes(x = grupo_edad_plot, y = delta_pp, fill = P1_4_9)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_col(position = position_dodge(width = .7), width = .6) +
  geom_errorbar(aes(ymin = lower_pp, ymax = upper_pp),
                position = position_dodge(width = .7), width = .18) +
  geom_text(aes(y = y_pos, label = sig),
            position = position_dodge(width = .7), size = 4) +
  facet_wrap(~ GRA_bin, nrow = 1,
             labeller = labeller(GRA_bin = c(No = "Lower education",
                                             Yes = "Higher education"))) +
  labs(
    title = "Adjusted difference in percentage points by Age × Internet access, faceted by Education",
    subtitle = "Change in recognising emotional violence, 2021 − 2016 (95% CI)",
    x = "Age group", y = "Δ percentage points (pp)", fill = "Internet access",
    caption = "Significance: *** p<0.001, ** p<0.01, * p<0.05, • p<0.10 (BH-adjusted)"
  ) +
  theme_minimal(base_size = 12)

print(p_change)
ggsave("../results/combined/plots/delta_pp_age_internet_by_education_sig.png",
       p_change, width = 11, height = 5.5, dpi = 300)
# ============================
# 4) Mini resumen (top cambios)
# ============================
top_up   <- dif_sub %>% arrange(desc(delta_pp)) %>% slice_head(n = 5)
top_down <- dif_sub %>% arrange(delta_pp)      %>% slice_head(n = 5)
print(top_up)
print(top_down)

# 1) Ns per subgroup (to show in facet strips)
Ns <- data %>%
  count(year, grupo_edad, P1_4_9, GRA_bin) %>%
  group_by(grupo_edad, P1_4_9, GRA_bin) %>%
  summarise(N_2016 = sum(n[year == 2016], na.rm = TRUE),
            N_2021 = sum(n[year == 2021], na.rm = TRUE),
            .groups = "drop")

# 2) Merge Ns + tidy labels and significance symbols (you already had 'sig')
dif_sub_plot <- dif_sub %>%
  left_join(Ns, by = c("grupo_edad","P1_4_9","GRA_bin")) %>%
  mutate(
    grupo_edad = factor(grupo_edad,
                        levels = c("<20","20-29","30-39","40-49","50-59","60-79",">80")),
    sig = case_when(
      p_adj_BH < 0.001 ~ "***",
      p_adj_BH < 0.01  ~ "**",
      p_adj_BH < 0.05  ~ "*",
      p_adj_BH < 0.10  ~ "•",
      TRUE ~ ""
    ),
    # place the text just above/below CI
    y_pos = if_else(delta_pp >= 0, upper_pp + 0.35, lower_pp - 0.35),
    lab   = sprintf("%.1f", delta_pp)  # delta value label
  )

# 3) Plot: value labels + significance + Ns in caption
p_change <- ggplot(dif_sub_plot,
                   aes(x = grupo_edad, y = delta_pp, fill = P1_4_9)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_col(position = position_dodge(width = .7), width = .6) +
  geom_errorbar(aes(ymin = lower_pp, ymax = upper_pp),
                position = position_dodge(width = .7), width = .18) +
  geom_text(aes(y = y_pos, label = sig),
            position = position_dodge(width = .7), size = 4) +
  geom_text(aes(label = lab),
            position = position_dodge(width = .7), vjust = ifelse(dif_sub_plot$delta_pp>=0, -0.2, 1.2),
            size = 3.2, color = "gray30") +
  facet_wrap(~ GRA_bin, nrow = 1,
             labeller = labeller(GRA_bin = c(No = "Lower education",
                                             Yes = "Higher education"))) +
  labs(
    title = "Adjusted difference in percentage points by Age × Internet access, faceted by Education",
    subtitle = "Change in recognising emotional violence, 2021 − 2016 (95% CI)",
    x = "Age group", y = "Δ percentage points (pp)", fill = "Internet access",
    caption = "Significance: *** p<0.001, ** p<0.01, * p<0.05, • p<0.10 (BH-adjusted)"
  ) +
  theme_minimal(base_size = 12)

print(p_change)

## Add levels
library(ggeffects)

# Predicted probabilities for each subgroup, by year
# Predicted probabilities for each subgroup, by year (robust to column names)
emm_sub_link  <- emmeans(model_timeint, ~ year | grupo_edad + P1_4_9 + GRA_bin)
emm_sub_resp  <- regrid(emm_sub_link, transform = "response")

s <- as.data.frame(summary(emm_sub_resp))

# detect column names across emmeans versions
val_col <- intersect(c("response","prob","emmean",".response"), names(s))[1]
low_col <- if ("lower.CL" %in% names(s)) "lower.CL" else "asymp.LCL"
up_col  <- if ("upper.CL" %in% names(s)) "upper.CL"  else "asymp.UCL"

levs_df <- s %>%
  dplyr::transmute(
    year, grupo_edad, P1_4_9, GRA_bin,
    prob  = .data[[val_col]],
    lower = .data[[low_col]],
    upper = .data[[up_col]]
  ) %>%
  dplyr::mutate(
    grupo_edad = factor(grupo_edad,
                        levels = c("<20","20-29","30-39","40-49","50-59","60-79",">80"))
  )

# Plot levels (2016 vs 2021) per subgroup
library(ggplot2)
p_levels <- ggplot(levs_df,
                   aes(x = grupo_edad, y = prob, group = year, color = year)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .15) +
  facet_wrap(~ GRA_bin + P1_4_9, nrow = 2,
             labeller = labeller(GRA_bin = c(No = "Lower education", Yes = "Higher education"),
                                 P1_4_9 = c(No = "No internet", Yes = "Internet"))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Predicted probability of recognising emotional violence",
    subtitle = "By Age × Internet access × Education; lines compare 2016 vs 2021",
    x = "Age group", y = "Predicted probability"
  ) +
  theme_minimal(base_size = 12)

print(p_levels)

# Save
dir.create("../results/combined/plots", recursive = TRUE, showWarnings = FALSE)
ggsave("../results/combined/plots/predicted_levels_by_subgroup.png",
       p_levels, width = 12, height = 7, dpi = 300)


# ============================
# Solo mujeres que vivieron violencia emocional
# ============================
library(tidyverse)
library(forcats)
library(emmeans)
library(readr)
library(scales)

# 1) Datos + filtros + factores
data_viv <- read_csv("../data/combined/data_combined.csv", show_col_types = FALSE) %>%
  filter(violencia_emocional_vivida %in% c(1, TRUE)) %>%   # <-- solo quienes la vivieron
  mutate(
    year       = factor(year, levels = c(2016, 2021)),
    grupo_edad = fct_relevel(grupo_edad, "60-79",
                             "<20","20-29","30-39","40-49","50-59",">80"),
    GRA_bin    = factor(GRA_bin, levels = c(0,1), labels = c("No","Yes")),
    P1_4_9     = factor(P1_4_9, levels = c(2,1), labels = c("No","Yes"))
  )

# (opcional) chequeo rápido
data_viv %>% count(year)

# 2) Modelo con interacciones (mismo que antes)
model_timeint_viv <- glm(
  violencia_emocional_reconocida ~ 
    year * (grupo_edad + P1_4_9 + GRA_bin) +
    grupo_edad * P1_4_9 + GRA_bin * P1_4_9,
  data = data_viv, family = binomial()
)

# 3) Emmeans por año dentro de cada subgrupo (respuesta) -> niveles 2016/2021
emm_sub_link  <- emmeans(model_timeint_viv, ~ year | grupo_edad + P1_4_9 + GRA_bin)
emm_sub_resp  <- regrid(emm_sub_link, transform = "response")
s <- as.data.frame(summary(emm_sub_resp))

# columnas robustas (cambian según versión de emmeans)
val_col <- intersect(c("response","prob","emmean",".response"), names(s))[1]
low_col <- if ("lower.CL" %in% names(s)) "lower.CL" else "asymp.LCL"
up_col  <- if ("upper.CL" %in% names(s)) "upper.CL"  else "asymp.UCL"

levels_long <- s %>%
  transmute(grupo_edad, P1_4_9, GRA_bin, year,
            prob  = .data[[val_col]],
            lower = .data[[low_col]],
            upper = .data[[up_col]])

levels_wide <- levels_long %>%
  mutate(year = paste0("y", year)) %>%
  pivot_wider(names_from = year, values_from = c(prob, lower, upper), names_sep = "_")

# 4) Δ pp (2021–2016) por subgrupo con BH
pairs_df <- as.data.frame(summary(pairs(emm_sub_resp, reverse = TRUE), infer = TRUE))
ci_low_col  <- if ("lower.CL" %in% names(pairs_df)) "lower.CL" else "asymp.LCL"
ci_high_col <- if ("upper.CL" %in% names(pairs_df)) "upper.CL" else "asymp.UCL"

deltas <- pairs_df %>%
  transmute(grupo_edad, P1_4_9, GRA_bin,
            delta_pp = 100 * estimate,
            lower_pp = 100 * .data[[ci_low_col]],
            upper_pp = 100 * .data[[ci_high_col]],
            p.value,
            p_adj_BH = p.adjust(p.value, method = "BH"))

# 5) Tabla final bonita para apéndice
final_tab_viv <- levels_wide %>%
  left_join(deltas, by = c("grupo_edad","P1_4_9","GRA_bin")) %>%
  mutate(
    Education   = recode(GRA_bin, "No"="Lower education", "Yes"="Higher education"),
    Internet    = recode(P1_4_9, "No"="No internet", "Yes"="Internet"),
    `Age group` = factor(grupo_edad, levels = c("<20","20-29","30-39","40-49","50-59","60-79",">80")),
    `2016 prob` = scales::percent(prob_y2016, accuracy = 0.1),
    `2016 CI`   = sprintf("(%s, %s)", scales::percent(lower_y2016, 0.1), scales::percent(upper_y2016, 0.1)),
    `2021 prob` = scales::percent(prob_y2021, accuracy = 0.1),
    `2021 CI`   = sprintf("(%s, %s)", scales::percent(lower_y2021, 0.1), scales::percent(upper_y2021, 0.1)),
    `Δ pp (2021−2016)` = sprintf("%.1f", delta_pp),
    `Δ 95% CI`         = sprintf("(%.1f, %.1f)", lower_pp, upper_pp),  # <— un solo %
    `BH p-value`        = scales::pvalue(p_adj_BH, add_p = TRUE, accuracy = 0.001)
  ) %>%
  arrange(Education, Internet, `Age group`) %>%
  select(Education, Internet, `Age group`,
         `2016 prob`, `2016 CI`, `2021 prob`, `2021 CI`,
         `Δ pp (2021−2016)`, `Δ 95% CI`, `BH p-value`)


# 6) Guardar para el apéndice
dir.create("../results/combined/tables", recursive = TRUE, showWarnings = FALSE)
write_csv(final_tab_viv, "../results/combined/tables/levels_and_deltas_by_subgroup.csv")

# vistazo
print(head(final_tab_viv, 12))

library(readr)
library(dplyr)

library(flextable)
library(officer)

library(dplyr); library(readr)

# Partiendo de levels_wide y deltas que ya generaste arriba
fmt_pct <- function(x) ifelse(is.na(x), NA_character_, sprintf("%.1f", 100*x))

compact_tab_viv <- levels_wide %>%
  left_join(deltas, by = c("grupo_edad","P1_4_9","GRA_bin")) %>%
  mutate(
    Education   = recode(GRA_bin, "No"="Lower education", "Yes"="Higher education"),
    Internet    = recode(P1_4_9, "No"="No internet", "Yes"="Internet"),
    `Age group` = factor(grupo_edad, levels = c("<20","20-29","30-39","40-49","50-59","60-79",">80")),
    `2016 % (95% CI)` = sprintf("%s (%s–%s)",
                                fmt_pct(prob_y2016), fmt_pct(lower_y2016), fmt_pct(upper_y2016)),
    `2021 % (95% CI)` = sprintf("%s (%s–%s)",
                                fmt_pct(prob_y2021), fmt_pct(lower_y2021), fmt_pct(upper_y2021)),
    `Δ pp (95% CI)`   = sprintf("%s (%s, %s)",
                                sprintf("%.1f", delta_pp),
                                sprintf("%.1f", lower_pp),
                                sprintf("%.1f", upper_pp)),
    `BH p-value`      = scales::pvalue(p_adj_BH, add_p = TRUE, accuracy = 0.001)
  ) %>%
  arrange(Education, Internet, `Age group`) %>%
  select(Education, Internet, `Age group`,
         `2016 % (95% CI)`, `2021 % (95% CI)`,
         `Δ pp (95% CI)`, `BH p-value`)

# Guarda CSV compacto
dir.create("../results/combined/tables", recursive = TRUE, showWarnings = FALSE)
write_csv(compact_tab_viv, "../results/combined/tables/levels_deltas_compact_experienced.csv")

library(flextable); library(officer)

ft <- flextable(compact_tab_viv) |>
  autofit() |>
  fontsize(size = 9) |>                 # letra más pequeña
  fit_to_width(max_width = 6.5) |>      # ajusta al ancho (p. ej., 6.5" para página A4/Letter)
  align(align = "center", part = "all") |>
  align(j = 1:3, align = "left", part = "all") |>
  set_caption("Table A1. Recognising emotional violence among exposed women: 2016 vs 2021")

print(ft, preview = "docx",
      path = "../results/combined/tables/appendix_levels_deltas_compact_experienced.docx")

