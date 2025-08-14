# ============================
# 1) Packages
# ============================
library(tidyverse)
library(forcats)
library(broom)
library(ggplot2)
library(ggeffects)

# ============================
# 2) Load + keep ONLY victims of emotional violence
# ============================
data <- read_csv("../data/combined/data_combined.csv", show_col_types = FALSE) %>%
  filter(violencia_emocional_vivida %in% c(1, TRUE)) %>%   # <-- key line
  mutate(
    year       = factor(year, levels = c(2016, 2021)),      # 2016 as ref
    grupo_edad = fct_relevel(grupo_edad, "60-79",
                             "<20","20-29","30-39","40-49","50-59",">80"),
    GRA_bin    = factor(GRA_bin, levels = c(0,1), labels = c("No","Yes")),
    P1_4_9     = factor(P1_4_9, levels = c(2,1), labels = c("No","Yehttp://127.0.0.1:30111/graphics/8646fa2b-90e3-4446-a68b-451bb5c418f4.pngs"))
  ) %>%
  drop_na(violencia_emocional_reconocida, year, grupo_edad, GRA_bin, P1_4_9)

# (Optional) sanity check sample sizes
data %>% count(year) %>% print()

# ============================
# 3) Model (same spec, victims-only)
# ============================
model_interactions_oldref <- glm(
  violencia_emocional_reconocida ~ year + grupo_edad * P1_4_9 + GRA_bin * P1_4_9,
  data = data, family = binomial()
)

# ============================
# 4) Odds ratios (forest data)
# ============================
extract_OR <- function(model) {
  broom::tidy(model, exponentiate = TRUE, conf.int = TRUE, conf.method = "wald") %>%
    select(term, estimate, conf.low, conf.high, p.value) %>%
    rename(OR = estimate, CI_low = conf.low, CI_high = conf.high, p_value = p.value)
}

plot_df <- extract_OR(model_interactions_oldref) %>%
  filter(term != "(Intercept)")

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

plot_df <- plot_df %>%
  mutate(label = dplyr::recode(term, !!!pretty_terms, .default = term))

desired_order <- c(
  "Age <20","Age 20–29","Age 30–39","Age 40–49","Age 50–59","Age >80",
  "Higher education","Internet access","Year 2021",
  "Age <20 × Internet","Age 20–29 × Internet","Age 30–39 × Internet",
  "Age 40–49 × Internet","Age 50–59 × Internet","Age >80 × Internet",
  "Internet × Higher education"
)
plot_df <- plot_df %>%
  filter(label %in% desired_order) %>%
  mutate(label = factor(label, levels = desired_order))

# Forest plot (victims-only)
p_forest <- ggplot(plot_df, aes(x = label, y = OR)) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.18) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "red") +
  coord_flip() +
  labs(
    title = "Odds Ratios for Recognising Emotional Violence",
    subtitle = "Reference: Age 60–79, lower education, no internet, year 2016",
    x = "", y = "Odds Ratio (95% CI)"
  ) +
  theme_minimal(base_size = 13)

print(p_forest)
ggsave("../results/combined/plots/forest_victims_only.png", p_forest, width = 9, height = 6, dpi = 300)

# ============================
# 5) Predicted probabilities plot (victims-only)
# ============================
library(stringr)
age_levels <- c("<20","20-29","30-39","40-49","50-59","60-79",">80")

preds_oldref <- ggpredict(
  model_interactions_oldref,
  terms = c("age_levels","P1_4_9"),
  condition = list(year = "2021")
) %>%
  dplyr::mutate(
    # Cambia cualquier en/em dash por guion normal
    x = str_replace_all(x, "[\u2013\u2014]", "-"),
    x = factor(x, levels = age_levels)
  )


p_levels <- ggplot(preds_oldref, aes(x = x, y = predicted, color = group, group = group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .15) +
  scale_x_discrete(drop = FALSE) +
  labs(x = "Age group", y = "Predicted probability", color = "Internet access") +
  theme_minimal(base_size = 12)

print(p_levels)

ggsave("../results/combined/plots/levels_victims_only_2021.png", p_levels, width = 9, height = 5, dpi = 300)


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
data <- data %>% filter(violencia_emocional_vivida == 1)

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

AIC(model_base, model_timeint)


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

ggsave("../results/combined/plots/  .png",
       p_levels, width = 12, height = 7, dpi = 300)


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


#=================================#=================================#=================================#=================================
#=================================#=================================#=================================
# ============================
# 1) Paquetes
# ============================
library(tidyverse)
library(forcats)
library(emmeans)
library(broom)
library(ggplot2)

# ============================
# 2) Datos: SOLO víctimas + factores
# ============================
data_vict <- readr::read_csv("../data/combined/data_combined.csv", show_col_types = FALSE) %>%
  filter(violencia_emocional_vivida %in% c(1, TRUE)) %>%
  mutate(
    year       = factor(year, levels = c(2016, 2021)),  # 2016 = ref
    grupo_edad = fct_relevel(grupo_edad, "60-79",
                             "<20","20-29","30-39","40-49","50-59",">80"),
    GRA_bin    = factor(GRA_bin, levels = c(0,1), labels = c("No","Yes")),
    P1_4_9     = factor(P1_4_9, levels = c(2,1), labels = c("No","Yes"))
  ) %>%
  drop_na(violencia_emocional_reconocida, year, grupo_edad, GRA_bin, P1_4_9)

# ============================
# 3) Modelo: year × edad × internet  +  year × edad × educación
# ============================
m_both3 <- glm(
  violencia_emocional_reconocida ~
    year * grupo_edad * P1_4_9 +
    year * grupo_edad * GRA_bin,
  data = data_vict, family = binomial()
)

# ============================
# 4) CONTRASTES 2021 vs 2016 por subgrupo (Edad × Internet × Educación)
# ============================
emm_sub_link  <- emmeans(m_both3, ~ year | grupo_edad + P1_4_9 + GRA_bin)   # escala logit
emm_sub_resp  <- regrid(emm_sub_link, transform = "response")                # a probabilidades

## OR 2021 vs 2016
or_raw <- as.data.frame(
  summary(contrast(emm_sub_link, method = list("2021 vs 2016" = c(-1, 1))), infer = TRUE)
)
ci_low  <- intersect(c("lower.CL","asymp.LCL"), names(or_raw))[1]
ci_high <- intersect(c("upper.CL","asymp.UCL"), names(or_raw))[1]

or_subgroups <- or_raw %>%
  transmute(
    grupo_edad, P1_4_9, GRA_bin,
    OR      = exp(estimate),
    CI_low  = exp(.data[[ci_low]]),
    CI_high = exp(.data[[ci_high]]),
    p.value
  ) %>%
  mutate(
    Education = recode(GRA_bin, "No"="Lower education", "Yes"="Higher education"),
    Internet  = recode(P1_4_9, "No"="No internet", "Yes"="Internet"),
    grupo_edad = factor(grupo_edad, levels = c("<20","20-29","30-39","40-49","50-59","60-79",">80"))
  )

## Δ puntos porcentuales (2021 − 2016)
ds <- as.data.frame(summary(pairs(emm_sub_resp, reverse = TRUE), infer = TRUE))
ci_low_r  <- intersect(c("lower.CL","asymp.LCL"), names(ds))[1]
ci_high_r <- intersect(c("upper.CL","asymp.UCL"), names(ds))[1]

delta_pp <- ds %>%
  transmute(
    grupo_edad, P1_4_9, GRA_bin,
    delta_pp = 100 * estimate,
    lower_pp = 100 * .data[[ci_low_r]],
    upper_pp = 100 * .data[[ci_high_r]],
    p.value,
    p_adj_BH = p.adjust(p.value, method = "BH")
  ) %>%
  mutate(
    Education = recode(GRA_bin, "No"="Lower education", "Yes"="Higher education"),
    Internet  = recode(P1_4_9, "No"="No internet", "Yes"="Internet"),
    grupo_edad = factor(grupo_edad, levels = c("<20","20-29","30-39","40-49","50-59","60-79",">80"))
  )

# ============================
# 5) Forest de OR por subgrupo (facetas por Educación)
# ============================
p_forest <- ggplot(or_subgroups,
                   aes(y = grupo_edad, x = OR, color = Internet)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  geom_point(position = position_dodge(width = .4)) +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high),
                 height = .15, position = position_dodge(width = .4)) +
  facet_grid(Education ~ .) +
  labs(
    title = "2021 vs 2016 – Recognising Emotional Violence (victims only)",
    subtitle = "Odds ratios by Age × Internet (faceted by Education)",
    x = "Odds Ratio (2021 vs 2016)", y = "Age group", color = "Internet"
  ) +
  theme_minimal(base_size = 12)

print(p_forest)
ggsave("../results/combined/plots/forest_year_x_age_x_internet_faceted_by_edu.png",
       p_forest, width = 10, height = 6.5, dpi = 300)

# ============================
# 6) Probabilidades por Edad × Internet × Año (facetas por Educación)
# ============================
emm_sub_link  <- emmeans(model_timeint, ~ year | grupo_edad + P1_4_9 + GRA_bin)
emm_sub_resp  <- regrid(emm_sub_link, transform = "response")

s <- as.data.frame(summary(emm_sub_resp))

# Make this robust to emmeans column-name differences
val_col <- intersect(c("response","prob","emmean",".response"), names(s))[1]
low_col <- if ("lower.CL" %in% names(s)) "lower.CL" else "asymp.LCL"
up_col  <- if ("upper.CL" %in% names(s)) "upper.CL"  else "asymp.UCL"

levs <- s %>%
  transmute(
    year, grupo_edad, P1_4_9, GRA_bin,
    prob  = .data[[val_col]],
    lower = .data[[low_col]],
    upper = .data[[up_col]]
  ) %>%
  mutate(
    Education = recode(GRA_bin, "No"="Lower education", "Yes"="Higher education"),
    Internet  = recode(P1_4_9, "No"="No internet", "Yes"="Internet"),
    grupo_edad = factor(grupo_edad,
                        levels = c("<20","20-29","30-39","40-49","50-59","60-79",">80"))
  )

# (Optional) drop any rows with NA/Inf to avoid warnings
levs <- levs %>% filter(is.finite(prob), is.finite(lower), is.finite(upper))

p_levels <- ggplot(levs,
                   aes(x = grupo_edad, y = prob,
                       color = Internet, group = interaction(Internet, year))) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .15) +
  facet_grid(Education ~ year) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Predicted probability by Age × Internet × Year (victims only)",
    subtitle = "Faceted by Education (Lower vs Higher)",
    x = "Age group", y = "Predicted probability", color = "Internet"
  ) +
  theme_minimal(base_size = 12)

print(p_levels)


# ============================
# 7) Guardar tablas (opcional)
# ============================
dir.create("../results/combined/tables", recursive = TRUE, showWarnings = FALSE)
readr::write_csv(or_subgroups, "../results/combined/tables/or_year_by_age_internet_edu.csv")
readr::write_csv(delta_pp,   "../results/combined/tables/deltapp_year_by_age_internet_edu.csv")

#===================================================================
#===============================================================
# --- Packages
library(tidyverse)
library(forcats)
library(emmeans)
library(ggplot2)

# --- 1) Keep 2021 victims only + set references
data_2021 <- readr::read_csv("../data/combined/data_combined.csv", show_col_types = FALSE) %>%
  filter(violencia_emocional_vivida %in% c(1, TRUE),
         year == 2021) %>%                               # <- 2021 only
  mutate(
    grupo_edad = fct_relevel(grupo_edad, "60-79",
                             "<20","20-29","30-39","40-49","50-59",">80"),
    GRA_bin    = factor(GRA_bin, levels = c(0,1), labels = c("No","Yes")),
    P1_4_9     = factor(P1_4_9, levels = c(2,1), labels = c("No","Yes"))
  ) %>%
  drop_na(violencia_emocional_normalizada, grupo_edad, GRA_bin, P1_4_9)

# --- 2) Logistic model (no year term)
m2021 <- glm(
  violencia_emocional_normalizada ~ grupo_edad*P1_4_9 + GRA_bin*P1_4_9,
  data = data_2021, family = binomial()
)

library(tidyverse)
library(forcats)
library(emmeans)
library(ggplot2)

# ---- modelo 2021 (ya lo tienes) ----
# m2021 <- glm(violencia_emocional_normalizada ~ grupo_edad*P1_4_9 + GRA_bin*P1_4_9,
#              data = data_2021, family = binomial())

# 1) EMMs en 2021 (link)
emm_2021 <- emmeans(m2021, ~ grupo_edad * P1_4_9 * GRA_bin)
grid      <- as.data.frame(emm_2021)[, c("grupo_edad","P1_4_9","GRA_bin")]

# 2) Índice de la referencia (60-79, No, No)
ref_idx <- which(grid$grupo_edad=="60-79" & grid$P1_4_9=="No" & grid$GRA_bin=="No")

# 3) Contrastes "tratamiento vs control" (vs referencia) y CIs robustas
con <- contrast(emm_2021, method = "trt.vs.ctrl", ref = ref_idx)
s   <- as.data.frame(summary(con, infer = TRUE, adjust = "dunnettx"))

lo <- intersect(c("lower.CL","asymp.LCL"), names(s))[1]
hi <- intersect(c("upper.CL","asymp.UCL"), names(s))[1]

# 4) Juntar por posición: quitar la fila de referencia de la grilla y enlazar ORs
or_2021 <- grid[-ref_idx, ] %>%
  mutate(
    grupo_edad = fct_relevel(grupo_edad, "<20","20-29","30-39","40-49","50-59","60-79",">80"),
    Education  = recode(GRA_bin, "No"="Lower education", "Yes"="Higher education"),
    Internet   = recode(P1_4_9,  "No"="No internet",     "Yes"="Internet"),
    Education  = factor(Education, levels = c("Lower education","Higher education")),
    Internet   = factor(Internet,  levels = c("No internet","Internet"))
  ) %>%
  bind_cols(
    s %>% transmute(
      OR     = exp(estimate),
      CI_low = exp(.data[[lo]]),
      CI_high= exp(.data[[hi]])
    )
  ) %>%
  filter(is.finite(OR), is.finite(CI_low), is.finite(CI_high))   # por si acaso

# 5) Forest plot (en inglés), sin facet NA
p <- ggplot(or_2021, aes(y = grupo_edad, x = OR, color = Internet)) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_point(position = position_dodge(width = .45), size = 2) +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high),
                 position = position_dodge(width = .45), height = .18) +
  facet_grid(Education ~ ., scales = "free_y", space = "free_y") +
  scale_x_log10(breaks = c(0.5, 1, 2, 5, 10),
                labels = scales::number_format(accuracy = 0.1)) +
  labs(
    title = "Odds Ratios for Normalised Emotional Violence (2021, victims only)",
    subtitle = "Reference within 2021: Age 60–79, lower education, no internet",
    x = "Odds Ratio (2021)",
    y = "Age group",
    color = "Internet access"
  ) +
  theme_minimal(base_size = 13)

# (Opcional) acotar la vista si hay CIs gigantes:
# p <- p + coord_cartesian(xlim = c(0.4, 6))

print(p)
# ggsave("../results/combined/plots/forest_2021_normalised_victims_only.png",
#        p, width = 11, height = 6.5, dpi = 300)

#==================================================
#)================================================

# --- Paquetes ---
library(tidyverse)
library(forcats)
library(emmeans)
library(ggplot2)

# --- Datos base: SOLO víctimas + factores coherentes ---
data_vict <- readr::read_csv("../data/combined/data_combined.csv", show_col_types = FALSE) %>%
  filter(violencia_emocional_vivida %in% c(1, TRUE)) %>%
  # Armoniza posible "60-80" -> "60-79"
  mutate(grupo_edad = ifelse(grupo_edad == "60-80", "60-79", grupo_edad)) %>%
  mutate(
    year       = factor(year, levels = c(2016, 2021)),
    grupo_edad = fct_relevel(factor(grupo_edad), "60-79",
                             "<20","20-29","30-39","40-49","50-59",">80"),
    GRA_bin    = factor(GRA_bin, levels = c(0,1), labels = c("No","Yes")),
    P1_4_9     = factor(P1_4_9, levels = c(2,1), labels = c("No","Yes"))
  )


gap_analysis <- function(outcome_var,
                         young_level = "20-29",
                         old_level   = "60-79",
                         out_prefix  = NULL,
                         data = data_vict) {
  if (is.null(out_prefix)) out_prefix <- outcome_var
  
  # 1) Filtrar NAs del outcome y asegurar niveles/etiquetas
  d <- data %>%
    tidyr::drop_na(!!rlang::sym(outcome_var), year, grupo_edad, GRA_bin, P1_4_9) %>%
    dplyr::mutate(
      grupo_edad = forcats::fct_relevel(grupo_edad,
                                        "<20","20-29","30-39","40-49","50-59","60-79",">80"),
      GRA_bin = forcats::fct_relevel(GRA_bin, "No","Yes"),
      P1_4_9  = forcats::fct_relevel(P1_4_9,  "No","Yes")
    )
  
  # 2) Modelo logístico con interacciones temporales (como vienes usando)
  fml <- stats::as.formula(
    paste0(outcome_var, " ~ year * (grupo_edad + P1_4_9 + GRA_bin) + ",
           "grupo_edad * P1_4_9 + GRA_bin * P1_4_9")
  )
  m <- glm(fml, data = d, family = binomial())
  
  # 3) EMMs en un grid con YEAR explícito como factor (no 'by')
  emm_all      <- emmeans::emmeans(m, ~ year * grupo_edad * P1_4_9 * GRA_bin)
  emm_all_resp <- emmeans::regrid(emm_all, transform = "response")
  
  # 4) Tomar SOLO los dos perfiles (Priv y Disadv) en ambos años
  emm_four <- subset(
    emm_all_resp,
    (grupo_edad == young_level & P1_4_9 == "Yes" & GRA_bin == "Yes") |
      (grupo_edad == old_level   & P1_4_9 == "No"  & GRA_bin == "No")
  )
  
  df_four <- as.data.frame(emm_four)
  
  # Sanity check: deben ser 4 filas (2016-Disadv, 2016-Priv, 2021-Disadv, 2021-Priv)
  if (nrow(df_four) < 4) {
    message("Faltan combinaciones. Esto suele pasar por N muy pequeños en el perfil Priv.")
    print(df_four)
    stop("No hay las 4 combinaciones necesarias para construir el gap y su cambio.")
  }
  
  # 5) Ordenar filas: dentro de cada year, primero Disadv (No/No/old), luego Priv (Yes/Yes/young)
  df_four <- df_four %>%
    dplyr::mutate(
      is_priv = (grupo_edad == young_level & P1_4_9 == "Yes" & GRA_bin == "Yes")
    ) %>%
    dplyr::arrange(year, is_priv)
  
  # Reordenar el emmGrid según ese orden (indice sobre las filas actuales)
  ord <- match(
    paste(df_four$year, df_four$grupo_edad, df_four$P1_4_9, df_four$GRA_bin),
    paste(as.data.frame(emm_four)$year,
          as.data.frame(emm_four)$grupo_edad,
          as.data.frame(emm_four)$P1_4_9,
          as.data.frame(emm_four)$GRA_bin)
  )
  emm_four_sorted <- emm_four[ord]
  
  # 6) Definir contrastes lineales en escala de respuesta:
  # Orden esperado ahora: [2016 Disadv, 2016 Priv, 2021 Disadv, 2021 Priv]
  C <- list(
    "Gap 2016 (Priv − Disadv)" = c(-1, +1,  0,  0),
    "Gap 2021 (Priv − Disadv)" = c( 0,  0, -1, +1),
    "Δ gap (2021−2016)"        = c(+1, -1, -1, +1)
  )
  
  contr <- emmeans::contrast(emm_four_sorted, method = C)
  sumc  <- as.data.frame(base::summary(contr, infer = TRUE))
  
  # Columnas robustas de IC (cambian según versión)
  lo <- if ("lower.CL" %in% names(sumc)) "lower.CL" else "asymp.LCL"
  hi <- if ("upper.CL" %in% names(sumc)) "upper.CL" else "asymp.UCL"
  
  # 7) Partir en (a) gaps por año y (b) cambio del gap
  gap_by_year_df <- sumc %>%
    dplyr::filter(grepl("^Gap\\s20", contrast)) %>%
    dplyr::mutate(
      year = ifelse(grepl("2016", contrast), "2016", "2021")
    ) %>%
    dplyr::transmute(
      year = factor(year, levels = c("2016","2021")),
      gap_pp   = 100 * estimate,
      lower_pp = 100 * .data[[lo]],
      upper_pp = 100 * .data[[hi]],
      p.value
    )
  
  gap_change_df <- sumc %>%
    dplyr::filter(grepl("Δ gap", contrast)) %>%
    dplyr::transmute(
      contrast,
      delta_gap_pp = 100 * estimate,
      lower_pp     = 100 * .data[[lo]],
      upper_pp     = 100 * .data[[hi]],
      p.value
    )
  
  # 8) Gráfico (en inglés)
  p_gap <- ggplot2::ggplot(gap_by_year_df,
                           ggplot2::aes(x = year, y = gap_pp, fill = year)) +
    ggplot2::geom_col(width = 0.55) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lower_pp, ymax = upper_pp), width = 0.15) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::labs(
      title    = "Privileged vs Disadvantaged gap in adjusted probability",
      subtitle = paste0(
        "Outcome: ", outcome_var,
        " | Priv: ", young_level, ", Internet=Yes, Higher ed=Yes; ",
        "Disadv: ", old_level, ", No internet, Lower ed"
      ),
      x = "Year", y = "Gap (percentage points)", fill = "Year"
    ) +
    ggplot2::theme_minimal(base_size = 12)
  
  # 9) Guardar
  dir.create("../results/combined/plots", recursive = TRUE, showWarnings = FALSE)
  dir.create("../results/combined/tables", recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(paste0("../results/combined/plots/gap_", out_prefix, ".png"),
                  p_gap, width = 8, height = 5, dpi = 300)
  readr::write_csv(gap_by_year_df, paste0("../results/combined/tables/gap_by_year_", out_prefix, ".csv"))
  readr::write_csv(gap_change_df,  paste0("../results/combined/tables/gap_change_",  out_prefix, ".csv"))
  
  list(
    model = m,
    contrasts = sumc,
    gap_by_year = gap_by_year_df,
    gap_change  = gap_change_df,
    plot = p_gap
  )
}
# 1) Reconocida
res_rec <- gap_analysis(
  "violencia_emocional_reconocida",
  young_level = "20-29", old_level = "60-79",
  out_prefix = "recognised"
)

# 2) Normalizada
res_norm <- gap_analysis(
  "violencia_emocional_normalizada",
  young_level = "20-29", old_level = "60-79",
  out_prefix = "normalized"
)

library(emmeans)
library(dplyr)
library(tidyr)
library(readr)

# Cambia entre res_rec$model y res_norm$model según el outcome
mdl <- res_norm$model   # o: mdl <- res_rec$model

emm_link <- emmeans(mdl, ~ year | grupo_edad + P1_4_9 + GRA_bin)
emm_resp <- regrid(emm_link, transform = "response")

# df con las emmeans en escala de probabilidad
df <- as.data.frame(emm_resp)

# Detecta el nombre correcto de la columna de probabilidad
val_col <- intersect(c("response","prob","emmean",".response"), names(df))[1]
if (is.na(val_col)) {
  # fallback por si tu versión usa otro nombre numérico
  cand <- names(df)[sapply(df, is.numeric)]
  cand <- setdiff(cand, c("SE","df","z.ratio","t.ratio","p.value",
                          "lower.CL","upper.CL","asymp.LCL","asymp.UCL"))
  val_col <- cand[1]
}

probs <- df %>%
  dplyr::filter(
    (grupo_edad=="20-29" & P1_4_9=="Yes" & GRA_bin=="Yes") |
      (grupo_edad=="60-79" & P1_4_9=="No"  & GRA_bin=="No")
  ) %>%
  dplyr::mutate(profile = dplyr::if_else(
    grupo_edad=="20-29" & P1_4_9=="Yes" & GRA_bin=="Yes",
    "Privileged","Disadvantaged"
  )) %>%
  dplyr::transmute(year, profile, prob = .data[[val_col]]) %>%
  tidyr::pivot_wider(names_from = profile, values_from = prob) %>%
  dplyr::mutate(gap_pp = 100*(Privileged - Disadvantaged))

probs


m_rec <- glm(
  violencia_emocional_reconocida ~ year * (grupo_edad + P1_4_9 + GRA_bin) +
    grupo_edad * P1_4_9 + GRA_bin * P1_4_9,
  data = data_vict, family = binomial()
)

emm_rec <- emmeans(m_rec, ~ year | grupo_edad + P1_4_9 + GRA_bin)
emm_rec_resp <- regrid(emm_rec, transform = "response")

df <- as.data.frame(emm_rec_resp)
val <- intersect(c("response","prob","emmean",".response"), names(df))[1]

probs_rec <- df %>%
  filter((grupo_edad=="20-29" & P1_4_9=="Yes" & GRA_bin=="Yes") |
           (grupo_edad=="60-79" & P1_4_9=="No"  & GRA_bin=="No")) %>%
  mutate(profile = if_else(grupo_edad=="20-29" & P1_4_9=="Yes" & GRA_bin=="Yes",
                           "Privileged","Disadvantaged")) %>%
  select(year, profile, prob = .data[[val]]) %>%
  pivot_wider(names_from = profile, values_from = prob) %>%
  mutate(gap_pp = 100*(Privileged - Disadvantaged))

print(probs_rec)

# Confirmar datos
data_vict %>%
  summarise(
    mean_rec = mean(violencia_emocional_reconocida, na.rm=TRUE),
    mean_norm = mean(violencia_emocional_normalizada, na.rm=TRUE)
  )

# Por subgrupos
data_vict %>%
  filter(grupo_edad %in% c("20-29","60-79"),
         GRA_bin %in% c("No","Yes"),
         P1_4_9 %in% c("No","Yes")) %>%
  group_by(year, grupo_edad, GRA_bin, P1_4_9) %>%
  summarise(p_rec = mean(violencia_emocional_reconocida, na.rm=TRUE), .groups="drop")

# Por Intervalos
m_rec <- glm(
  violencia_emocional_reconocida ~ year * (grupo_edad + P1_4_9 + GRA_bin) +
    grupo_edad * P1_4_9 + GRA_bin * P1_4_9,
  data = data_vict, family = binomial()
)

emm_rec <- emmeans(m_rec, ~ year | grupo_edad + P1_4_9 + GRA_bin)
emm_rec_resp <- regrid(emm_rec, transform = "response")
two <- subset(emm_rec_resp,
              (grupo_edad=="20-29" & P1_4_9=="Yes" & GRA_bin=="Yes") |
                (grupo_edad=="60-79" & P1_4_9=="No"  & GRA_bin=="No")
)

gap_y <- contrast(two, method = list("Priv − Disadv" = c(1, -1)), by = "year")
summary(gap_y, infer = TRUE)          # gap por año con IC95%

contrast(gap_y, method = list("2021 − 2016" = c(-1, +1))) |>
  summary(infer = TRUE)               # cambio del gap con p-valor


# --- Packages ---
library(tidyverse)
library(forcats)
library(emmeans)

# --- Data: victims only + clean factors ---
data_vict <- readr::read_csv("../data/combined/data_combined.csv", show_col_types = FALSE) %>%
  filter(violencia_emocional_vivida %in% c(1, TRUE)) %>%
  mutate(grupo_edad = ifelse(grupo_edad == "60-80", "60-79", grupo_edad)) %>%
  mutate(
    year       = factor(year, levels = c(2016, 2021)),
    grupo_edad = fct_relevel(factor(grupo_edad), "60-79",
                             "<20","20-29","30-39","40-49","50-59",">80"),
    GRA_bin    = factor(GRA_bin, levels = c(0,1), labels = c("No","Yes")),
    P1_4_9     = factor(P1_4_9, levels = c(2,1), labels = c("No","Yes"))
  )

# --- Model (same spec you’ve been using) ---
d <- data_vict %>%
  tidyr::drop_na(violencia_emocional_reconocida, year, grupo_edad, GRA_bin, P1_4_9)

m <- glm(
  violencia_emocional_reconocida ~ year * (grupo_edad + P1_4_9 + GRA_bin) +
    grupo_edad * P1_4_9 + GRA_bin * P1_4_9,
  data = d, family = binomial()
)

# --- Marginal means on the response scale ---
emm_link <- emmeans(m, ~ year | grupo_edad + P1_4_9 + GRA_bin)
emm_resp <- regrid(emm_link, transform = "response")

# --- Keep only the two profiles:
# Privileged = 20-29, Internet=Yes, Higher ed=Yes
# Disadvantaged = 60-79, Internet=No, Higher ed=No
emm_two <- subset(
  emm_resp,
  (grupo_edad == "20-29" & P1_4_9 == "Yes" & GRA_bin == "Yes") |
    (grupo_edad == "60-79" & P1_4_9 == "No"  & GRA_bin == "No")
)

# --- GAP per year (Priv − Disadv) with 95% CI ---
gap_y <- contrast(emm_two, method = list("Priv − Disadv" = c(1, -1)), by = "year")
gdf <- as.data.frame(summary(gap_y, infer = TRUE))

# Be robust to column-name differences across emmeans versions
effect_col <- intersect(c("estimate","response","prob","emmean",".response"), names(gdf))[1]
lo_col <- if ("lower.CL" %in% names(gdf)) "lower.CL" else "asymp.LCL"
hi_col <- if ("upper.CL" %in% names(gdf)) "upper.CL" else "asymp.UCL"

gap_by_year <- gdf %>%
  transmute(
    year,
    gap_pp   = 100 * .data[[effect_col]],
    lower_pp = 100 * .data[[lo_col]],
    upper_pp = 100 * .data[[hi_col]],
    p.value
  )

print(gap_by_year)

# --- Optional: Ns for each profile per year (useful to report)
Ns <- d %>%
  filter((grupo_edad=="20-29" & P1_4_9=="Yes" & GRA_bin=="Yes") |
           (grupo_edad=="60-79" & P1_4_9=="No"  & GRA_bin=="No")) %>%
  mutate(profile = if_else(GRA_bin=="Yes" & P1_4_9=="Yes" & grupo_edad=="20-29",
                           "Privileged","Disadvantaged")) %>%
  count(year, profile) %>%
  tidyr::pivot_wider(names_from = profile, values_from = n, values_fill = 0)
print(Ns)

# --- Plot (bar chart with 95% CI) ---
p_gap <- ggplot(gap_by_year, aes(x = factor(year), y = gap_pp, fill = factor(year))) +
  geom_col(width = 0.55) +
  geom_errorbar(aes(ymin = lower_pp, ymax = upper_pp), width = 0.15) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(aes(label = sprintf("%.1f pp", gap_pp)), vjust = -0.4, size = 3.5) +
  labs(
    title = "Privileged vs. Disadvantaged Gap in Adjusted Probability",
    subtitle = "Outcome: Recognising emotional violence\nPrivileged = 20–29, Internet=Yes, Higher ed=Yes; Disadvantaged = 60–79, No internet, Lower ed",
    x = "Year", y = "Gap (percentage points)", fill = "Year"
  ) +
  coord_cartesian(ylim = c(min(gap_by_year$lower_pp) - 2, max(gap_by_year$upper_pp) + 4)) +
  theme_minimal(base_size = 12)

print(p_gap)

# Save
dir.create("../results/combined/plots", recursive = TRUE, showWarnings = FALSE)
ggsave("../results/combined/plots/gap_recognise.png", p_gap, width = 8, height = 5, dpi = 300)

# Opción 1: atajo "revpairwise" (hace 2021 − 2016)
gap_change <- contrast(gap_y, "revpairwise", by = NULL)
summary(gap_change, infer = TRUE)

out <- as.data.frame(summary(gap_change, infer = TRUE))
delta_pp <- 100 * out$estimate
ci_low   <- 100 * (out$lower.CL %||% out$asymp.LCL)
ci_high  <- 100 * (out$upper.CL %||% out$asymp.UCL)
pval     <- out$p.value
c(delta_pp, ci_low, ci_high, pval)

# 1) Mira el orden real en cada año
as.data.frame(emm_two) %>%
  dplyr::select(year, grupo_edad, P1_4_9, GRA_bin) %>%
  dplyr::arrange(year, grupo_edad, P1_4_9, GRA_bin)

# Gap = Priv − Disadv (ajustado al orden real: [Desfav, Priv])
gap_y <- contrast(emm_two, method = list("Priv − Disadv" = c(-1, 1)), by = "year")
gdf <- as.data.frame(summary(gap_y, infer = TRUE))
effect_col <- intersect(c("estimate","response","prob","emmean",".response"), names(gdf))[1]
lo_col <- if ("lower.CL" %in% names(gdf)) "lower.CL" else "asymp.LCL"
hi_col <- if ("upper.CL" %in% names(gdf)) "upper.CL" else "asymp.UCL"

gap_by_year <- gdf %>%
  dplyr::transmute(
    year,
    gap_pp   = 100 * .data[[effect_col]],
    lower_pp = 100 * .data[[lo_col]],
    upper_pp = 100 * .data[[hi_col]],
    p.value
  )
print(gap_by_year)

library(dplyr)
library(tidyr)

d %>%
  count(year, grupo_edad, P1_4_9, GRA_bin, name = "N") %>%
  arrange(year, grupo_edad, P1_4_9, GRA_bin)

print(gap_by_year, preview = "docx",
      path = "../results/combined/tables/appendix_gap_by_year.docx")
gap_by_year

###===========================
# REGRESION MODEL
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

data<- read_rds("../data/combined/data_combined.rds") 

