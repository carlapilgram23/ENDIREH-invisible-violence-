# =====================================================
# Logistic regressions: recognition & normalisation
# Clean labels, OR table (DOCX), forest plot, ANOVA/AIC
# =====================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(forcats)
  library(broom)
  library(ggplot2)
  library(flextable)
  library(officer)
})

# ---------- Paths ----------
out_plot_dir  <- "../results/combined/plots"
out_table_dir <- "../results/combined/tables"
dir.create(out_plot_dir,  recursive = TRUE, showWarnings = FALSE)
dir.create(out_table_dir, recursive = TRUE, showWarnings = FALSE)

# ---------- Data ----------
data_vict <- read_rds("../data/combined/data_combined.rds") %>%
  filter(violencia_emocional_vivida %in% c(1, TRUE)) %>%
  mutate(
    # evitar contrastes polinomiales
    grupo_edad = factor(as.character(grupo_edad), ordered = FALSE),
    # armoniza y ordena niveles de edad
    grupo_edad = fct_relevel(grupo_edad, "60-79","<20","20-29","30-39","40-49","50-59",">80"),
    # factores
    year       = factor(year, levels = c(2016, 2021)),
    GRA_bin    = factor(GRA_bin, levels = c(0,1), labels = c("No","Yes")),
    P1_4_9     = factor(P1_4_9, levels = c(2,1), labels = c("No","Yes"))
  ) %>%
  drop_na(violencia_emocional_reconocida, year, grupo_edad, GRA_bin, P1_4_9)

# ---------- Models ----------
m_rec <- glm(
  violencia_emocional_reconocida ~ year + grupo_edad * P1_4_9 + GRA_bin * P1_4_9,
  data = data_vict, family = binomial()
)

m_norm <- glm(
  violencia_emocional_normalizada ~ year + grupo_edad * P1_4_9 + GRA_bin * P1_4_9,
  data = data_vict, family = binomial()
)

# ---------- Pretty labels (single, complete dict) ----------
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

# helper para ver qué términos no fueron mapeados
check_unmapped <- function(model, dict) {
  terms_raw <- unique(broom::tidy(model)$term)
  setdiff(terms_raw, names(dict))
}

# ---------- OR tables (Recognised & Normalised) ----------
tidy_or <- function(modelo, nombre_modelo) {
  broom::tidy(modelo, conf.int = TRUE, exponentiate = TRUE) %>%
    filter(term != "(Intercept)") %>%
    transmute(
      Model = nombre_modelo,
      Variable = term,
      OR = round(estimate, 2),
      `CI low`  = round(conf.low, 2),
      `CI high` = round(conf.high, 2),
      p = p.value
    ) %>%
    mutate(
      `p-value` = case_when(
        is.na(p)   ~ NA_character_,
        p < 0.0001 ~ "<0.0001",
        p < 0.01   ~ sprintf("%.4f", p),
        TRUE       ~ sprintf("%.2f", p)
      ),
      Variable = dplyr::recode(Variable, !!!pretty_terms, .default = Variable)
    ) %>%
    select(Model, Variable, OR, `CI low`, `CI high`, `p-value`)
}

res_or <- bind_rows(
  tidy_or(m_rec, "Recognised"),
  tidy_or(m_norm, "Normalised")
)

# imprime en consola y salva DOCX
print(res_or)

ft_or <- flextable(res_or) %>%
  set_caption("Logistic regressions: odds ratios (95% CI) and p-values") %>%
  theme_booktabs() %>%
  fontsize(size = 10, part = "all") %>%
  align(align = "center", part = "all") %>%
  autofit()

save_as_docx("Regression results" = ft_or,
             path = file.path(out_table_dir, "regression_results.docx"))

# ---------- Forest plot for recognition ----------
# orden de términos (en crudo) para posicionar
term_order <- c(
  "year2021","GRA_binYes","P1_4_9Yes",
  "grupo_edad<20","grupo_edad20-29","grupo_edad30-39","grupo_edad40-49","grupo_edad50-59","grupo_edad>80",
  "grupo_edad<20:P1_4_9Yes","grupo_edad20-29:P1_4_9Yes","grupo_edad30-39:P1_4_9Yes",
  "grupo_edad40-49:P1_4_9Yes","grupo_edad50-59:P1_4_9Yes","grupo_edad>80:P1_4_9Yes",
  "P1_4_9Yes:GRA_binYes"
)

or_df <- broom::tidy(m_rec, exponentiate = TRUE, conf.int = TRUE, conf.method = "wald") %>%
  filter(term != "(Intercept)") %>%
  mutate(
    label_nice = dplyr::recode(term, !!!pretty_terms, .default = term),
    label_nice = factor(label_nice, levels = dplyr::recode(term_order, !!!pretty_terms)),
    OR = estimate
  )

# avisa si hay términos sin mapear
umap <- check_unmapped(m_rec, pretty_terms)
if (length(umap) > 0) {
  message("Unmapped terms (add to pretty_terms if needed): ", paste(umap, collapse = ", "))
}

p_forest <- ggplot(or_df, aes(x = OR, y = label_nice)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = .18) +
  labs(
    title = "Odds Ratios for Recognising Emotional Violence",
    subtitle = "Reference: Age 60–79, lower education, no internet, year 2016",
    x = "Odds Ratio (95% CI)", y = NULL
  ) +
  theme_minimal(base_size = 12)

ggsave(file.path(out_plot_dir, "forest_or_recognised.png"),
       p_forest, width = 8.5, height = 6, dpi = 300)

# ---------- Model comparison: ANOVA & AIC ----------
model_base <- glm(
  violencia_emocional_reconocida ~ year + grupo_edad * P1_4_9 + GRA_bin * P1_4_9,
  data = data_vict, family = binomial()
)

model_timeint <- glm(
  violencia_emocional_reconocida ~ year * (grupo_edad + P1_4_9 + GRA_bin) +
    grupo_edad * P1_4_9 + GRA_bin * P1_4_9,
  data = data_vict, family = binomial()
)

anova_tab <- anova(model_base, model_timeint, test = "Chisq") %>% as.data.frame()
aic_tab   <- AIC(model_base, model_timeint) %>% as.data.frame()

write_csv(anova_tab, file.path(out_table_dir, "model_anova_recognised.csv"))
write_csv(aic_tab,   file.path(out_table_dir, "model_aic_recognised.csv"))

print(anova_tab)
print(aic_tab)

message("Done ✓  Outputs in: ", normalizePath(out_table_dir), " and ", normalizePath(out_plot_dir))
