
# ==========================================
# Difference in differences  Emotional violence recognition
# Mexico City vs other states  pre vs post
# Figures in English  No physical violence variables used
# ==========================================

# Libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
  library(stringr)
  library(ggplot2)
  library(scales)
  library(broom)
  library(purrr)
})

# Paths
out_plot_dir  <- "../results/combined/plots"
out_table_dir <- "../results/combined/tables"
dir.create(out_plot_dir,  recursive = TRUE, showWarnings = FALSE)
dir.create(out_table_dir, recursive = TRUE, showWarnings = FALSE)

# Load data
data <- read_csv("../data/combined/data_combined.csv", show_col_types = FALSE) %>%
  mutate(
    treated = as.integer(NOM_ENT == "CIUDAD DE MEXICO"),
    post    = as.integer(periodo == "post")
  )

# ==========================================
# 1) Main DiD  outcome: emotional recognition among those who experienced it
# ==========================================
d1 <- data %>%
  filter(violencia_emocional_vivida == TRUE,
         !is.na(violencia_emocional_reconocida))

m1 <- feols(
  violencia_emocional_reconocida ~ i(post, treated, ref = 0) | NOM_ENT + year,
  data = d1, cluster = ~NOM_ENT
)
print(summary(m1))

# Extract ATT and CI safely for caption
att_name <- names(coef(m1))[str_detect(names(coef(m1)), "post.*treated")]
att_est  <- unname(coef(m1)[att_name])

ci_mat   <- confint(m1)                        # matrix rows = coefs
att_low  <- unname(ci_mat[att_name, 1])
att_high <- unname(ci_mat[att_name, 2])

# Means for plot with binomial SE
g_means <- d1 %>%
  group_by(periodo, treated) %>%
  summarise(p = mean(violencia_emocional_reconocida), n = n(), .groups = "drop") %>%
  mutate(
    group   = if_else(treated == 1, "Mexico City", "Other states"),
    periodo = factor(periodo, levels = c("pre","post")),
    se      = sqrt(p * (1 - p) / n)
  )

p1 <- ggplot(g_means, aes(x = periodo, y = p, color = group, group = group)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = p - 1.96 * se, ymax = p + 1.96 * se), width = 0.1) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title    = "Difference in differences: Emotional violence recognition",
    subtitle = "Women who experienced emotional violence, Mexico City vs other states",
    x = "Period",
    y = "Share recognising emotional violence",
    color = "Group",
    caption = paste0(
      "ATT = ", percent(att_est, accuracy = 0.1),
      " [",  percent(att_low, accuracy = 0.1), ", ",
      percent(att_high, accuracy = 0.1), "]"
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave(file.path(out_plot_dir, "did_emotional_recognition.png"),
       p1, width = 8, height = 5, dpi = 300)

# ==========================================
# 2) Robustness A  incidence of emotional violence  all women
#    shows the DiD does not necessarily mean violence fell
# ==========================================
d_inc <- data %>% filter(!is.na(violencia_emocional_vivida))

m_inc <- feols(
  violencia_emocional_vivida ~ i(post, treated, ref = 0) | NOM_ENT + year,
  data = d_inc, cluster = ~NOM_ENT
)
print(summary(m_inc))

g_inc <- d_inc %>%
  group_by(periodo, treated) %>%
  summarise(p = mean(violencia_emocional_vivida), n = n(), .groups = "drop") %>%
  mutate(
    group   = if_else(treated == 1, "Mexico City", "Other states"),
    periodo = factor(periodo, levels = c("pre","post")),
    se      = sqrt(p * (1 - p) / n)
  )

p2 <- ggplot(g_inc, aes(x = periodo, y = p, color = group, group = group)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = p - 1.96 * se, ymax = p + 1.96 * se), width = 0.1) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Incidence of emotional violence",
    subtitle = "All women, Mexico City vs other states",
    x = "Period",
    y = "Share experiencing emotional violence",
    color = "Group"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave(file.path(out_plot_dir, "did_emotional_incidence.png"),
       p2, width = 8, height = 5, dpi = 300)

# ==========================================
# 3) Robustness B  falsification with pseudo treated states
#    skips states without variation and handles errors safely
# ==========================================
states_test <- c("JALISCO","NUEVO LEON","PUEBLA","GUANAJUATO","VERACRUZ DE IGNACIO DE LA LLAVE")

run_pseudo <- function(st) {
  d_tmp <- d1 %>%
    mutate(
      treated_pseudo = as.integer(NOM_ENT == st),
      post_treat     = post * treated_pseudo
    )
  
  # Check that pseudo treated has both periods
  has_two_periods <- d_tmp %>%
    filter(treated_pseudo == 1) %>%
    distinct(year) %>% nrow() == 2
  
  if (!has_two_periods) {
    return(tibble(state = st, estimate = NA_real_, std.error = NA_real_,
                  statistic = NA_real_, p.value = NA_real_,
                  note = "skipped no pre or post in pseudo treated"))
  }
  
  m <- feols(
    violencia_emocional_reconocida ~ post_treat | NOM_ENT + year,
    data = d_tmp, cluster = ~NOM_ENT
  )
  
  est <- coef(m)["post_treat"]
  se  <- sqrt(vcov(m)["post_treat","post_treat"])
  tibble(
    state = st,
    estimate = est,
    std.error = se,
    statistic = est / se,
    p.value = 2 * pnorm(-abs(est / se)),
    note = "ok"
  )
}

safe_run <- safely(run_pseudo)
res_list <- map(states_test, safe_run)

res_pseudo <- map_dfr(res_list, function(x) {
  if (!is.null(x$result)) return(x$result)
  tibble(state = NA_character_, estimate = NA_real_, std.error = NA_real_,
         statistic = NA_real_, p.value = NA_real_,
         note = paste("error", x$error$message))
})

print(res_pseudo)
write_csv(res_pseudo, file.path(out_table_dir, "pseudo_treated_results.csv"))

# Optional quick plot for pseudo treated results with 95% CI
res_plot <- res_pseudo %>%
  filter(!is.na(state), note == "ok") %>%
  mutate(
    low = estimate - 1.96 * std.error,
    high = estimate + 1.96 * std.error
  )

if (nrow(res_plot) > 0) {
  p3 <- ggplot(res_plot, aes(y = reorder(state, estimate), x = estimate)) +
    geom_point() +
    geom_errorbarh(aes(xmin = low, xmax = high), height = 0.2) +
    geom_vline(xintercept = 0, linetype = "dotted") +
    labs(
      title = "Placebo DiD with pseudo treated states",
      subtitle = "Effect on emotional violence recognition",
      x = "ATT estimate",
      y = "State"
    ) +
    theme_minimal(base_size = 12)
  ggsave(file.path(out_plot_dir, "pseudo_treated_falsification.png"),
         p3, width = 7, height = 4.5, dpi = 300)
}

# ==========================================
# 4) Optional DDD by digital access and by age exposure
#    runs only if columns exist
# ==========================================
# a) Digital access
if ("P1_4_9" %in% names(d1)) {
  d_ddd_dig <- d1 %>% filter(!is.na(P1_4_9))
  if (nrow(d_ddd_dig) > 0) {
    m_ddd_digital <- feols(
      violencia_emocional_reconocida ~ post * treated * P1_4_9 | NOM_ENT + year,
      data = d_ddd_dig, cluster = ~NOM_ENT
    )
    print(summary(m_ddd_digital))
  }
}

# b) Age based exposure
if ("grupo_edad" %in% names(d1)) {
  d1 <- d1 %>% mutate(joven = as.integer(grupo_edad %in% c("<20","20-29","30-39")))
  if (!all(is.na(d1$joven))) {
    m_ddd_age <- feols(
      violencia_emocional_reconocida ~ post * treated * joven | NOM_ENT + year,
      data = d1, cluster = ~NOM_ENT
    )
    print(summary(m_ddd_age))
  }
}

# ==========================================
# End
# ==========================================

#Anexos
# ===== Tables to Word for annexes =====
#install.packages("modelsummary")
library(modelsummary)
library(flextable)
library(officer)
library(stringr)
library(dplyr)
library(scales)

dir.create("../results/combined/tables", recursive = TRUE, showWarnings = FALSE)

# 1) Annex table: Main DiD and Incidence DiD side by side
# Find the ATT names in each model
att1 <- names(coef(m1))[stringr::str_detect(names(coef(m1)), "post.*treated")]
att2 <- names(coef(m_inc))[stringr::str_detect(names(coef(m_inc)), "post.*treated")]
# Map both to the same readable label
coef_map <- setNames("ATT post × Mexico City", unique(c(att1, att2)))

# === Annex table: DiD with 95% CIs ===
ft_models <- modelsummary(
  list("Emotional recognition DiD" = m1,
       "Emotional incidence DiD"  = m_inc),
  output     = "flextable",
  coef_map   = coef_map,
  confint    = TRUE,                  # <- correcto (sin guion bajo)
  conf_level = 0.95,                  # <- nivel del IC
  estimate   = "{estimate}",          # muestra el coeficiente
  statistic  = "[{conf.low}, {conf.high}]",  # y en 'statistic' los IC
  gof_omit   = "IC|Log|Adj|AIC|BIC|RMSE|Within|Pseudo|Deviance"
) |>
  autofit() |>
  add_header_lines(values = "Annex. Difference in differences results") |>
  footnote(
    i = 1, j = 1,
    value = as_paragraph("Notes: Standard errors clustered by state. Models include state and year fixed effects."),
    ref_symbols = c("a")
  )

save_as_docx("DiD models" = ft_models,
             path = "../results/combined/tables/annex_did_models.docx")


# 2) Annex table: Pseudo treated falsification results
# If you already created res_pseudo with estimate and std.error
res_fmt <- res_pseudo |>
  mutate(
    ATT = percent(estimate, accuracy = 0.1),
    `Std. error` = round(std.error, 3),
    `95% low`  = percent(estimate - 1.96*std.error, accuracy = 0.1),
    `95% high` = percent(estimate + 1.96*std.error, accuracy = 0.1)
  ) |>
  select(state, ATT, `Std. error`, `95% low`, `95% high`, p.value, note)

ft_pseudo <- flextable(res_fmt) |>
  autofit() |>
  set_caption("Annex. Placebo DiD with pseudo treated states")

save_as_docx("Pseudo treated placebo" = ft_pseudo,
             path = "../results/combined/tables/annex_pseudo_treated.docx")

