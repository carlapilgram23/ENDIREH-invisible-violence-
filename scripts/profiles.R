# ================================
# Reconocimiento: 2016 % / 2021 % / Δ pp (IC95%) / BH p-value
# ================================

# Paquetes
library(tidyverse)
library(forcats)
library(readr)
library(emmeans)
library(broom)
library(flextable)
library(officer)

# ---------- 1) Cargar y preparar datos (víctimas) ----------
data_vict <- read_rds("../data/combined/data_combined.rds") %>%
  filter(violencia_emocional_vivida %in% c(1, TRUE)) %>%
  mutate(
    # Armoniza niveles de edad
    grupo_edad = if ("60-80" %in% levels(grupo_edad)) fct_recode(grupo_edad, "60-79" = "60-80") else grupo_edad,
    grupo_edad = fct_relevel(grupo_edad, "<20","20-29","30-39","40-49","50-59","60-79",">80"),
    # Factores clave
    year       = factor(year, levels = c(2016, 2021)),
    GRA_bin    = factor(GRA_bin, levels = c(0,1), labels = c("Lower education","Higher education")),
    P1_4_9     = factor(P1_4_9, levels = c(2,1), labels = c("No internet","Internet"))
  ) %>%
  drop_na(violencia_emocional_reconocida, year, grupo_edad, GRA_bin, P1_4_9)

# ---------- 2) Modelo logístico ----------
m <- glm(
  violencia_emocional_reconocida ~ year * (grupo_edad + GRA_bin + P1_4_9),
  data = data_vict, family = binomial()
)

# ---------- 3) Probabilidades ajustadas por año y perfil ----------
emm_link <- emmeans(m, ~ year | grupo_edad + GRA_bin + P1_4_9)
emm_resp <- regrid(emm_link, transform = "response")  # escala prob.

# summary() puede nombrar las columnas distinto según versión; detectamos robustamente
s <- summary(emm_resp, infer = TRUE) %>% as_tibble()
nm <- names(s)
val_col <- intersect(c("response","prob","emmean",".response","estimate"), nm)[1]
lo_col  <- intersect(c("lower.CL","asymp.LCL","LCL"), nm)[1]
hi_col  <- intersect(c("upper.CL","asymp.UCL","UCL"), nm)[1]
stopifnot(!any(is.na(c(val_col, lo_col, hi_col))))

# Renombrar a nombres uniformes y convertir a numérico
preds <- s %>%
  rename(
    Education  = GRA_bin,
    Internet   = P1_4_9,
    `Age group`= grupo_edad,
    Year       = year
  ) %>%
  mutate(
    prob = as.numeric(.data[[val_col]]),
    lo   = as.numeric(.data[[lo_col]]),
    hi   = as.numeric(.data[[hi_col]])
  )

# Tabla ancha: "2016 % (95% CI)" y "2021 % (95% CI)"
preds_fmt <- preds %>%
  mutate(
    pct  = 100 * prob,
    lo_pp = 100 * lo,
    hi_pp = 100 * hi,
    label = sprintf("%.1f (%.1f–%.1f)", pct, lo_pp, hi_pp)
  ) %>%
  select(Education, Internet, `Age group`, Year, label) %>%
  tidyr::pivot_wider(names_from = Year, values_from = label)

# ---------- 4) Δ p.p. (2021 − 2016) por perfil + BH p-value ----------
# Contraste explícito 2021 - 2016
diffs_raw <- contrast(
  emm_resp,
  method = list("delta_2021_2016" = c(-1, +1)),
  by = c("grupo_edad","GRA_bin","P1_4_9")
) %>%
  summary(infer = TRUE, adjust = "none") %>%
  as_tibble()

# Detectar nombres y uniformar
nm2  <- names(diffs_raw)
val2 <- intersect(c("estimate","response","prob","emmean",".response"), nm2)[1]
lo2  <- intersect(c("lower.CL","asymp.LCL","LCL"), nm2)[1]
hi2  <- intersect(c("upper.CL","asymp.UCL","UCL"), nm2)[1]
stopifnot(!any(is.na(c(val2, lo2, hi2))))

diffs <- diffs_raw %>%
  rename(
    Education   = GRA_bin,
    Internet    = P1_4_9,
    `Age group` = grupo_edad
  ) %>%
  mutate(
    diff_num = as.numeric(.data[[val2]]),
    lo_num   = as.numeric(.data[[lo2]]),
    hi_num   = as.numeric(.data[[hi2]])
  ) %>%
  mutate(
    `Δ pp (95% CI)` = sprintf("%.1f (%.1f, %.1f)", 100*diff_num, 100*lo_num, 100*hi_num),
    p_BH = p.adjust(p.value, method = "BH")
  ) %>%
  mutate(
    `BH p-value` = case_when(
      is.na(p_BH)           ~ NA_character_,
      p_BH < 0.001          ~ "p<0.001",
      p_BH < 0.01           ~ sprintf("p=%.2f", round(p_BH, 2)),
      TRUE                  ~ sprintf("p=%.3f", round(p_BH, 3))
    )
  ) %>%
  select(Education, Internet, `Age group`, `Δ pp (95% CI)`, `BH p-value`)

# ---------- 5) Unir y ordenar ----------
tabla_final <- preds_fmt %>%
  left_join(diffs, by = c("Education","Internet","Age group")) %>%
  rename(
    `2016 % (95% CI)` = `2016`,
    `2021 % (95% CI)` = `2021`
  ) %>%
  arrange(Education, Internet, `Age group`)

# ---------- 6) Exportar a CSV y Word ----------
dir.create("../results/combined/tables", recursive = TRUE, showWarnings = FALSE)
readr::write_csv(tabla_final, "../results/combined/tables/recognition_by_profile_2016_2021.csv")

ft <- flextable(tabla_final) |>
  theme_booktabs() |>
  fontsize(size = 10, part = "all") |>
  font(fontname = "Calibri", part = "all") |>
  bold(part = "header") |>
  align(align = "center", part = "all") |>
  width(j = c("Education","Internet","Age group"), width = 1.3) |>
  width(j = c("2016 % (95% CI)","2021 % (95% CI)","Δ pp (95% CI)","BH p-value"), width = 1.6) |>
  autofit()

save_as_docx("Recognition by profile (2016–2021)" = ft,
             path = "../results/combined/tables/recognition_by_profile_2016_2021.docx")

message("✓ Hecho: CSV y DOCX guardados en ../results/combined/tables/")



