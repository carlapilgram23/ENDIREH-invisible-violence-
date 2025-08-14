
# =====================================================
# gap_analysis.R — Adjusted & raw gaps: Privileged − Disadvantaged
# =====================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(forcats)
  library(emmeans)
  library(ggplot2)
  library(flextable)
  library(officer)
  library(scales)
})

# ---------- Salidas ----------
out_plot_dir  <- "../results/combined/plots"
out_table_dir <- "../results/combined/tables"
dir.create(out_plot_dir,  recursive = TRUE, showWarnings = FALSE)
dir.create(out_table_dir, recursive = TRUE, showWarnings = FALSE)

# ---------- Datos: víctimas + factores coherentes ----------
data_vict <- read_rds("../data/combined/data_combined.rds") %>%
  filter(violencia_emocional_vivida %in% c(1, TRUE)) %>%
  mutate(
    # armoniza grupo_edad si viene "60-80"
    grupo_edad = as.character(grupo_edad),
    grupo_edad = ifelse(grupo_edad == "60-80", "60-79", grupo_edad),
    grupo_edad = factor(grupo_edad, levels = c("60-79","<20","20-29","30-39","40-49","50-59",">80")),
    year       = factor(year, levels = c(2016, 2021)),
    GRA_bin    = factor(GRA_bin, levels = c(0,1), labels = c("No","Yes")),
    P1_4_9     = factor(P1_4_9, levels = c(2,1), labels = c("No","Yes"))
  ) %>%
  drop_na(violencia_emocional_reconocida, year, grupo_edad, GRA_bin, P1_4_9)

# ---------- Chequeo crudo para validar perfiles ----------
check_profiles <- data_vict %>%
  mutate(
    Education = if_else(GRA_bin == "Yes", "Higher education", "Lower education"),
    Internet  = if_else(P1_4_9 == "Yes", "Internet", "No internet"),
    Profile = case_when(
      grupo_edad == "20-29" & Internet == "Internet"    & Education == "Higher education" ~ "Privileged",
      grupo_edad == "60-79" & Internet == "No internet" & Education == "Lower education"  ~ "Disadvantaged",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Profile)) %>%
  group_by(year, Profile) %>%
  summarise(N = n(),
            Recognition_pct = 100*mean(violencia_emocional_reconocida, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(year, Profile)

print(check_profiles)
# Esperado: Privileged > Disadvantaged (si no, revisa definiciones/etiquetas).

# ---------- Modelo logístico para reconocimiento ----------
m <- glm(
  violencia_emocional_reconocida ~ year * (grupo_edad + P1_4_9 + GRA_bin) +
    grupo_edad * P1_4_9 + GRA_bin * P1_4_9,
  data = data_vict, family = binomial()
)

# ---------- EMMs y contraste del gap (Privileged − Disadvantaged) ----------
emm_link <- emmeans(m, ~ year | grupo_edad + P1_4_9 + GRA_bin)
emm_resp <- regrid(emm_link, transform = "response")  # escala prob.

emm_two <- subset(
  emm_resp,
  (grupo_edad=="20-29" & P1_4_9=="Yes" & GRA_bin=="Yes") |   # Privileged
    (grupo_edad=="60-79" & P1_4_9=="No"  & GRA_bin=="No")      # Disadvantaged
)
emm_two <- update(emm_two, by.vars = "year")

# *** Señal elegida: Privileged − Disadvantaged ***
gap_y <- contrast(emm_two, method = list("Priv − Disadv" = c(+1, -1)), by = "year")
gdf    <- as.data.frame(summary(gap_y, infer = TRUE))

val <- intersect(c("estimate","response","prob","emmean",".response"), names(gdf))[1]
lo  <- if ("lower.CL" %in% names(gdf)) "lower.CL" else "asymp.LCL"
hi  <- if ("upper.CL" %in% names(gdf)) "upper.CL" else "asymp.UCL"

gap_tbl <- gdf %>%
  transmute(
    Year        = year,
    `Gap (pp)`  = 100*.data[[val]],
    `95% CI low`  = 100*.data[[lo]],
    `95% CI high` = 100*.data[[hi]],
    `p-value`   = p.value
  )

# Guardar CSV (ajustado)
write_csv(gap_tbl, file.path(out_table_dir, "gap_recognised_profiles.csv"))

# Figura (ajustado)
p_gap <- ggplot(gap_tbl, aes(x = factor(Year), y = `Gap (pp)`, fill = factor(Year))) +
  geom_col(width = .55) +
  geom_errorbar(aes(ymin = `95% CI low`, ymax = `95% CI high`), width = .15) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(aes(label = sprintf("%.1f pp", `Gap (pp)`)), vjust = -0.4, size = 3.5) +
  labs(
    title = "Privileged − Disadvantaged gap (adjusted probability)",
    subtitle = "Outcome: recognising emotional violence (victims only)",
    x = "Year", y = "Gap (percentage points)", fill = "Year"
  ) +
  theme_minimal(base_size = 12)

ggsave(file.path(out_plot_dir, "gap_recognised_profiles.png"),
       p_gap, width = 8, height = 5, dpi = 300)

# ---------- Conteos y gap crudo (Priv − Disadv) ----------
tab_raw <- data_vict %>%
  mutate(
    Education = if_else(GRA_bin == "Yes", "Higher education", "Lower education"),
    Internet  = if_else(P1_4_9 == "Yes", "Internet", "No internet"),
    Profile = case_when(
      grupo_edad=="20-29" & Internet=="Internet"    & Education=="Higher education" ~ "Privileged",
      grupo_edad=="60-79" & Internet=="No internet" & Education=="Lower education"  ~ "Disadvantaged",
      TRUE ~ NA_character_
    ),
    rec = as.integer(violencia_emocional_reconocida %in% c(1, TRUE))
  ) %>%
  filter(!is.na(Profile))

tab_counts <- tab_raw %>%
  group_by(year, Profile) %>%
  summarise(
    Recognised = sum(rec, na.rm = TRUE),
    Total      = n(),
    Share      = Recognised / Total,
    .groups = "drop"
  ) %>%
  arrange(year, Profile)

tab_wide <- tab_counts %>%
  select(year, Profile, Recognised, Total, Share) %>%
  pivot_wider(
    names_from = Profile,
    values_from = c(Recognised, Total, Share),
    names_sep = "_"
  )

tab_gap_raw <- tab_wide %>%
  rowwise() %>%
  mutate(
    gap_pp   = 100 * (Share_Privileged - Share_Disadvantaged),  # Priv − Disadv
    test     = list(prop.test(
      x = c(Recognised_Privileged, Recognised_Disadvantaged),
      n = c(Total_Privileged,      Total_Disadvantaged),
      correct = FALSE
    )),
    low_pp   = 100 * test$conf.int[1],
    high_pp  = 100 * test$conf.int[2],
    p_value  = test$p.value
  ) %>%
  ungroup() %>%
  mutate(
    `Privileged (x of n, %)`    = sprintf("%d of %d (%.1f%%)", Recognised_Privileged, Total_Privileged, 100*Share_Privileged),
    `Disadvantaged (x of n, %)` = sprintf("%d of %d (%.1f%%)", Recognised_Disadvantaged, Total_Disadvantaged, 100*Share_Disadvantaged)
  ) %>%
  transmute(
    Year = year,
    `Privileged (x of n, %)` ,
    `Disadvantaged (x of n, %)` ,
    `Gap (Priv − Disadv, pp)` = sprintf("%.1f", gap_pp),
    `95% CI`                  = sprintf("[%.1f, %.1f]", low_pp, high_pp),
    `p-value`                 = format.pval(p_value, digits = 3, eps = 1e-3)
  )

# Guardar CSV (crudo)
write_csv(tab_gap_raw, file.path(out_table_dir, "counts_with_gap_recognition.csv"))

# DOCX opcional con tabla cruda
ft <- flextable(tab_gap_raw) %>%
  set_caption("Counts, shares, and gap (Privileged − Disadvantaged)") %>%
  theme_booktabs() %>% fontsize(size = 10, part = "all") %>%
  align(align = "center", part = "all") %>% autofit()

save_as_docx("Counts, shares, and gap (Priv − Disadv)" = ft,
             path = file.path(out_table_dir, "counts_with_gap_recognition.docx"))

message("✓ Done — CSVs y plots en: ", normalizePath(out_table_dir), " / ", normalizePath(out_plot_dir))
