# ==========================================
# Exploratory Analysis – ENDIREH 2016 & 2021
# (solo RDS como entrada; sin leer CSV)
# ==========================================

# ---- Libraries ----
library(tidyverse)
library(readr)
library(scales)
library(ggplot2)
library(flextable)
library(officer)
library(emmeans)
library(forcats)

# ---- Carga SOLO desde RDS ----
rds_opts <- c("../data/combined/data_combined.rds", "/mnt/data/data_combined.rds")
rds_found <- rds_opts[file.exists(rds_opts)]

if (length(rds_found) == 0) {
  stop("No se encontró el archivo RDS de 'data_combined'. Verifica la ruta ../data/combined/data_combined.rds")
}
data_path <- rds_found[1]
df <- readr::read_rds(data_path)
message("Loaded RDS: ", data_path)

# ---- Armonizar nombres / tipos ----
# Corrige el typo si existe
if ("violencia_emcoional_vivida" %in% names(df)) {
  df <- df %>% rename(violencia_emocional_vivida = violencia_emcoional_vivida)
}

# Asegura binarios como 0/1 enteros
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
df <- df %>%
  mutate(across(any_of(viol_vars), ~ as.integer(.))) %>%
  mutate(
    year    = factor(year, levels = c(2016, 2021)),
    NOM_ENT = toupper(NOM_ENT)
  )

# Asegura ID_PER; si no existe, crea uno
if (!"ID_PER" %in% names(df)) {
  df <- df %>% mutate(ID_PER = dplyr::row_number())
}

# =======================================================
# 1) Resumen anual (cuentas y porcentajes) – Tabla general
# =======================================================
resumen <- df %>%
  group_by(year) %>%
  summarise(
    total_mujeres = n(),
    violencia_emocional_vivida      = sum(violencia_emocional_vivida, na.rm = TRUE),
    violencia_fisica_vivida         = sum(violencia_fisica_vivida,    na.rm = TRUE),
    violencia_emocional_normalizada = sum(violencia_emocional_normalizada, na.rm = TRUE),
    violencia_fisica_normalizada    = sum(violencia_fisica_normalizada,    na.rm = TRUE),
    violencia_emocional_reconocida  = sum(violencia_emocional_reconocida,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_emocional_vivida       = round(100 * violencia_emocional_vivida      / total_mujeres, 1),
    pct_fisica_vivida          = round(100 * violencia_fisica_vivida         / total_mujeres, 1),
    pct_emocional_normalizada  = round(100 * violencia_emocional_normalizada / pmax(violencia_emocional_vivida, 1), 1),
    pct_fisica_normalizada     = round(100 * violencia_fisica_normalizada    / pmax(violencia_fisica_vivida, 1), 1),
    pct_emocional_reconocida   = round(100 * violencia_emocional_reconocida  / pmax(violencia_emocional_vivida, 1), 1)
  )
dir.create("../results/combined/tables", recursive = TRUE, showWarnings = FALSE)
write_csv(resumen, "../results/combined/tables/resumen_general.csv")

# ======================================================================
# 2) Desagregación por conducta (P14_1_* experiencia, P14_2_* percepción)
# ======================================================================
p14_cols_exist <- any(grepl("^P14_[12]_(1[0-9]|2[0-2])$", names(df)))
if (p14_cols_exist) {
  dl <- df %>%
    select(ID_PER, year, matches("^P14_[12]_(1[0-9]|2[0-2])$")) %>%
    pivot_longer(
      cols = -c(ID_PER, year),
      names_to = c("serie","item"),
      names_pattern = "^P14_([12])_(\\d+)$",
      values_to = "val"
    ) %>%
    mutate(serie = if_else(serie == "1", "exp", "per")) %>%
    pivot_wider(id_cols = c(ID_PER, year, item),
                names_from = serie, values_from = val) %>%
    mutate(
      vivio      = exp %in% 1:3,
      normalizo  = vivio & per == 3,
      reconocio  = vivio & per %in% c(1, 2)
    )
  
  by_item <- dl %>%
    group_by(year, item) %>%
    summarise(
      n_mujeres   = n(),
      n_vivio     = sum(vivio, na.rm = TRUE),
      n_normalizo = sum(normalizo, na.rm = TRUE),
      n_reconocio = sum(reconocio, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      pct_normalizo = 100 * n_normalizo / pmax(n_vivio, 1),
      pct_reconocio = 100 * n_reconocio / pmax(n_vivio, 1)
    )
  
  labels_en <- c(
    "10"="Shaming/insulting/humiliating",
    "11"="Emotional neglect/lack of affection",
    "12"="Statements suggesting infidelity",
    "13"="Inducing fear",
    "14"="Threats",
    "15"="Locking in / restricting movement",
    "16"="Stalking or surveillance",
    "17"="Controlling your actions",
    "18"="Threatening with weapons or burning",
    "19"="Threats of murder/suicide/harm to children",
    "20"="Destroying or hiding belongings",
    "21"="Ignoring / silent treatment",
    "22"="Monitoring phone or email"
  )
  by_item <- by_item %>%
    mutate(tipo_violencia = factor(item, levels = names(labels_en),
                                   labels = labels_en[names(labels_en)]))
  
  dir.create("../results/combined/plots", recursive = TRUE, showWarnings = FALSE)
  
  plot_viv <- ggplot(by_item, aes(x = reorder(tipo_violencia, -n_vivio), y = n_vivio, fill = factor(year))) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = c("2016"="#1f77b4","2021"="#ff7f0e"), name="Year") +
    labs(title="Emotional violence by type and year",
         x="Type of emotional violence", y="Women who experienced it") +
    theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave("../results/combined/plots/emotional_by_item_experienced.png", plot_viv, width=11, height=6, dpi=300)
  
  plot_norm <- ggplot(by_item, aes(x = tipo_violencia, y = pct_normalizo, fill = factor(year))) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = c("2016"="#1f77b4","2021"="#ff7f0e"), name="Year") +
    labs(title="Normalised emotional violence by type",
         x="Type of emotional violence", y="% of exposed who marked 'not important'") +
    theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave("../results/combined/plots/emotional_by_item_normalised1.png", plot_norm, width=11, height=6, dpi=300)
  
  plot_reco <- ggplot(by_item, aes(x = tipo_violencia, y = pct_reconocio, fill = factor(year))) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = c("2016"="#1f77b4","2021"="#ff7f0e"), name="Year") +
    labs(title="Recognised emotional violence by type",
         x="Type of emotional violence", y="% of exposed who marked 1 or 2") +
    theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave("../results/combined/plots/emotional_by_item_recognised.png", plot_reco, width=11, height=6, dpi=300)
} else {
  message("P14_* columns not found; skipping behaviour-level plots.")
}

# ======================================================
# 3) Edad: normalización y reconocimiento (entre expuestas)
# ======================================================
niveles_edad <- c("<20","20-29","30-39","40-49","50-59","60-79",">80")
df <- df %>% mutate(grupo_edad = factor(grupo_edad, levels = niveles_edad))

# Normalización (entre quienes vivieron)
resumen_edad_norm <- df %>%
  filter(!is.na(violencia_emocional_normalizada), violencia_emocional_vivida == 1) %>%
  group_by(year, grupo_edad) %>%
  summarise(
    total = n(),
    normalizaron = sum(violencia_emocional_normalizada, na.rm = TRUE),
    porcentaje = round(100 * normalizaron / pmax(total, 1), 1),
    .groups = "drop"
  )

age_plot_norm <- ggplot(resumen_edad_norm, aes(x = grupo_edad, y = porcentaje, color = year, group = year)) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  labs(title="Normalised Emotional Violence (victims only)",
       subtitle="By age group – 2016 vs 2021",
       x="Age group", y="% who considered it 'not important'", color="Year") +
  theme_minimal(base_size = 11)
ggsave("../results/combined/plots/age_plot_normalised.png", age_plot_norm, width=8, height=5, dpi=300)

# Reconocimiento (entre quienes vivieron)
resumen_edad_reco <- df %>%
  filter(!is.na(violencia_emocional_reconocida), violencia_emocional_vivida == 1) %>%
  group_by(year, grupo_edad) %>%
  summarise(
    total_vivieron = n(),
    reconocieron   = sum(violencia_emocional_reconocida, na.rm = TRUE),
    porcentaje     = round(100 * reconocieron / pmax(total_vivieron, 1), 1),
    .groups = "drop"
  )

age_plot_rec <- ggplot(resumen_edad_reco, aes(x = grupo_edad, y = porcentaje, color = year, group = year)) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  labs(title="Recognised Emotional Violence (victims only)",
       subtitle="By age group – 2016 vs 2021",
       x="Age group", y="% who rated it 'severe'", color="Year") +
  theme_minimal(base_size = 11)
ggsave("../results/combined/plots/age_plot_recognised.png", age_plot_rec, width=8, height=5, dpi=300)

# ====== Tablas flextable para anexos: Normalización y Reconocimiento ======
# Compacta normalización
tab_norm_age <- resumen_edad_norm %>%
  select(year, grupo_edad, total_ = total, pct_ = porcentaje) %>%
  pivot_wider(names_from = year, values_from = c(total_, pct_), names_sep = "_") %>%
  mutate(delta_pp = round(pct__2021 - pct__2016, 1))
tot_norm <- resumen_edad_norm %>%
  group_by(year) %>%
  summarise(total_ = sum(total), pct_ = round(100*sum(normalizaron)/pmax(sum(total),1),1), .groups="drop") %>%
  pivot_wider(names_from = year, values_from = c(total_, pct_), names_sep = "_") %>%
  mutate(grupo_edad = "All ages", delta_pp = round(pct__2021 - pct__2016, 1))
compact_tab_norm <- bind_rows(tab_norm_age, tot_norm) %>%
  mutate(grupo_edad = factor(grupo_edad, levels = c(niveles_edad, "All ages"), ordered = TRUE)) %>%
  arrange(grupo_edad) %>%
  select(grupo_edad, total__2016, pct__2016, total__2021, pct__2021, delta_pp)

ft_norm <- flextable(compact_tab_norm) |>
  set_header_labels(
    grupo_edad="Age group",
    total__2016="N 2016", pct__2016="% 2016",
    total__2021="N 2021", pct__2021="% 2021",
    delta_pp="Change p.p."
  ) |> theme_vanilla() |>
  colformat_num(j=c("pct__2016","pct__2021","delta_pp"), digits=1, suffix="%") |>
  autofit() |> fontsize(size=9) |> fit_to_width(max_width=6.5) |>
  align(align="center", part="all") |> align(j=1, align="left", part="all") |>
  set_caption("Table A1. Normalising emotional violence among victims: 2016 vs 2021")
print(ft_norm, preview="docx", path="../results/combined/tables/appendix_norm_experienced.docx")

# Compacta reconocimiento
tab_reco_age <- resumen_edad_reco %>%
  transmute(year, grupo_edad, total_ = total_vivieron, pct_ = porcentaje) %>%
  pivot_wider(names_from = year, values_from = c(total_, pct_), names_sep = "_") %>%
  mutate(delta_pp = round(pct__2021 - pct__2016, 1))
tot_reco <- resumen_edad_reco %>%
  group_by(year) %>%
  summarise(total_v = sum(total_vivieron), reco = sum(reconocieron), .groups = "drop") %>%
  mutate(pct_ = round(100 * reco / pmax(total_v, 1), 1)) %>%
  pivot_wider(names_from = year, values_from = c(total_v, pct_), names_sep = "_") %>%
  mutate(grupo_edad = "All ages", delta_pp = round(pct__2021 - pct__2016, 1)) %>%
  select(grupo_edad, total_v_2016, pct__2016, total_v_2021, pct__2021, delta_pp)
compact_tab_reco <- bind_rows(
  tab_reco_age %>% rename(total_v_2016 = total__2016, total_v_2021 = total__2021),
  tot_reco
) %>%
  mutate(grupo_edad = factor(grupo_edad, levels = c(niveles_edad, "All ages"), ordered = TRUE)) %>%
  arrange(grupo_edad) %>%
  select(grupo_edad, total_v_2016, pct__2016, total_v_2021, pct__2021, delta_pp)

ft_reco <- flextable(compact_tab_reco) |>
  set_header_labels(
    grupo_edad="Age group",
    total_v_2016="N 2016", pct__2016="% 2016",
    total_v_2021="N 2021", pct__2021="% 2021",
    delta_pp="Change p.p."
  ) |> theme_vanilla() |>
  colformat_num(j=c("pct__2016","pct__2021","delta_pp"), digits=1, suffix="%") |>
  autofit() |> fontsize(size=9) |> fit_to_width(max_width=6.5) |>
  align(align="center", part="all") |> align(j=1, align="left", part="all") |>
  set_caption("Table A2. Recognising emotional violence among victims: 2016 vs 2021")
print(ft_reco, preview="docx", path="../results/combined/tables/appendix_recognised_experienced.docx")

# =======================================================
# 4) Reconocimiento por estado (entre expuestas) + barras + slope
# =======================================================
# Función robusta para IC binomial (Wilson por defecto de prop.test)
binom_ci <- function(x, n) {
  if (is.na(x) || is.na(n) || n <= 0) return(c(lower = NA_real_, upper = NA_real_))
  ci <- suppressWarnings(prop.test(x, n, correct = FALSE)$conf.int)
  c(lower = ci[1], upper = ci[2])
}

# Construir by_state UNA VEZ con IC95% correctos
by_state <- df %>%
  mutate(
    rec = as.integer(violencia_emocional_reconocida == 1),
    exp = as.integer(violencia_emocional_vivida == 1)
  ) %>%
  filter(exp == 1, !is.na(rec), !is.na(NOM_ENT), !is.na(year), NOM_ENT != "NA") %>%
  group_by(NOM_ENT, year) %>%
  summarise(
    recognised = sum(rec, na.rm = TRUE),
    total      = n(),
    share      = recognised / total,
    .groups    = "drop"
  ) %>%
  mutate(ci = map2(recognised, total, binom_ci)) %>%   # <- calcula IC por fila
  unnest_wider(ci) %>%                                 # <- crea columnas lower y upper
  mutate(pct = 100 * share)

# Orden por 2021
order_2021 <- by_state %>%
  filter(year == 2021) %>%
  arrange(share) %>%
  pull(NOM_ENT)

by_state <- by_state %>%
  mutate(
    NOM_ENT = factor(NOM_ENT, levels = order_2021)
  ) %>%
  filter(!is.na(NOM_ENT)) %>%
  mutate(NOM_ENT = fct_drop(NOM_ENT))

dir.create("../results/combined/plots", recursive = TRUE, showWarnings = FALSE)
dir.create("../results/combined/tables", recursive = TRUE, showWarnings = FALSE)

# (a) Barras por estado (sin IC)
p_barras <- ggplot(by_state, aes(x = pct, y = NOM_ENT, fill = year)) +
  geom_col(position = position_dodge(width = .65), width = .6) +
  geom_text(aes(label = sprintf("%.1f%%", pct)),
            position = position_dodge(width = .65),
            hjust = -0.15, size = 3) +
  scale_x_continuous(limits = c(0, max(by_state$pct, na.rm = TRUE) + 6)) +
  labs(
    title = "Recognition of emotional violence among victims, by state",
    subtitle = "Share recognising as ‘severe’ (ordered by 2021)",
    x = "Recognised (%)", y = NULL, fill = "Year"
  ) +
  theme_minimal(base_size = 12)

ggsave("../results/combined/plots/emotional_recognition_by_state_bars.png",
       p_barras, width = 10, height = 8, dpi = 300)

# (b) Barras con IC95% (horizontales)
p_states <- ggplot(by_state, aes(x = share, y = NOM_ENT, fill = year)) +
  geom_col(position = position_dodge(width = .7), width = .6, alpha = .9) +
  geom_errorbarh(aes(xmin = lower, xmax = upper),
                 position = position_dodge(width = .7), height = 0.2, alpha = 0.6) +
  scale_x_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, max(by_state$upper, na.rm = TRUE) + 0.03)) +
  labs(
    title = "Recognition of emotional violence among victims, by state",
    subtitle = "Share rating emotional violence as 'severe' (95% CI)",
    x = "Recognised share", y = NULL, fill = "Year"
  ) +
  theme_minimal(base_size = 12)

ggsave("../results/combined/plots/emotional_recognition_by_state.png",
       p_states, width = 10, height = 8, dpi = 300)

# (c) Slopegraph 2016 vs 2021
p_slope <- ggplot(by_state, aes(x = year, y = share, group = NOM_ENT)) +
  geom_line(alpha = 0.5) + geom_point(size = 1.8) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Recognition of emotional violence among victims, by state",
    subtitle = "Slopegraph: 2016 vs 2021", x = NULL, y = "Recognised share"
  ) +
  theme_minimal(base_size = 12)

ggsave("../results/combined/plots/emotional_recognition_by_state_slope.png",
       p_slope, width = 8, height = 6, dpi = 300)

# Exporta tabla para anexo
readr::write_csv(by_state, "../results/combined/tables/emotional_recognition_by_state.csv")

# =======================================================
# 5) Perfiles Privileged vs Disadvantaged (tamaños & gap)
# =======================================================
# Mapear educación e internet
df <- df %>%
  mutate(
    Education = factor(if_else(GRA_bin == 1, "Higher education", "Lower education"),
                       levels = c("Lower education", "Higher education")),
    Internet  = factor(if_else(P1_4_9 == 1, "Internet", "No internet"),
                       levels = c("No internet", "Internet"))
  )

vict <- df %>% filter(violencia_emocional_vivida == 1)

# Tamaños de los perfiles en víctimas
sizes <- vict %>%
  mutate(profile = case_when(
    grupo_edad == "20-29" & Internet == "Internet"    & Education == "Higher education" ~ "Privileged",
    grupo_edad == "60-79" & Internet == "No internet" & Education == "Lower education"  ~ "Disadvantaged",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(profile)) %>%
  count(year, profile) %>%
  tidyr::pivot_wider(names_from = profile, values_from = n, values_fill = 0)
write_csv(sizes, "../results/combined/tables/pd_counts_victims_wide.csv")

# GAP ajustado (outcome = reconocida)
m <- glm(
  violencia_emocional_reconocida ~ year * (grupo_edad + Internet + Education) +
    grupo_edad * Internet + Education * Internet,
  data = vict, family = binomial()
)
emm_link <- emmeans(m, ~ year | grupo_edad + Internet + Education)
emm_resp <- regrid(emm_link, transform = "response")

emm_two <- subset(
  emm_resp,
  (grupo_edad=="20-29" & Internet=="Internet" & Education=="Higher education") |
    (grupo_edad=="60-79" & Internet=="No internet" & Education=="Lower education")
)
emm_two <- update(emm_two, by.vars = "year")
gap_y <- contrast(emm_two, method = list("Priv - Disadv" = c(1, -1)), by = "year")
gdf <- as.data.frame(summary(gap_y, infer = TRUE))

val <- intersect(c("estimate","response","prob","emmean",".response"), names(gdf))[1]
lo  <- if ("lower.CL" %in% names(gdf)) "lower.CL" else "asymp.LCL"
hi  <- if ("upper.CL" %in% names(gdf)) "upper.CL" else "asymp.UCL"

gap_by_year <- gdf %>%
  transmute(
    Year = year,
    `Gap (pp)`   = 100*.data[[val]],
    `95% CI low` = 100*.data[[lo]],
    `95% CI high`= 100*.data[[hi]],
    `p-value`    = p.value
  )
write_csv(gap_by_year, "../results/combined/tables/gap_recognised_profiles.csv")

p_gap <- ggplot(gap_by_year, aes(x=factor(Year), y=`Gap (pp)`, fill=factor(Year))) +
  geom_col(width=.55) +
  geom_errorbar(aes(ymin=`95% CI low`, ymax=`95% CI high`), width=.15) +
  geom_hline(yintercept = 0, linetype="dashed") +
  geom_text(aes(label=sprintf("%.1f pp", `Gap (pp)`)), vjust=-0.4, size=3.5) +
  labs(title="Privileged vs Disadvantaged Gap (adjusted)",
       subtitle="Outcome: recognising emotional violence (victims only)",
       x="Year", y="Gap (percentage points)", fill="Year") +
  theme_minimal(base_size = 12)
ggsave("../results/combined/plots/gap_recognised_profiles.png", p_gap, width=8, height=5, dpi=300)

message("Done ✅ — Solo RDS de entrada. Tablas y figuras en ../results/combined/")

