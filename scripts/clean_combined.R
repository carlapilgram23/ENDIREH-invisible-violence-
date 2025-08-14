# ==========================================
# build_data_combined.R
# Combina 2016 + 2021 y estandariza NOM_ENT
#  - "MÉXICO"/"EDO. DE MEXICO" -> "ESTADO DE MEXICO"
#  - "CDMX"/"DISTRITO FEDERAL"  -> "CIUDAD DE MEXICO"
# ==========================================

library(tidyverse)
library(stringr)
library(stringi)

# --- Cargar datasets limpios ---
data_2016_cleaned <- read_rds("../data/2016/cleaned/data_2016_cleaned.rds")
data_2021_cleaned <- read_rds("../data/2021/cleaned/data_2021_cleaned.rds")

# --- Variables binarias a homogeneizar como 0/1 enteros ---
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

# --- Helper: estandarizar nombres de entidad ---
std_nom_ent <- function(x) {
  x <- stri_trans_general(as.character(x), "Latin-ASCII")
  x <- toupper(str_squish(x))
  dplyr::case_when(
    x %in% c("MEXICO", "ESTADO DE MEXICO", "EDO DE MEXICO", "EDO. DE MEXICO") ~ "ESTADO DE MEXICO",
    x %in% c("CIUDAD DE MEXICO", "CDMX", "DISTRITO FEDERAL")                  ~ "CIUDAD DE MEXICO",
    TRUE ~ x
  )
}

# --- Alinear 2016 ---
d16 <- data_2016_cleaned %>%
  mutate(
    year    = 2016L,
    NOM_ENT = std_nom_ent(NOM_ENT),
    across(any_of(viol_vars), ~ as.integer(.)),
    GRA_bin = as.integer(GRA_bin)
  )

# --- Alinear 2021 ---
d21 <- data_2021_cleaned %>%
  mutate(
    year    = 2021L,
    NOM_ENT = std_nom_ent(NOM_ENT),
    across(any_of(viol_vars), ~ as.integer(.)),
    GRA_bin = as.integer(GRA_bin)
  )

# --- Conjunto total de columnas ---
all_cols <- union(names(d16), names(d21))

# --- Detectar columnas P14_* del cuestionario ---
survey_cols <- grep("^P14_[12]_\\d+$", all_cols, value = TRUE)

# --- Helper: añadir columnas faltantes con tipo adecuado ---
add_missing_cols <- function(df) {
  missing <- setdiff(all_cols, names(df))
  for (nm in missing) {
    if (nm %in% survey_cols) {
      df[[nm]] <- NA_real_
    } else if (nm %in% c("EDAD","GRA","acceso_digital")) {
      df[[nm]] <- NA_real_
    } else if (nm %in% c("GRA_bin")) {
      df[[nm]] <- NA_integer_
    } else if (nm %in% c("ID_PER","ID_VIV","NOM_ENT")) {
      df[[nm]] <- NA_character_
    } else if (nm %in% c(
      "violencia_emocional_vivida",
      "violencia_emocional_normalizada",
      "violencia_emocional_reconocida",
      "violencia_fisica_vivida",
      "violencia_fisica_normalizada",
      "violencia_vivida",
      "violencia_normalizada",
      "violencia_reconocida"
    )) {
      df[[nm]] <- NA_integer_
    } else if (nm == "grupo_edad") {
      df[[nm]] <- factor(NA, levels = c("<20","20-29","30-39","40-49","50-59","60-79",">80"),
                         ordered = TRUE)
    } else if (nm == "year") {
      df[[nm]] <- NA_integer_
    } else if (nm == "periodo") {
      df[[nm]] <- NA_character_
    } else {
      df[[nm]] <- NA
    }
  }
  df
}

# --- Homogeneizar columnas y ordenar ---
d16_h <- add_missing_cols(d16)[, all_cols]
d21_h <- add_missing_cols(d21)[, all_cols]

# --- Unir y finalizar ---
data_combined_cleaned <- bind_rows(d16_h, d21_h) %>%
  mutate(
    periodo    = if_else(year == 2016L, "pre", "post"),
    NOM_ENT    = std_nom_ent(NOM_ENT),  # aplicar por si quedó algo suelto
    grupo_edad = factor(
      as.character(grupo_edad),
      levels = c("<20","20-29","30-39","40-49","50-59","60-79",">80"),
      ordered = TRUE
    ),
    across(any_of(viol_vars), ~ as.integer(.)),
    GRA_bin = as.integer(GRA_bin)
  )

# --- Guardar ---
dir.create("../data/combined", recursive = TRUE, showWarnings = FALSE)
write_rds(data_combined_cleaned, "../data/combined/data_combined.rds")
write_csv(data_combined_cleaned, "../data/combined/data_combined.csv")

message("OK ✅  Guardado en ../data/combined/")
