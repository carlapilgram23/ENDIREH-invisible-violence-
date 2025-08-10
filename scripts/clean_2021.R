# ====================================
# Clean and prepare ENDIREH 2021 data
# ====================================

library(data.table)
library(tidyverse)
library(stringr)
library(stringi)

# --- 1) Selección
cols_emoc_exp <- paste0("P14_1_", 10:22)
cols_emoc_per <- paste0("P14_2_", 10:22)
cols_fis_exp  <- paste0("P14_1_", 1:9)
cols_fis_per  <- paste0("P14_2_", 1:9)

cols_all_exp <- paste0("P14_1_", 1:22)
cols_all_per <- paste0("P14_2_", 1:22)

cols_all <- c("ID_PER", cols_emoc_exp, cols_emoc_per, cols_fis_exp, cols_fis_per)

# --- 2) Violencia (forzar numérico)
sec_xiv_2021 <- fread(
  "../data/2021/raw/conjunto_de_datos_TB_SEC_XIV/conjunto_de_datos/conjunto_de_datos_TB_SEC_XIV.csv",
  select = unique(c("ID_PER", cols_all_exp, cols_all_per)), encoding = "UTF-8"
) |> as_tibble() |>
  mutate(across(-ID_PER, ~ suppressWarnings(as.numeric(.))))

# --- 3) Socio-demo
tsdem_2021 <- fread(
  "../data/2021/raw/conjunto_de_datos_TSDem/conjunto_de_datos/conjunto_de_datos_TSDem.csv",
  select = c("ID_PER","EDAD","GRA","NOM_ENT"), encoding = "UTF-8"
) |> as_tibble() |>
  mutate(
    across(c(EDAD, GRA), ~ suppressWarnings(as.numeric(.))),
    across(c(ID_PER, NOM_ENT), str_trim),
    NOM_ENT = stri_trans_general(NOM_ENT, "Latin-ASCII") # quitar acentos
  )

# --- 4) Merge + grupos + GRA_bin
data_2021_cleaned <- sec_xiv_2021 |>
  mutate(ID_PER = str_trim(ID_PER)) |>
  left_join(tsdem_2021, by="ID_PER") |>
  mutate(
    grupo_edad = case_when(
      EDAD < 20 ~ "<20",
      EDAD < 30 ~ "20-29",
      EDAD < 40 ~ "30-39",
      EDAD < 50 ~ "40-49",
      EDAD < 60 ~ "50-59",
      EDAD < 80 ~ "60-79",
      EDAD >= 80 ~ ">80",
      TRUE ~ NA_character_
    ),
    GRA_bin = if_else(GRA >= 8, 1L, 0L, missing = NA_integer_)
  ) |>
  filter(!is.na(grupo_edad))

# --- 5) Vivienda + acceso digital
tviv_2021 <- fread(
  "../data/2021/raw/conjunto_de_datos_TVIV/conjunto_de_datos/conjunto_de_datos_TVIV.csv",
  select = c("ID_VIV","P1_4_2","P1_4_3","P1_4_4","P1_4_5","P1_4_9"), encoding = "Latin-1"
) |> as_tibble() |>
  mutate(across(-ID_VIV, ~ suppressWarnings(as.numeric(.))),
         ID_VIV = str_trim(as.character(ID_VIV)))

data_2021_cleaned <- data_2021_cleaned |>
  mutate(ID_VIV = str_extract(ID_PER, "^\\d+\\.\\d+"),
         ID_VIV = str_trim(as.character(ID_VIV))) |>
  left_join(tviv_2021, by="ID_VIV") |>
  mutate(
    acceso_digital = rowSums(across(c("P1_4_2","P1_4_3","P1_4_5","P1_4_9"), ~ . == 1), na.rm = TRUE)
  )

# --- 6) Indicadores 0/1 numéricos (igual que en 2016)

# Precomputar matrices
exp_em  <- data_2021_cleaned |> select(all_of(cols_emoc_exp)) |> as.matrix()
per_em  <- data_2021_cleaned |> select(all_of(cols_emoc_per)) |> as.matrix()
exp_fis <- data_2021_cleaned |> select(all_of(cols_fis_exp))  |> as.matrix()
per_fis <- data_2021_cleaned |> select(all_of(cols_fis_per))  |> as.matrix()
exp_all <- data_2021_cleaned |> select(all_of(cols_all_exp))  |> as.matrix()
per_all <- data_2021_cleaned |> select(all_of(cols_all_per))  |> as.matrix()

# máscaras por fila
lived_em   <- (exp_em >= 1 & exp_em <= 3)               # situaciones emocionales vividas
valid_em   <- lived_em & (per_em %in% c(1, 2))          # vivida y marcada como 1 o 2
n_lived_em <- rowSums(lived_em, na.rm = TRUE)
n_valid_em <- rowSums(valid_em, na.rm = TRUE)

data_2021_cleaned <- data_2021_cleaned |>
  mutate(
    # Emocionales (sin cambios en vivida/normalizada)
    violencia_emocional_vivida       = as.integer(rowSums(exp_em  >= 1 & exp_em  <= 3, na.rm = TRUE) > 0),
    violencia_emocional_normalizada  = as.integer(rowSums((exp_em >= 1 & exp_em <= 3) & (per_em == 3), na.rm = TRUE) > 0),
    
    # Reconocida estricta: todas las vividas deben ser 1 o 2 (NA o 3 invalidan)
    violencia_emocional_reconocida   = as.integer(n_lived_em > 0 & n_valid_em == n_lived_em),
    
    # Físicas (igual que antes)
    violencia_fisica_vivida          = as.integer(rowSums(exp_fis >= 1 & exp_fis <= 3, na.rm = TRUE) > 0),
    violencia_fisica_normalizada     = as.integer(rowSums((exp_fis >= 1 & exp_fis <= 3) & (per_fis == 3), na.rm = TRUE) > 0),
    
    # Generales (igual que antes)
    violencia_vivida                 = as.integer(rowSums(exp_all >= 1 & exp_all <= 3, na.rm = TRUE) > 0),
    violencia_normalizada            = as.integer(rowSums((exp_all >= 1 & exp_all <= 3) & (per_all == 3), na.rm = TRUE) > 0),
    violencia_reconocida             = as.integer(rowSums((exp_all >= 1 & exp_all <= 3) & (per_all %in% c(1, 2)), na.rm = TRUE) > 0)
  )


# --- 7) Guardar
dir.create("../data/2021/cleaned", recursive=TRUE, showWarnings=FALSE)
write_rds(data_2021_cleaned, "../data/2021/cleaned/data_2021_cleaned.rds")
write_csv(data_2021_cleaned, "../data/2021/cleaned/data_2021_cleaned.csv")
