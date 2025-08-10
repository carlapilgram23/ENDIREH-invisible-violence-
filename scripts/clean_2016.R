# ====================================
# Clean and prepare ENDIREH 2016 data
# ====================================

library(data.table)
library(tidyverse)
library(tibble)
library(stringr)
library(purrr)

# ==== 1. Define columns to load ====

cols_emoc_exp <- paste0("P13_1_", 10:22)
cols_emoc_per <- paste0("P13_2_", 10:22)
cols_fis_exp  <- paste0("P13_1_", 1:9)
cols_fis_per  <- paste0("P13_2_", 1:9)

# para indicadores generales
cols_all_exp  <- paste0("P13_1_", 1:22)
cols_all_per  <- paste0("P13_2_", 1:22)

# ==== 2. Load violence section ====

sec_xiii_2016 <- fread(
  "../data/2016/raw/conjunto_de_datos_tb_sec_xiii_endireh_2016/conjunto_de_datos/conjunto_de_datos_tb_sec_xiii_endireh_2016.csv",
  select   = unique(c("ID_MUJ", cols_all_exp, cols_all_per)),
  encoding = "UTF-8"
) %>% 
  as_tibble() %>%
  rename(ID_PER = ID_MUJ) %>%
  mutate(across(-ID_PER, ~ suppressWarnings(as.numeric(.))))

# ==== 3. Create indicators ====

# ==== 3. Create indicators (versión con reconocida estricta) ====

# Helpers
any_yes_1to3 <- function(x) any(x %in% 1:3, na.rm = TRUE)

# Matrices de experiencia y percepción
exp_em  <- sec_xiii_2016 %>% select(all_of(cols_emoc_exp)) %>% as.matrix()
per_em  <- sec_xiii_2016 %>% select(all_of(cols_emoc_per)) %>% as.matrix()
exp_fis <- sec_xiii_2016 %>% select(all_of(cols_fis_exp))  %>% as.matrix()
per_fis <- sec_xiii_2016 %>% select(all_of(cols_fis_per))  %>% as.matrix()
exp_all <- sec_xiii_2016 %>% select(all_of(cols_all_exp))  %>% as.matrix()
per_all <- sec_xiii_2016 %>% select(all_of(cols_all_per))  %>% as.matrix()

# Máscaras para reconocida estricta (emocional)
lived_em   <- (exp_em >= 1 & exp_em <= 3)              # situaciones emocionales vividas
valid_em   <- lived_em & (per_em %in% c(1, 2))         # vividas y marcadas como 1 o 2
n_lived_em <- rowSums(lived_em,  na.rm = TRUE)
n_valid_em <- rowSums(valid_em,  na.rm = TRUE)

sec_xiii_2016 <- sec_xiii_2016 %>%
  mutate(
    # Emocionales
    violencia_emocional_vivida      = as.integer(rowSums(exp_em >= 1 & exp_em <= 3, na.rm = TRUE) > 0),
    violencia_emocional_normalizada = as.integer(rowSums((exp_em >= 1 & exp_em <= 3) & (per_em == 3), na.rm = TRUE) > 0),
    # ESTRICTA: todas las vividas deben ser 1 o 2 (si alguna es 3 o NA → 0)
    violencia_emocional_reconocida  = as.integer(n_lived_em > 0 & n_valid_em == n_lived_em),
    
    # Físicas
    violencia_fisica_vivida         = as.integer(rowSums(exp_fis >= 1 & exp_fis <= 3, na.rm = TRUE) > 0),
    violencia_fisica_normalizada    = as.integer(rowSums((exp_fis >= 1 & exp_fis <= 3) & (per_fis == 3), na.rm = TRUE) > 0),
    
    # Generales (sin cambios)
    violencia_vivida                = as.integer(rowSums(exp_all >= 1 & exp_all <= 3, na.rm = TRUE) > 0),
    violencia_normalizada           = as.integer(rowSums((exp_all >= 1 & exp_all <= 3) & (per_all == 3), na.rm = TRUE) > 0),
    violencia_reconocida            = as.integer(rowSums((exp_all >= 1 & exp_all <= 3) & (per_all %in% c(1, 2)), na.rm = TRUE) > 0)
  )

# ==== 4. Load sociodemographic data ====

tsdem_2016 <- fread(
  "../data/2016/raw/conjunto_de_datos_tsdem_endireh_2016/conjunto_de_datos/conjunto_de_datos_tsdem_endireh_2016.csv",
  select   = c("ID_MUJ", "EDAD", "GRA", "NOM_ENT"),
  encoding = "UTF-8"
) %>%
  as_tibble() %>%
  rename(ID_PER = ID_MUJ) %>%
  mutate(
    ID_PER  = str_trim(ID_PER),
    EDAD    = suppressWarnings(as.numeric(EDAD)),
    GRA     = suppressWarnings(as.numeric(GRA)),
    NOM_ENT = str_to_upper(str_trim(NOM_ENT))
  )

# ==== 5. Merge and add age groups ====

data_2016_cleaned <- sec_xiii_2016 %>%
  mutate(ID_PER = str_trim(ID_PER)) %>%
  left_join(tsdem_2016, by = "ID_PER") %>%
  mutate(
    grupo_edad = case_when(
      EDAD < 20 ~ "<20",
      EDAD >= 20 & EDAD < 30 ~ "20-29",
      EDAD >= 30 & EDAD < 40 ~ "30-39",
      EDAD >= 40 & EDAD < 50 ~ "40-49",
      EDAD >= 50 & EDAD < 60 ~ "50-59",
      EDAD >= 60 & EDAD < 80 ~ "60-79",
      EDAD > 80 ~ ">80",
      TRUE ~ NA_character_
    ),
    GRA_bin = if_else(GRA >= 8, 1, 0, missing = NA_real_)
  ) %>%
  filter(!is.na(grupo_edad))

# ==== 6. Load housing data ====

tviv_2016 <- fread(
  "../data/2016/raw/conjunto_de_datos_tviv_endireh_2016/conjunto_de_datos/conjunto_de_datos_tviv_endireh_2016.csv",
  select   = c("ID_VIV", "P1_4_1", "P1_4_2", "P1_4_3", "P1_4_4", "P1_4_5", "P1_4_9"),
  encoding = "Latin-1"
) %>%
  as_tibble() %>%
  mutate(
    ID_VIV = str_trim(as.character(ID_VIV)),
    across(c("P1_4_1","P1_4_2","P1_4_3","P1_4_4","P1_4_5","P1_4_9"), ~ suppressWarnings(as.numeric(.)))
  )

# ==== 7. Extract ID_VIV from ID_PER and merge with housing ====

data_2016_cleaned <- data_2016_cleaned %>%
  mutate(
    ID_VIV = str_extract(ID_PER, "^\\d+\\.\\d+"),
    ID_VIV = str_trim(as.character(ID_VIV))
  ) %>%
  left_join(tviv_2016, by = "ID_VIV") %>%
  mutate(
    acceso_digital = rowSums(select(., all_of(c("P1_4_2","P1_4_3","P1_4_5","P1_4_9"))) == 1, na.rm = TRUE)
  )
# ==== 7b. Renombrar columnas P13 -> P14 ====
data_2016_cleaned <- data_2016_cleaned %>%
  rename_with(~ str_replace(.x, "^P13", "P14"), starts_with("P13"))

# ==== 8. Save cleaned data ====
if (!dir.exists("../data/2016/cleaned")) {
  dir.create("../data/2016/cleaned", recursive = TRUE)
}
saveRDS(data_2016_cleaned, "../data/2016/cleaned/data_2016_cleaned.rds")
write_csv(data_2016_cleaned, "../data/2016/cleaned/data_2016_cleaned.csv")


