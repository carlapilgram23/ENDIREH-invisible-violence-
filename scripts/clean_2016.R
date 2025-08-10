# ====================================
# Clean and prepare ENDIREH 2016 data
# ====================================

library(data.table)
library(tidyverse)
library(tibble)
library(stringr)

# ==== 1. Define columns to load ====

cols_emoc_exp <- paste0("P13_1_", 10:22)
cols_emoc_per <- paste0("P13_2_", 10:22)
cols_fis_exp <- paste0("P13_1_", 1:9)
cols_fis_per <- paste0("P13_2_", 1:9)
cols_all <- c("ID_MUJ", cols_emoc_exp, cols_emoc_per, cols_fis_exp, cols_fis_per)

# ==== 2. Load violence section ====

sec_xiii_2016 <- fread(
  "../data/2016/raw/conjunto_de_datos_tb_sec_xiii_endireh_2016/conjunto_de_datos/conjunto_de_datos_tb_sec_xiii_endireh_2016.csv",
  select = cols_all,
  encoding = "UTF-8"
) %>% 
  as_tibble() %>%
  mutate(across(-ID_MUJ, as.numeric))


# ==== 3. Create indicators ====

sec_xiii_2016 <- sec_xiii_2016 %>%
  mutate(
    violencia_emocional_vivida = pmap_lgl(select(., all_of(cols_emoc_exp)),
                                          ~ any(c(...) %in% 1:3, na.rm = TRUE)),
    
    violencia_emocional_normalizada = pmap_lgl(select(., all_of(c(cols_emoc_exp, cols_emoc_per))),
                                               function(...) {
                                                 valores <- list(...)
                                                 exp_vals <- valores[1:length(cols_emoc_exp)]
                                                 per_vals <- valores[(length(cols_emoc_exp) + 1):length(valores)]
                                                 any(mapply(function(e, p) !is.na(e) && e %in% 1:3 && p == 3, exp_vals, per_vals))
                                               }),
    
    violencia_fisica_vivida = pmap_lgl(select(., all_of(cols_fis_exp)),
                                       ~ any(c(...) %in% 1:3, na.rm = TRUE)),
    
    violencia_fisica_normalizada = pmap_lgl(select(., all_of(c(cols_fis_exp, cols_fis_per))),
                                            function(...) {
                                              valores <- list(...)
                                              exp_vals <- valores[1:length(cols_fis_exp)]
                                              per_vals <- valores[(length(cols_fis_exp) + 1):length(valores)]
                                              any(mapply(function(e, p) !is.na(e) && e %in% 1:3 && p == 3, exp_vals, per_vals))
                                            })
  )

# ==== 4. Load sociodemographic data ====

tsdem_2016 <- fread(
  "../data/2016/raw/conjunto_de_datos_tsdem_endireh_2016/conjunto_de_datos/conjunto_de_datos_tsdem_endireh_2016.csv",
  select = c("ID_MUJ", "EDAD", "GRA", "NOM_ENT"),
  encoding = "UTF-8"
) %>%
  mutate(
    ID_MUJ = str_trim(ID_MUJ),
    EDAD = as.numeric(EDAD),
    GRA = as.numeric(GRA),
    NOM_ENT = str_trim(NOM_ENT)
  ) %>%
  as_tibble()

# ==== 5. Merge and add age groups ====

data_2016_cleaned <- sec_xiii_2016 %>%
  mutate(ID_MUJ = str_trim(ID_MUJ)) %>%
  left_join(tsdem_2016, by = "ID_MUJ") %>%
  mutate(grupo_edad = case_when(
    EDAD < 20 ~ "<20",
    EDAD >= 20 & EDAD < 30 ~ "20-29",
    EDAD >= 30 & EDAD < 40 ~ "30-39",
    EDAD >= 40 & EDAD < 50 ~ "40-49",
    EDAD >= 50 & EDAD < 60 ~ "50-59",
    EDAD >= 60 & EDAD < 80 ~ "60-79",
    EDAD > 80 ~ ">80",
    TRUE ~ NA_character_
  ),
  # Variable binaria de educación: 1 si GRA = 9, 0 si menor
  GRA_bin = if_else(GRA == 8, 1, 0, missing = NA_real_)
  ) %>%
  filter(!is.na(grupo_edad))






# ==== 6. Load housing data ====

tviv_2016 <- fread(
  "../data/2016/raw/conjunto_de_datos_tviv_endireh_2016/conjunto_de_datos/conjunto_de_datos_tviv_endireh_2016.csv",
  select = c("ID_VIV", "P1_4_1", "P1_4_2", "P1_4_3", "P1_4_4", "P1_4_5", "P1_4_9"),
  encoding = "Latin-1"
) %>%
  as_tibble() %>%
  mutate(across(-ID_VIV, as.numeric))


# ==== 7. Extract ID_VIV from ID_MUJ and merge with housing ====

# 1. Extraer ID_VIV desde ID_MUJ
data_2016_cleaned <- data_2016_cleaned %>%
  mutate(ID_VIV = str_extract(ID_MUJ, "^\\d+\\.\\d+"))

# Limpiar ambos ID_VIV
tviv_2016 <- tviv_2016 %>%
  mutate(ID_VIV = str_trim(as.character(ID_VIV)))

data_2016_cleaned <- data_2016_cleaned %>%
  mutate(ID_VIV = str_trim(as.character(ID_VIV)))

# 2. Unir con los datos de vivienda (tviv)
data_2016_cleaned <- data_2016_cleaned %>%
  left_join(tviv_2016, by = "ID_VIV")

# 3. Crear variable de acceso digital
data_2016_cleaned <- data_2016_cleaned %>%
  mutate(
    acceso_digital = rowSums(
      select(., all_of(c("P1_4_2", "P1_4_3", "P1_4_5", "P1_4_9"))) == 1,
      na.rm = TRUE
    )
  )


# ==== 8. Save cleaned data ====
if (!dir.exists("../data/2016/cleaned")) {
  dir.create("../data/2016/cleaned", recursive = TRUE)
}

write_rds(data_2016_cleaned, "../data/2016/cleaned/data_2016_cleaned.rds")
write_csv(data_2016_cleaned, "../data/2016/cleaned/data_2016_cleaned.csv")

cat("✅ Cleaned dataset with acceso_digital saved as .rds and .csv\n")










