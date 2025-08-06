# ====================================
# Clean and prepare ENDIREH 2021 data
# ====================================

library(data.table)
library(tidyverse)
library(tibble)
library(stringr)

# ==== 1. Define columns to load ====

cols_emoc_exp <- paste0("P14_1_", 10:22)
cols_emoc_per <- paste0("P14_2_", 10:22)
cols_fis_exp <- paste0("P14_1_", 1:9)
cols_fis_per <- paste0("P14_2_", 1:9)
cols_all <- c("ID_PER", cols_emoc_exp, cols_emoc_per, cols_fis_exp, cols_fis_per)

# ==== 2. Load violence section ====

sec_xiv_2021 <- fread(
  "../data/2021/raw/conjunto_de_datos_TB_SEC_XIV/conjunto_de_datos/conjunto_de_datos_TB_SEC_XIV.csv",
  select = cols_all,
  encoding = "UTF-8"
) %>% 
  as_tibble() %>%
  mutate(across(-ID_PER, as.numeric))


# ==== 3. Create indicators ====

sec_xiv_2021 <- sec_xiv_2021 %>%
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

tsdem_2021 <- fread(
  "../data/2021/raw/conjunto_de_datos_TSDem/conjunto_de_datos/conjunto_de_datos_TSDem.csv",
  select = c("ID_PER", "EDAD", "GRA", "NOM_ENT"),
  encoding = "UTF-8"
) %>%
  mutate(
    ID_PER = str_trim(ID_PER),
    EDAD = as.numeric(EDAD),
    GRA = as.numeric(GRA),
    NOM_ENT = str_trim(NOM_ENT)
  ) %>%
  as_tibble()

# ==== 5. Merge and add age groups ====

data_2021_cleaned <- sec_xiv_2021 %>%
  mutate(ID_PER = str_trim(ID_PER)) %>%
  left_join(tsdem_2021, by = "ID_PER") %>%
  mutate(grupo_edad = case_when(
    EDAD < 20 ~ "<20",
    EDAD >= 20 & EDAD < 30 ~ "20-29",
    EDAD >= 30 & EDAD < 40 ~ "30-39",
    EDAD >= 40 & EDAD < 50 ~ "40-49",
    EDAD >= 50 & EDAD < 60 ~ "50-59",
    EDAD >= 60 & EDAD <= 80 ~ "60-80",
    EDAD > 80 ~ ">80",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(grupo_edad))



# ==== 6. Load housing data ====

tviv_2021 <- fread(
  "../data/2021/raw/conjunto_de_datos_TVIV/conjunto_de_datos/conjunto_de_datos_TVIV.csv",
  select = c("ID_VIV", "P1_4_1", "P1_4_2", "P1_4_3", "P1_4_4", "P1_4_5", "P1_4_9"),
  encoding = "Latin-1"
) %>%
  as_tibble() %>%
  mutate(across(-ID_VIV, as.numeric))

# ==== 6.5. Load children count from SEC XIII ====
sec_xiii_2021 <- fread(
  "../data/2021/raw/conjunto_de_datos_TB_SEC_XIII/conjunto_de_datos/conjunto_de_datos_TB_SEC_XIII.csv",
  select   = c("ID_PER", "P13_1"),
  encoding = "Latin-1"
) %>%
  as_tibble() %>%
  mutate(
    ID_PER = str_trim(ID_PER),
    hijos  = as.integer(P13_1)
  ) %>%
  select(ID_PER, hijos)


# ==== 7. Extract ID_VIV from ID_PER and merge with housing ====

# 1. Extraer ID_VIV desde ID_PER
data_2021_cleaned <- data_2021_cleaned %>%
  mutate(ID_VIV = str_extract(ID_PER, "^\\d+\\.\\d+"))

# Limpiar ambos ID_VIV
tviv_2021 <- tviv_2021 %>%
  mutate(ID_VIV = str_trim(as.character(ID_VIV)))

data_2021_cleaned <- data_2021_cleaned %>%
  mutate(ID_VIV = str_trim(as.character(ID_VIV)))

# 2. Unir con los datos de vivienda (tviv)
data_2021_cleaned <- data_2021_cleaned %>%
  left_join(tviv_2021, by = "ID_VIV")

# 3. Crear variable de acceso digital
data_2021_cleaned <- data_2021_cleaned %>%
  mutate(
    acceso_digital = rowSums(
      select(., all_of(c("P1_4_1", "P1_4_2", "P1_4_3", "P1_4_4", "P1_4_5", "P1_4_9"))) == 1,
      na.rm = TRUE
    )
  )


# ==== 8. Save cleaned data ====
if (!dir.exists("../data/2021/cleaned")) {
  dir.create("../data/2021/cleaned", recursive = TRUE)
}

write_rds(data_2021_cleaned, "../data/2021/cleaned/data_2021_cleaned.rds")
write_csv(data_2021_cleaned, "../data/2021/cleaned/data_2021_cleaned.csv")

cat("✅ Cleaned dataset with acceso_digital saved as .rds and .csv\n")










