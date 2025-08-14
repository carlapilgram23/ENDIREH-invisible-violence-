# exploratory_subgroups.R
library(tidyverse)
library(readr)
library(forcats)
library(emmeans)
library(ggplot2)
library(scales)

data_vict <- read_rds("../data/combined/data_combined.rds") %>%
  filter(violencia_emocional_vivida %in% c(1, TRUE)) %>%
  mutate(
    grupo_edad = if ("60-80" %in% levels(grupo_edad)) fct_recode(grupo_edad, "60-79" = "60-80") else grupo_edad,
    grupo_edad = fct_relevel(grupo_edad, "60-79","<20","20-29","30-39","40-49","50-59",">80"),
    year       = factor(year, levels = c(2016, 2021)),
    GRA_bin    = factor(GRA_bin, levels = c(0,1), labels = c("No","Yes")),
    P1_4_9     = factor(P1_4_9, levels = c(2,1), labels = c("No","Yes"))
  ) %>%
  drop_na(violencia_emocional_reconocida, year, grupo_edad, GRA_bin, P1_4_9)

m <- glm(
  violencia_emocional_reconocida ~ 
    year * (grupo_edad + P1_4_9 + GRA_bin) + 
    grupo_edad * P1_4_9 + GRA_bin * P1_4_9,
  data = data_vict, family = binomial()
)

# EMMs por año dentro de cada subgrupo → escala de probabilidad
emm_sub  <- emmeans(m, ~ year | grupo_edad + P1_4_9 + GRA_bin)
emm_resp <- regrid(emm_sub, transform = "response")

# Δ pp (2021−2016) con IC95% y BH para múltiples comparaciones
pairs_df <- as.data.frame(summary(pairs(emm_resp, reverse = TRUE), infer = TRUE))
lo <- intersect(c("lower.CL","asymp.LCL"), names(pairs_df))[1]
hi <- intersect(c("upper.CL","asymp.UCL"), names(pairs_df))[1]

deltas <- pairs_df %>%
  transmute(
    grupo_edad, P1_4_9, GRA_bin,
    delta_pp = 100*estimate,
    lower_pp = 100*.data[[lo]],
    upper_pp = 100*.data[[hi]],
    p.value,
    p_adj_BH = p.adjust(p.value, method = "BH")
  ) %>%
  mutate(
    Education = recode(GRA_bin, "No"="Lower education", "Yes"="Higher education"),
    Internet  = recode(P1_4_9, "No"="No internet", "Yes"="Internet"),
    grupo_edad = factor(grupo_edad, levels = c("<20","20-29","30-39","40-49","50-59","60-79",">80"))
  )

# Guardar tabla
dir.create("../results/combined/tables", recursive = TRUE, showWarnings = FALSE)
write_csv(deltas, "../results/combined/tables/delta_pp_by_subgroup.csv")

# Gráfico: Δ pp por Edad × Internet (facetas por Educación)
deltas_plot <- deltas %>%
  mutate(ypos = if_else(delta_pp >= 0, upper_pp + 0.35, lower_pp - 0.35),
         sig = case_when(
           p_adj_BH < 0.001 ~ "***",
           p_adj_BH < 0.01  ~ "**",
           p_adj_BH < 0.05  ~ "*",
           p_adj_BH < 0.10  ~ "•",
           TRUE ~ ""
         ))

p <- ggplot(deltas_plot, aes(x=grupo_edad, y=delta_pp, fill=Internet)) +
  geom_hline(yintercept = 0, linetype="dashed") +
  geom_col(position = position_dodge(width=.7), width=.6) +
  geom_errorbar(aes(ymin=lower_pp, ymax=upper_pp),
                position = position_dodge(width=.7), width=.18) +
  geom_text(aes(y=ypos, label=sig),
            position = position_dodge(width=.7), size=4) +
  facet_wrap(~ Education, nrow = 1) +
  labs(title="Adjusted change 2021 − 2016 by Age × Internet",
       subtitle="Outcome: recognising emotional violence (victims only)",
       x="Age group", y="Δ percentage points (pp)", fill="Internet access",
       caption="Significance: *** p<0.001, ** p<0.01, * p<0.05, • p<0.10 (BH-adjusted)") +
  theme_minimal(base_size = 12)

dir.create("../results/combined/plots", recursive = TRUE, showWarnings = FALSE)
ggsave("../results/combined/plots/delta_pp_by_subgroup.png", p, width=11, height=5.5, dpi=300)
