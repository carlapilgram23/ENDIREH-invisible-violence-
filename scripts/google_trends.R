#install.packages("gtrendsR")

library(gtrendsR)
library(tidyverse)

# Define keywords and time

keywords <- c("violencia de género", "Ni una menos", "8M", "Marcha 8M")
time_range <- "2015-01-01 2021-12-31"
geo <- "MX"

# Download trends
trends <- gtrends(keyword = keywords, geo = geo, time = time_range)

# Clean temporal serie
trend_data <- trends$interest_over_time %>%
  select(date, hits, keyword) %>%
  mutate(hits = ifelse(hits == "<1", 0, as.numeric(hits)))

# Graph

trends_plot <- ggplot(trend_data, aes(x = date, y = hits, color = keyword)) +
  geom_line(size = 1) +
  labs(
    title = "Interest in femenism movements and gender violence",
    subtitle = "Source: Google Trends (2015–2021)",
    x = "Date",
    y = "Relative Interest (0–100)",
    color = "Search term"
  ) +
  theme_minimal()

# Guardar en una carpeta llamada "resultados"
ggsave("../results/combined/plots/trends_plot.png", plot = trends_plot, width = 6, height = 4, units = "in")
