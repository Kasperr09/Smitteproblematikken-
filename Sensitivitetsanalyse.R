library(ggplot2)
library(dplyr)
library(scales)

# Data
df <- expand.grid(
  Forsinkelse = c(0, 3, 6, 9, 12),
  forrentningskrav = c("2,000%", "2,910%", "4,000%")
)

df$Værdi <- c(
  8.39, 8.31, 8.23, 8.15, 7.83,
  4.91, 4.83, 4.75, 4.67, 4.36,
  2.15, 2.07, 1.98, 1.90, 1.60
)

basecase <- 4.91

df <- df |>
  mutate(
    forrentningskrav = factor(forrentningskrav, levels = c("2,000%", "2,910%", "4,000%")),
    Forsinkelse = factor(Forsinkelse, levels = rev(c(0, 3, 6, 9, 12))),
    Label = paste0(format(round(Værdi, 2), decimal.mark = ","), "%")
  )

ggplot(df, aes(x = forrentningskrav, y = Forsinkelse, fill = Værdi)) +
  
  geom_tile(width = 0.985, height = 0.985, color = NA) +
  
  geom_text(
    aes(label = Label),
    family = "serif",
    fontface = "bold",
    size = 5.3,
    color = "#111827"
  ) +
  
  scale_fill_gradientn(
    colors = c(
      "#D77A7A",
      "#F3DFA2",
      "#3F7F45"
    ),
    values = rescale(c(
      min(df$Værdi),
      basecase,
      max(df$Værdi)
    )),
    limits = c(min(df$Værdi), max(df$Værdi)),
    guide = "none"
  ) +
  
  labs(
    title = "Sensitivitetsanalyse",
    subtitle = "Afkast ved variation i procestid og markedets forrentningskrav",
    x = "Markedets forrentningskrav",
    y = "Måneder forsinkelse",
    caption = "Note: Basecase er 4,91%. Værdier under basecase vises i røde nuancer, mens værdier over basecase vises i grønne nuancer."
  ) +
  
  coord_fixed(ratio = 0.72) +
  
  theme_minimal(base_family = "serif") +
  theme(
    plot.title = element_text(size = 26, face = "bold", color = "#111111"),
    plot.subtitle = element_text(size = 14.5, color = "#4B5563", margin = margin(b = 24)),
    
    axis.title.x = element_text(size = 14.5, face = "bold", margin = margin(t = 12)),
    axis.title.y = element_text(size = 14.5, face = "bold", margin = margin(r = 12)),
    
    axis.text.x = element_text(size = 12.5, face = "bold", color = "#374151", margin = margin(t = 8)),
    axis.text.y = element_text(size = 12.5, face = "bold", color = "#374151", margin = margin(r = 8)),
    
    plot.caption = element_text(size = 9, color = "#6B7280", hjust = 0, margin = margin(t = 18)),
    
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(35, 45, 28, 45)
  )