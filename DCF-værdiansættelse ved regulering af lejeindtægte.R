library(ggplot2)
library(scales)

# Data
df <- data.frame(
  scenarie = factor(
    c("0 ejendomme", "1 ejendom", "5 ejendomme", "10 ejendomme", "20 ejendomme"),
    levels = c("0 ejendomme", "1 ejendom", "5 ejendomme", "10 ejendomme", "20 ejendomme")
  ),
  vaerdi = c(244986880, 242895731, 234531138, 224075397, 203163915),
  ændring_pct = c(NA, -0.85, -3.44, -4.46, -9.33)
)

# Dansk talformat
dk_number <- function(x) {
  paste0(
    format(round(x, 0), big.mark = ".", decimal.mark = ",", scientific = FALSE),
    " kr."
  )
}

# Labels
df$label <- dk_number(df$vaerdi)

df$pct_label <- ifelse(
  is.na(df$ændring_pct),
  "",
  paste0(
    "-",
    formatC(abs(df$ændring_pct), format = "f", digits = 2, decimal.mark = ","),
    "%"
  )
)

# Dynamisk y-akse
y_min <- floor(min(df$vaerdi) / 10000000) * 10000000
y_max <- ceiling(max(df$vaerdi) / 10000000) * 10000000

ggplot(df, aes(x = scenarie, y = vaerdi, group = 1)) +
  
  geom_line(
    color = "#1F354D",
    linewidth = 0.9
  ) +
  
  geom_point(
    shape = 21,
    size = 5,
    fill = "#AEB8BF",
    color = "#6F7F87",
    stroke = 0.8
  ) +
  
  # Prislabels
  geom_text(
    aes(label = label),
    nudge_y = 4500000,
    size = 3.3,
    family = "serif",
    fontface = "bold",
    color = "black"
  ) +
  
  # Procentlabels
  geom_text(
    aes(label = pct_label),
    nudge_y = -4500000,
    size = 3,
    family = "serif",
    color = "#1F6FB2",
    fontface = "bold"
  ) +
  
  scale_y_continuous(
    limits = c(y_min - 5000000, y_max + 10000000),
    breaks = seq(y_min, y_max, by = 10000000),
    labels = dk_number,
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  
  labs(
    title = "DCF-værdiansættelse ved regulering af lejeindtægter",
    subtitle = "Illustration af værdifald ved indbringelse af sager for huslejenævnet",
    x = "Antal ejendomme med medhold i huslejenævnet",
    y = "Estimeret ejendomsværdi",
    caption = "Note: Figuren illustrerer ændringen i værdiansættelsen, når fremtidige lejeindtægter reguleres som følge af medhold i huslejenævnet."
  ) +
  
  theme_minimal(base_family = "serif") +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 9.5, color = "gray35", hjust = 0),
    plot.caption = element_text(size = 7.5, color = "gray35", hjust = 0),
    
    axis.title.x = element_text(size = 9, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 9, face = "bold", margin = margin(r = 10)),
    axis.text.x = element_text(size = 8.5, color = "gray20"),
    axis.text.y = element_text(size = 8.5, color = "gray20"),
    
    panel.grid.major.y = element_line(color = "gray88", linewidth = 0.4),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    
    axis.line.x = element_line(color = "gray50", linewidth = 0.4),
    axis.line.y = element_line(color = "gray50", linewidth = 0.4),
    
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    
    plot.margin = margin(15, 25, 15, 25)
  )