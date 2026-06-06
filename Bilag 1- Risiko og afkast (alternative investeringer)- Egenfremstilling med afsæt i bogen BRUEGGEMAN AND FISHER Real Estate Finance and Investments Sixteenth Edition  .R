library(ggplot2)

# Data
x <- seq(0, 10, length.out = 300)

# Konkav potensfunktion
y_curve <- 2 + 1.2 * x
# Riskless rate
y_rf <- rep(2, length(x))

df <- data.frame(x, y_curve, y_rf)

# Plot
ggplot(df, aes(x = x)) +
  
  # Riskless rate
  geom_line(aes(y = y_rf), linewidth = 0.6) +
  
  # Konkav kurve
  geom_line(aes(y = y_curve), linewidth = 0.6) +
  
  # Labels
  annotate("text", x = 0.5, y = 4.2, label = "Statsobligationer (kort)", hjust = 0, fontface = "bold") +
  annotate("text", x = 1.2, y = 5.1, label = "Kommunale obligationer", hjust = 0, fontface = "bold") +
  annotate("text", x = 2, y = 6, label = "Realkreditobligationer", hjust = 0, fontface = "bold") +
  annotate("text", x = 3, y = 7, label = "Virksomhedsobligationer", hjust = 0, fontface = "bold") +
  annotate("text", x = 4.5, y = 8.5, label = "Fast ejendom", hjust = 0, fontface = "bold") +
  annotate("text", x = 6, y = 9.8, label = "Aktier", hjust = 0, fontface = "bold") +
  annotate("text", x = 6.5, y = 2.4, label = "Risikofri rente", hjust = 0, fontface = "bold") +
  
  # Akser
  scale_x_continuous(limits = c(0, 8), expand = c(0, 0)) +
  scale_y_continuous(
    limits = c(0, 10),
    breaks = seq(0, 8, 2),   # <- 0, 2, 4, 6, 8
    expand = c(0, 0)
  ) +
  
  # Stilrent layout
  theme_classic(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    
    # Fjern x-akse
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    
    # Behold y-akse
    axis.text.y = element_text(),
    axis.ticks.y = element_line()
  ) +
  
  labs(
    x = "Risiko",
    y = "Afkast (%)"
  )