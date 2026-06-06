# Pakker
library(ggplot2)
library(dplyr)
library(scales)
library(tibble)
library(grid)

# -----------------------------
# 1. Data
# -----------------------------

df <- tibble(
  komponent = c(
    "Inflation",
    "Realrente / resterende del",
    "Markedsrisikopræmie",
    "Illikviditetspræmie",
    "Reguleringsrisikopræmie"
  ),
  vaerdi = c(
    2.00,
    0.86,
    1.25,
    0.80,
    0.24
  )
)

# -----------------------------
# 2. Rækkefølge i søjlen
# nederst -> øverst
# -----------------------------

df <- df %>%
  mutate(
    komponent = factor(
      komponent,
      levels = c(
        "Inflation",
        "Realrente / resterende del",
        "Markedsrisikopræmie",
        "Illikviditetspræmie",
        "Reguleringsrisikopræmie"
      )
    )
  ) %>%
  arrange(komponent)

# -----------------------------
# 3. Label-justeringer
# negative tal = ned
# positive tal = op
# -----------------------------

label_justeringer <- tibble(
  komponent = c(
    "Inflation",
    "Realrente / resterende del",
    "Markedsrisikopræmie",
    "Illikviditetspræmie",
    "Reguleringsrisikopræmie"
  ),
  justering = c(
    2.40,
    -1.70,
    -1.70,
    0.30,
    -4.90
  )
)

# -----------------------------
# 4. Beregninger
# -----------------------------

df <- df %>%
  mutate(
    ymax = cumsum(vaerdi),
    ymin = lag(ymax, default = 0),
    ypos_base = (ymin + ymax) / 2
  ) %>%
  left_join(label_justeringer, by = "komponent") %>%
  mutate(
    justering = ifelse(is.na(justering), 0, justering),
    ypos = ypos_base + justering,
    label = paste0(
      number(vaerdi, accuracy = 0.01, decimal.mark = ","),
      " %"
    )
  )

samlet_afkast <- sum(df$vaerdi)

# -----------------------------
# 5. Farver
# -----------------------------

farver <- c(
  "Inflation"                    = "#1F4A9A",
  "Realrente / resterende del"   = "#3B6CB7",
  "Markedsrisikopræmie"          = "#159E95",
  "Illikviditetspræmie"          = "#A8CBB3",
  "Reguleringsrisikopræmie"      = "#79C143"
)

# -----------------------------
# 6. Plot-indstillinger
# -----------------------------

x_pos <- 1.18

p <- ggplot(df, aes(x = x_pos, y = vaerdi, fill = komponent)) +
  
  geom_col(
    width = 0.26,
    colour = "white",
    linewidth = 0.8
  ) +
  
  geom_text(
    aes(y = ypos, label = label),
    colour = "white",
    fontface = "bold",
    size = 5
  ) +
  
  annotate(
    "text",
    x = x_pos,
    y = samlet_afkast + 0.25,
    label = paste0(
      "Samlet afkastkrav: ",
      number(samlet_afkast, accuracy = 0.01, decimal.mark = ","),
      " %"
    ),
    fontface = "bold",
    size = 5.2
  ) +
  
  scale_fill_manual(values = farver) +
  
  scale_y_continuous(
    limits = c(0, 6),
    breaks = seq(0, 6, by = 1),
    labels = function(x) paste0(
      number(x, accuracy = 1, decimal.mark = ","),
      " %"
    ),
    expand = c(0, 0)
  ) +
  
  scale_x_continuous(
    limits = c(0.72, 1.62),
    breaks = NULL
  ) +
  
  labs(
    title = "Egen illustration: Sammensætning af diskonteringsrenten",
    subtitle = "Build-up-model baseret på inflation, realrente og selvstændige risikopræmier",
    y = "Årligt afkast (%)",
    x = NULL,
    fill = NULL,
    caption = paste(
      "Note: Den risikofri rente er opdelt i inflation på 2,00 procentpoint og en resterende realrente på 0,86 procentpoint.",
      "Reguleringsrisikopræmien er fastsat til 0,24 procentpoint på baggrund af den estimerede effekt af udmatrikuleringsprocessen."
    )
  ) +
  
  coord_cartesian(clip = "off") +
  
  theme_minimal(base_size = 14) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 21,
      hjust = 0,
      margin = margin(b = 6)
    ),
    
    plot.subtitle = element_text(
      size = 13,
      hjust = 0,
      margin = margin(b = 18)
    ),
    
    plot.caption = element_text(
      size = 9.5,
      hjust = 0,
      margin = margin(t = 14)
    ),
    
    axis.title.y = element_text(
      size = 15,
      margin = margin(r = 16)
    ),
    
    axis.text.y = element_text(
      size = 12,
      colour = "#4F4F4F"
    ),
    
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    panel.background = element_rect(
      fill = "white",
      colour = NA
    ),
    
    plot.background = element_rect(
      fill = "white",
      colour = NA
    ),
    
    legend.position = c(0.70, 0.82),
    legend.justification = c(0, 1),
    legend.direction = "vertical",
    
    legend.text = element_text(size = 13),
    legend.key.size = unit(0.9, "cm"),
    legend.spacing.y = unit(0.15, "cm"),
    
    plot.margin = margin(20, 30, 20, 20)
  ) +
  
  guides(
    fill = guide_legend(ncol = 1, byrow = TRUE)
  )

print(p)

# -----------------------------
# 7. Gem figur
# -----------------------------

ggsave(
  filename = "diskonteringsrente_build_up_model.png",
  plot = p,
  width = 11,
  height = 8,
  dpi = 320,
  bg = "white"
)