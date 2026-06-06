library(ggplot2)
library(dplyr)
library(scales)
library(tibble)
library(ggrepel)
library(stringr)

# -------------------------------------------------
# 1. Grundlag
# -------------------------------------------------
risikofri_rente <- 2.86
markedsrisikopraemie <- 1.25
illikviditetspraemie <- 0.80
juridisk_praemie_lav <- 0.24
juridisk_praemie_hoj <- 0.55

# -------------------------------------------------
# 2. Data
# -------------------------------------------------
risici <- tibble(
  risiko = c(
    "10-årig statsobligation",
    "Markedsrisikopræmie",
    "Illikviditetspræmie",
    "Juridisk risikopræmie (lav)",
    "Juridisk risikopræmie (høj)"
  ),
  praemie = c(
    risikofri_rente,
    markedsrisikopraemie,
    illikviditetspraemie,
    juridisk_praemie_lav,
    juridisk_praemie_hoj
  )
) %>%
  mutate(
    impact_vaerdi = praemie / (praemie + 1) * 100,
    label = paste0(
      risiko,
      "\n",
      number(praemie, accuracy = 0.01, decimal.mark = ","),
      " %"
    )
  )

# -------------------------------------------------
# 3. Baggrundsgrid
# -------------------------------------------------
grid <- expand.grid(
  impact_bin = seq(5, 75, by = 10),
  praemie_bin = seq(0.25, 2.75, by = 0.5)
)

grid$score <- grid$impact_bin * grid$praemie_bin

# -------------------------------------------------
# 4. Plot
# -------------------------------------------------
p <- ggplot() +
  
  geom_tile(
    data = grid,
    aes(x = impact_bin, y = praemie_bin, fill = score),
    color = "grey85",
    linewidth = 0.3
  ) +
  
  geom_point(
    data = risici,
    aes(x = impact_vaerdi, y = praemie),
    size = 3.2,
    color = "black"
  ) +
  
  geom_text_repel(
    data = risici,
    aes(x = impact_vaerdi, y = praemie, label = label),
    size = 4.0,
    fontface = "bold",
    family = "serif",
    box.padding = 0.55,
    point.padding = 0.35,
    min.segment.length = 0,
    segment.color = "grey40",
    segment.size = 0.4,
    seed = 123,
    max.overlaps = Inf
  ) +
  
  scale_fill_gradientn(
    colours = c("#d8f3dc", "#95d5b2", "#ffd166", "#f4a261", "#e63946"),
    guide = "none"
  ) +
  
  scale_x_continuous(
    breaks = seq(0, 80, by = 10),
    labels = function(x) paste0(number(x, accuracy = 1, decimal.mark = ","), " %"),
    limits = c(0, 80),
    expand = c(0, 0)
  ) +
  
  scale_y_continuous(
    breaks = seq(0, 3.0, by = 0.5),
    labels = function(x) paste0(number(x, accuracy = 0.1, decimal.mark = ","), " %"),
    limits = c(0, 3.1),
    expand = c(0, 0)
  ) +
  
  labs(
    title = "Risikomatrix for diskonteringsrentens præmier",
    subtitle = "Sammenstilling af præmiestørrelse og estimeret relativ værdipåvirkning",
    x = "Estimeret relativ værdipåvirkning ved isoleret komponenttillæg (%)",
    y = "Komponentens størrelse (%)",
    caption = str_wrap(
      "Note: Figuren viser diskonteringsrentens præmier positioneret efter størrelse og estimeret relativ værdipåvirkning. En højere komponent medfører alt andet lige en større negativ påvirkning af værdiansættelsen. Den juridiske risikopræmie er vist i et lavt scenarie på 0,21 procentpoint og et højt scenarie på 0,51 procentpoint.",
      width = 115
    )
  ) +
  
  theme_minimal(base_size = 13, base_family = "serif") +
  theme(
    panel.grid = element_blank(),
    
    axis.title = element_text(
      face = "bold",
      size = 13
    ),
    axis.text = element_text(
      color = "black",
      size = 11
    ),
    
    plot.title = element_text(
      face = "bold",
      size = 16,
      hjust = 0.5,
      margin = margin(b = 6)
    ),
    plot.subtitle = element_text(
      size = 11.5,
      hjust = 0.5,
      margin = margin(b = 14)
    ),
    plot.caption = element_text(
      size = 9.5,
      hjust = 0,
      lineheight = 1.15,
      margin = margin(t = 14)
    ),
    
    plot.margin = margin(20, 45, 55, 20)
  )

print(p)

# -------------------------------------------------
# 5. Gem figur
# -------------------------------------------------
ggsave(
  filename = "risikomatrix_diskonteringsrente.png",
  plot = p,
  width = 13,
  height = 8.5,
  dpi = 320,
  bg = "white"
)