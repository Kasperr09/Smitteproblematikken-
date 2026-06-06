# Pakker
library(ggplot2)
library(dplyr)
library(scales)

# Data
df <- data.frame(
  Investorprofil = c("Familieformue", "Mindre ejendomsselskab", "Stor ejendomsfond"),
  Samlet_portefolje = c(300000000, 1000000000, 10000000000),
  Ejendommens_vaegt = c(81.66, 24.50, 2.45),
  Tab_ejendom = c(11046261, 11046261, 11046261),
  Tab_portefolje = c(3.68, 1.10, 0.11)
)

# Rækkefølge i figuren
df$Investorprofil <- factor(
  df$Investorprofil,
  levels = c("Familieformue", "Mindre ejendomsselskab", "Stor ejendomsfond")
)

# Labels + dynamisk placering af tekst
df <- df %>%
  mutate(
    vaegt_label = paste0(gsub("\\.", ",", round(Ejendommens_vaegt, 2)), " %"),
    tab_label = paste0(gsub("\\.", ",", round(Tab_portefolje, 2)), " %"),
    portefolje_label = case_when(
      Samlet_portefolje == 300000000    ~ "300 mio. kr.",
      Samlet_portefolje == 1000000000   ~ "1 mia. kr.",
      Samlet_portefolje == 10000000000  ~ "10 mia. kr."
    ),
    info_label = paste0(
      "Ejendommens vægt: ", vaegt_label, "\n",
      "Samlet portefølje: ", portefolje_label
    ),
    
    # Her styres placeringen af teksten inde i/ved søjlerne
    # Lavere værdi = længere ned ad y-aksen
    info_y = case_when(
      Investorprofil == "Stor ejendomsfond" ~ 0.06,
      TRUE ~ Tab_portefolje / 2
    ),
    
    info_size = case_when(
      Investorprofil == "Stor ejendomsfond" ~ 3.0,
      TRUE ~ 3.5
    )
  )

# Figur
ggplot(df, aes(x = Investorprofil, y = Tab_portefolje)) +
  geom_col(
    fill  = "#AEB8BF",
    color = "#6F7C85",
    width = 0.58
  ) +
  
  # Procentlabel over søjlerne
  geom_text(
    aes(label = tab_label),
    vjust    = -0.7,
    size     = 4,
    fontface = "bold",
    color    = "black"
  ) +
  
  # Infotekst om ejendommens vægt og samlet portefølje
  geom_text(
    aes(
      y     = info_y,
      label = info_label,
      size  = info_size
    ),
    color      = "black",
    lineheight = 0.9
  ) +
  
  scale_size_identity() +
  
  scale_y_continuous(
    labels = function(x) paste0(gsub("\\.", ",", x), " %"),
    limits = c(0, 4.2),
    breaks = seq(0, 4, 0.5),
    expand = expansion(mult = c(0, 0.08))
  ) +
  
  labs(
    title    = "Porteføljemæssig effekt af regulatorisk risiko",
    subtitle = "Illustration af værdifaldets relative betydning på tværs af investortyper",
    x        = "Investorprofil",
    y        = "Tab af samlet portefølje",
    caption  = "Note: Figuren viser effekten af et værdifald på 11,05 mio. kr. ved 12 måneders procesforsinkelse. Ejendommens værdi før risikopræmie udgør 244,99 mio. kr."
  ) +
  
  theme_minimal(base_family = "serif") +
  
  theme(
    plot.title         = element_text(face = "bold", size = 16, hjust = 0),
    plot.subtitle      = element_text(size = 10, hjust = 0, color = "grey30"),
    axis.title.x       = element_text(size = 10, face = "bold", margin = margin(t = 12)),
    axis.title.y       = element_text(size = 10, face = "bold", margin = margin(r = 12)),
    axis.text.x        = element_text(size = 10, color = "black"),
    axis.text.y        = element_text(size = 9, color = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(color = "grey88", linewidth = 0.35),
    plot.caption       = element_text(size = 8, hjust = 0, color = "grey30"),
    plot.margin        = margin(15, 20, 15, 20)
  )
