# Pakker
install.packages("ggplot2")
install.packages("tidyr")
install.packages("dplyr")
install.packages("scales")
install.packages("gridExtra")

library(ggplot2)
library(tidyr)
library(dplyr)
library(scales)
library(gridExtra)
library(grid)

# ---------------------------------------------------------
# 1. Data
# ---------------------------------------------------------

data <- data.frame(
  Maaneder_forsinkelse = c(0, 3, 6, 9, 12),
  Pessimistisk = c(0.00, 0.08, 0.16, 0.24, 0.55),
  Base = c(0.00, 0.08, 0.16, 0.24, 0.55),
  Optimistisk = c(0.00, 0.08, 0.16, 0.24, 0.55)
)

data_lang <- data %>%
  pivot_longer(
    cols = c(Pessimistisk, Base, Optimistisk),
    names_to = "Scenarie",
    values_to = "Risikopraemie"
  ) %>%
  mutate(
    Label = paste0(
      formatC(Risikopraemie, format = "f", digits = 2, decimal.mark = ","),
      "%"
    )
  )

# Label-data:
# Én label pr. måned og procentværdi
label_data <- data_lang %>%
  group_by(Maaneder_forsinkelse, Risikopraemie, Label) %>%
  summarise(
    Scenarier = paste(Scenarie, collapse = ", "),
    .groups = "drop"
  ) %>%
  mutate(
    vjust_label = case_when(
      Maaneder_forsinkelse == 6 & Risikopraemie == 0.15 ~ -1.1,
      Maaneder_forsinkelse == 6 & Risikopraemie == 0.14 ~ 1.8,
      Maaneder_forsinkelse == 9 & Risikopraemie == 0.22 ~ -1.1,
      Maaneder_forsinkelse == 9 & Risikopraemie == 0.21 ~ 1.8,
      TRUE ~ -1.1
    )
  )

# ---------------------------------------------------------
# 2. Lille scenarie-tabel øverst
# ---------------------------------------------------------

scenarie_tabel <- data.frame(
  Sagstype = "Ejendomsmatrikulering",
  Pessimistisk = "+ 12 måneder",
  Base = "9 måneder",
  Optimistisk = "6 måneder"
)

tabel_theme <- ttheme_minimal(
  core = list(
    fg_params = list(
      fontsize = 11,
      fontfamily = "serif",
      col = "#0B1F3A"
    ),
    bg_params = list(fill = "#F7F7F7", col = "#C8CDD2")
  ),
  colhead = list(
    fg_params = list(
      fontsize = 11,
      fontface = "bold",
      fontfamily = "serif",
      col = "#0B1F3A"
    ),
    bg_params = list(fill = "#ECEFF1", col = "#C8CDD2")
  )
)

tabel_grob <- tableGrob(
  scenarie_tabel,
  rows = NULL,
  theme = tabel_theme
)

# ---------------------------------------------------------
# 3. Graf
# ---------------------------------------------------------

graf <- ggplot(
  data_lang,
  aes(
    x = Maaneder_forsinkelse,
    y = Risikopraemie,
    group = Scenarie,
    color = Scenarie,
    linetype = Scenarie
  )
) +
  geom_line(linewidth = 1.1) +
  geom_point(
    size = 3.6,
    shape = 21,
    fill = "#C9D1D8",
    stroke = 0.9
  ) +
  geom_text(
    data = label_data,
    aes(
      x = Maaneder_forsinkelse,
      y = Risikopraemie,
      label = Label,
      vjust = vjust_label
    ),
    inherit.aes = FALSE,
    size = 3.5,
    family = "serif",
    color = "#0B1F3A"
  ) +
  scale_color_manual(
    values = c(
      "Pessimistisk" = "#0B1F3A",
      "Base" = "#385A7C",
      "Optimistisk" = "#6F87A3"
    )
  ) +
  scale_linetype_manual(
    values = c(
      "Pessimistisk" = "solid",
      "Base" = "dashed",
      "Optimistisk" = "dotted"
    )
  ) +
  scale_x_continuous(
    breaks = c(0, 3, 6, 9, 12),
    limits = c(0, 12)
  ) +
  scale_y_continuous(
    limits = c(0, 0.55),
    breaks = c(0, 0.10, 0.20, 0.30, 0.40, 0.50, 0.55),
    labels = function(x) paste0(
      formatC(x, format = "f", digits = 2, decimal.mark = ","),
      "%"
    )
  ) +
  labs(
    title = "Risikopræmie ved forsinkelse – Ejendomsmatrikulering",
    subtitle = "Illustration af risikopræmie ved forskellige scenarier og markedets afkastkrav",
    x = "Måneder forsinkelse",
    y = "Risikopræmie (%)",
    color = NULL,
    linetype = NULL,
    caption = paste(
      "Note: Figuren illustrerer udviklingen i risikopræmien ved forskellige forsinkelsesscenarier for ejendomsmatrikulering.",
      "Beregningen er baseret på tre scenarier for markedets afkastkrav."
    )
  ) +
  theme_minimal(base_family = "serif") +
  theme(
    plot.background = element_rect(fill = "#FAFAF8", color = NA),
    panel.background = element_rect(fill = "#FAFAF8", color = NA),
    
    plot.title = element_text(
      face = "bold",
      size = 17,
      color = "#111111",
      margin = margin(b = 4)
    ),
    plot.subtitle = element_text(
      size = 10.5,
      color = "#4F5B66",
      margin = margin(b = 18)
    ),
    
    axis.title = element_text(
      size = 11,
      face = "bold",
      color = "#111111"
    ),
    axis.text = element_text(
      size = 10,
      color = "#111111"
    ),
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(
      color = "#D9DDE1",
      linewidth = 0.35,
      linetype = "dashed"
    ),
    
    legend.position = "bottom",
    legend.text = element_text(
      size = 10,
      color = "#111111"
    ),
    
    plot.caption = element_text(
      size = 8,
      color = "#4F5B66",
      hjust = 0,
      margin = margin(t = 18)
    ),
    
    plot.margin = margin(15, 20, 15, 20)
  )

# ---------------------------------------------------------
# 4. Saml tabel og graf
# ---------------------------------------------------------

final_plot <- arrangeGrob(
  tabel_grob,
  graf,
  ncol = 1,
  heights = c(0.18, 1)
)

grid.newpage()
grid.draw(final_plot)