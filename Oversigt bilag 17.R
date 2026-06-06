# Pakker
# install.packages(c("ggplot2", "dplyr", "tibble"))

library(ggplot2)
library(dplyr)
library(tibble)

# -----------------------------
# DATA
# -----------------------------
df <- tibble(
  scenarie = c("A", "B", "C"),
  fortolkning = c(
    "Matrikulært\nejendomsbegreb",
    "Bygningsbaseret\nejendomsbegreb",
    "Usikker retstilstand"
  ),
  konsekvens = c(
    "Markedsleje kan være afskåret",
    "Markedsleje kan anvendes",
    "Investor kræver\nrisikopræmie/forsinkelse\nindregnes"
  ),
  input = c(
    "Det lejedes værdi",
    "Markedsleje",
    "Højere\ndiskonteringsrente/lavere\nIRR"
  ),
  y = c(3, 2, 1)
)

# -----------------------------
# FARVER
# -----------------------------
bg_col     <- "white"
text_col   <- "#1f2d3d"
header_col <- "#111111"
line_col   <- "#e3e3e3"

# Kolonneplaceringer
x_scenarie    <- 0.8
x_fortolkning <- 3.0
x_konsekvens  <- 6.4
x_input       <- 10.9

# -----------------------------
# FIGUR
# -----------------------------
p <- ggplot() +
  theme_void(base_family = "serif") +
  theme(
    plot.background  = element_rect(fill = bg_col, color = bg_col),
    panel.background = element_rect(fill = bg_col, color = bg_col),
    plot.margin = margin(25, 35, 25, 35)
  ) +

  # Diskrete vandrette linjer
  geom_segment(aes(x = 0.6, xend = 14.4, y = 3.65, yend = 3.65),
               linewidth = 0.45, color = "#cfcfcf") +
  geom_segment(aes(x = 0.6, xend = 14.4, y = 2.45, yend = 2.45),
               linewidth = 0.35, color = line_col) +
  geom_segment(aes(x = 0.6, xend = 14.4, y = 1.45, yend = 1.45),
               linewidth = 0.35, color = line_col) +

  # Overskrifter
  annotate("text", x = x_scenarie, y = 3.95, label = "Juridisk scenarie",
           hjust = 0, family = "serif", fontface = "bold",
           size = 5.6, color = header_col) +
  annotate("text", x = x_fortolkning, y = 3.95, label = "Fortolkning",
           hjust = 0, family = "serif", fontface = "bold",
           size = 5.6, color = header_col) +
  annotate("text", x = x_konsekvens, y = 3.95, label = "Lejeretlig konsekvens",
           hjust = 0, family = "serif", fontface = "bold",
           size = 5.6, color = header_col) +
  annotate("text", x = x_input, y = 3.95, label = "Økonomisk input",
           hjust = 0, family = "serif", fontface = "bold",
           size = 5.6, color = header_col) +

  # Indhold
  geom_text(data = df,
            aes(x = x_scenarie, y = y, label = scenarie),
            hjust = 0, vjust = 0.5,
            family = "serif", size = 5.8, color = text_col) +

  geom_text(data = df,
            aes(x = x_fortolkning, y = y, label = fortolkning),
            hjust = 0, vjust = 0.5,
            family = "serif", lineheight = 1.25,
            size = 5.4, color = text_col) +

  geom_text(data = df,
            aes(x = x_konsekvens, y = y, label = konsekvens),
            hjust = 0, vjust = 0.5,
            family = "serif", lineheight = 1.25,
            size = 5.4, color = text_col) +

  geom_text(data = df,
            aes(x = x_input, y = y, label = input),
            hjust = 0, vjust = 0.5,
            family = "serif", lineheight = 1.25,
            size = 5.4, color = text_col) +

  coord_cartesian(
    xlim = c(0.6, 14.7),
    ylim = c(0.55, 4.15),
    clip = "off"
  )

p