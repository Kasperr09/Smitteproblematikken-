library(ggplot2)
library(tibble)
library(scales)
library(grid)

# ── DATA ───────────────────────────────────────────────────

df <- tibble(
  Lejeprincip = factor(
    c("Det lejedes værdi", "Markedsleje"),
    levels = c("Det lejedes værdi", "Markedsleje")
  ),
  Leje = c(1675, 2464),
  x = c(1, 2)
)

dlv <- 1675
marked <- 2464

diff_kr  <- marked - dlv
diff_pct <- (marked - dlv) / dlv * 100

# ── TEMAFARVER ─────────────────────────────────────────────

theme_bar      <- "#AEB7BF"
theme_outline  <- "#6F7C86"
theme_text     <- "#2F3A40"
theme_grid     <- "#C9D0D5"
theme_green    <- "#4A8B5F"
theme_bg       <- "white"

# ── PLOT ───────────────────────────────────────────────────

plot <- ggplot(df, aes(x = x, y = Leje)) +
  
  geom_segment(
    aes(x = x, xend = x, y = 0, yend = Leje),
    color = theme_grid,
    linewidth = 0.5
  ) +
  
  geom_col(
    width = 0.56,
    fill = theme_bar,
    color = theme_outline,
    linewidth = 0.6
  ) +
  
  geom_text(
    aes(label = paste0(formatC(Leje, format = "f", digits = 0, big.mark = "."), " kr.")),
    vjust = -0.55,
    family = "serif",
    fontface = "bold",
    size = 4.5,
    color = theme_text
  ) +
  
  # ── BRACKET: DLV → MARKED ─────────────────────────────
  annotate(
    "segment",
    x = 1, xend = 2,
    y = 2680, yend = 2680,
    linewidth = 0.7,
    color = theme_outline
  ) +
  annotate(
    "segment",
    x = 1, xend = 1,
    y = 2000, yend = 2680,
    linewidth = 0.7,
    color = theme_outline
  ) +
  annotate(
    "segment",
    x = 2, xend = 2,
    y = 2580, yend = 2680,
    linewidth = 0.6,
    color = theme_outline,
    alpha = 0.9
  ) +
  
  annotate(
    "label",
    x = 1.5,
    y = 2755,
    label = paste0("+", round(diff_pct, 1), " %"),
    family = "serif",
    fontface = "bold",
    size = 4.9,
    fill = "white",
    color = theme_green,
    label.size = 0.25,
    label.r = unit(0.15, "lines")
  ) +
  
  annotate(
    "text",
    x = 1.5,
    y = 2655,
    label = paste0("(", formatC(diff_kr, format = "f", digits = 0, big.mark = "."), " kr.)"),
    family = "serif",
    size = 3.6,
    color = theme_text
  ) +
  
  scale_x_continuous(
    breaks = c(1, 2),
    labels = c(
      "Det lejedes\nværdi",
      "Markedsleje"
    ),
    expand = expansion(mult = c(0.08, 0.08))
  ) +
  
  scale_y_continuous(
    limits = c(0, 2900),
    breaks = seq(0, 2500, by = 500),
    expand = c(0, 0),
    labels = function(x) paste0(formatC(x, format = "f", digits = 0, big.mark = "."), " kr.")
  ) +
  
  labs(
    title = "Lejeniveauer i 2026",
    subtitle = "Sammenligning af det lejedes værdi og markedsleje",
    x = NULL,
    y = "Kr. pr. m² årligt",
    caption = "Note: Procentangivelser viser den relative forskel mellem lejeniveauerne, mens beløbet i parentes viser den absolutte forskel i kr. pr. m²."
  ) +
  
  theme_minimal(base_family = "serif", base_size = 12) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = theme_bg, color = NA),
    panel.background = element_rect(fill = theme_bg, color = NA)
  )

print(plot)

ggsave(
  filename = "lejeniveauer_2026_barplot_updated.pdf",
  plot = plot,
  width = 24,
  height = 16,
  units = "cm",
  dpi = 300,
  bg = theme_bg
)