# ============================================================
#  ANALYSE: Vægtet gennemsnitlig godkendt leje pr. m² pr. år
#  Datasæt: Det_lejedes_værdi_
#  Metode:  Sum(Godkendt leje, årlig) / Sum(m²)
# ============================================================


# ── 1. PAKKER ────────────────────────────────────────────────

if (!require(dplyr))      install.packages("dplyr")
if (!require(ggplot2))    install.packages("ggplot2")
if (!require(scales))     install.packages("scales")
if (!require(knitr))      install.packages("knitr")
if (!require(kableExtra)) install.packages("kableExtra")

library(dplyr)
library(ggplot2)
library(scales)
library(knitr)
library(kableExtra)


# ── 2. UDLED ÅR ──────────────────────────────────────────────

Det_lejedes_værdi_ <- Det_lejedes_værdi_ %>%
  mutate(År = as.integer(`Lejefastsættelses-tidspunktet`))


# ── 3. VÆGTET GENNEMSNIT + PROCENTUEL ÆNDRING PR. ÅR ────────

leje_per_år <- Det_lejedes_værdi_ %>%
  filter(!is.na(År),
         !is.na(`Godkendt leje, årlig`),
         !is.na(`m²`)) %>%
  group_by(År) %>%
  summarise(
    Sum_leje       = sum(`Godkendt leje, årlig`, na.rm = TRUE),
    Sum_m2         = sum(`m²`,                   na.rm = TRUE),
    Gns_leje_pr_m2 = Sum_leje / Sum_m2,
    Antal_obs      = n(),
    .groups        = "drop"
  ) %>%
  arrange(År) %>%
  mutate(
    # Procentuel ændring fra foregående år
    Pct_ændring = (Gns_leje_pr_m2 / lag(Gns_leje_pr_m2) - 1) * 100,
    # Label til graf: +X% eller -X%
    Pct_label   = case_when(
      is.na(Pct_ændring) ~ "",
      Pct_ændring >= 0   ~ paste0("+", round(Pct_ændring, 1), "%"),
      TRUE               ~ paste0(round(Pct_ændring, 1), "%")
    ),
    # Farve pr. år til cirklerne
    År_farve = as.factor(År)
  )


# ── 4. AKADEMISK TABEL ───────────────────────────────────────

leje_per_år %>%
  mutate(
    Gns_leje_pr_m2 = round(Gns_leje_pr_m2, 2),
    Sum_leje       = round(Sum_leje, 0),
    Sum_m2         = round(Sum_m2, 0),
    Pct_ændring    = ifelse(is.na(Pct_ændring), "—",
                            paste0(ifelse(Pct_ændring >= 0, "+", ""),
                                   round(Pct_ændring, 1), "%"))
  ) %>%
  select(År, Sum_leje, Sum_m2, Gns_leje_pr_m2, Pct_ændring, Antal_obs) %>%
  rename(
    `År`                              = År,
    `Sum godkendt leje (kr.)`         = Sum_leje,
    `Sum areal (m2)`                  = Sum_m2,
    `Vægtet gns. leje pr. m2 (kr.)`  = Gns_leje_pr_m2,
    `Ændring ift. forrige år`         = Pct_ændring,
    `Antal observationer`             = Antal_obs
  ) %>%
  kable(
    format      = "html",
    align       = c("c", "r", "r", "r", "r", "c"),
    caption     = "Tabel 1. Vægtet gennemsnitlig godkendt leje pr. m2 pr. år",
    format.args = list(big.mark = ".", decimal.mark = ",")
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "bordered"),
    full_width        = FALSE,
    position          = "center",
    font_size         = 13
  ) %>%
  row_spec(0, bold = TRUE, background = "#2C3E50", color = "white") %>%
  footnote(
    general           = "Vægtede gennemsnit er beregnet som summen af den godkendte årlige leje divideret med det samlede areal (m2) inden for hvert år. Observationer med manglende værdier er udeladt. Ændring angiver procentuel udvikling i vægtet gennemsnitsleje ift. forrige år.",
    general_title     = "Note:",
    footnote_as_chunk = TRUE
  )


# ── 5. AKADEMISK VISUALISERING ───────────────────────────────

p <- ggplot(leje_per_år, aes(x = År, y = Gns_leje_pr_m2)) +
  
  geom_area(fill = "#2C7BB6", alpha = 0.06) +
  
  geom_line(color = "#2C3E50", linewidth = 0.9, lineend = "round") +
  
  geom_point(
    aes(size = Antal_obs),
    color  = "#7F8C8D",
    fill   = "#BDC3C7",
    shape  = 21,
    stroke = 1.2,
    alpha  = 0.95
  ) +
  
  geom_text(
    aes(label = paste0(format(round(Gns_leje_pr_m2, 0),
                              big.mark  = ".",
                              scientific = FALSE), " kr.")),
    vjust    = -3.8,
    size     = 3.3,
    color    = "grey20",
    fontface = "bold",
    family   = "serif"
  ) +
  
  geom_text(
    aes(label = Pct_label),
    vjust    = -5.8,
    size     = 3.1,
    color    = "#2C7BB6",
    fontface = "bold",
    family   = "serif"
  ) +
  
  geom_text(
    aes(label = paste0("(n = ", Antal_obs, ")")),
    vjust  = 3.8,
    size   = 2.8,
    color  = "grey50",
    family = "serif"
  ) +
  
  scale_x_continuous(
    breaks = unique(leje_per_år$År),
    labels = as.character,
    expand = expansion(mult = c(0.10, 0.10))
  ) +
  
  scale_y_continuous(
    limits = c(0, NA),
    labels = function(x) paste0(formatC(x, format = "f", digits = 0,
                                        big.mark = " "), " kr."),
    expand = expansion(mult = c(0, 0.22))
  )+
  
  scale_size_continuous(
    name   = "Antal observationer",
    range  = c(5, 14),
    breaks = pretty_breaks()(leje_per_år$Antal_obs)
  ) +
  
  labs(
    title    = "Udvikling i det lejedes værdi - vægtet gennemsnitlig godkendt leje pr. m2 (2018-2023)",
    subtitle = "Procentuel ændring fra år til år angivet over hvert datapunkt",
    x        = "År for lejefastsættelse",
    y        = "Vægtet gns. godkendt leje pr. m2 (kr./år)",
    caption  = "Note: Punktstørrelse angiver antal observationer. Procentuelle ændringer er beregnet ift. forrige års vægtede gennemsnit.\nKilde: Det lejedes værdi - afgørelser fra Huslejenævnet."
  ) +
  
  theme_classic(base_size = 12, base_family = "serif") +
  theme(
    plot.title         = element_text(face = "bold", size = 13,
                                      margin = margin(b = 4)),
    plot.subtitle      = element_text(color = "grey35", size = 10,
                                      margin = margin(b = 12)),
    plot.caption       = element_text(color = "grey45", size = 8,
                                      hjust = 0, margin = margin(t = 10)),
    plot.margin        = margin(20, 30, 20, 20),
    axis.title         = element_text(face = "bold", size = 11),
    axis.text          = element_text(size = 10, color = "grey20"),
    axis.text.x        = element_text(margin = margin(t = 5)),
    axis.line          = element_line(color = "grey30", linewidth = 0.4),
    axis.ticks         = element_line(color = "grey30", linewidth = 0.4),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.4,
                                      linetype = "dotted"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position    = "bottom",
    legend.title       = element_text(face = "bold", size = 9),
    legend.text        = element_text(size = 9),
    legend.key.size    = unit(0.8, "lines"),
    plot.background    = element_rect(fill = "white", color = NA),
    panel.background   = element_rect(fill = "white", color = NA)
  )

print(p)

# ── 6. GEM GRAF ──────────────────────────────────────────────

ggsave(
  filename = "leje_pr_m2_per_aar.pdf",
  plot     = p,
  width    = 20,
  height   = 13,
  units    = "cm",
  dpi      = 300
)

# ── 6. GEM GRAF ──────────────────────────────────────────────

ggsave(
  filename = "leje_pr_m2_per_aar.pdf",
  width    = 20,
  height   = 13,
  units    = "cm",
  dpi      = 300
)