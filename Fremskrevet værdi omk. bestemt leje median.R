# ============================================================
# ANALYSE + FREMSKRIVNING:
# Godkendt leje pr. m² – Omkostningsbestemt leje
# Observeret 2016-2022 + fremskrevet 2023-2026
#
# Metode:
# 1) Observerede år beregnes som median pr. år
# 2) Outlier i 2020 (1.520,75 kr./m²) vises særskilt
#    og indgår ikke i fremskrivningsgrundlaget
# 3) Samlet median beregnes på baggrund af observerede år
#    ekskl. outlier
# 4) Fremskrivning 2023-2026 starter fra denne median
# 5) HUS3 anvendes til vækstrater for Region Hovedstaden,
#    Private boliger
# ============================================================


# ── 1. PAKKER ────────────────────────────────────────────────

required_packages <- c(
  "dplyr", "ggplot2", "scales", "readxl", "tidyr",
  "tibble", "grid"
)

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}


# ── 2. INDLÆS HUS3 ───────────────────────────────────────────

HUS3 <- read_excel("HUS3.xlsx", col_names = FALSE)


# ── 3. TJEK INPUTDATA ────────────────────────────────────────

if (!exists("Omkostningsbestemt_leje")) {
  stop("Datasættet 'Omkostningsbestemt_leje' findes ikke i miljøet.")
}

if (!("Lejen pr." %in% names(Omkostningsbestemt_leje))) {
  stop("Kolonnen 'Lejen pr.' findes ikke i datasættet.")
}

if (!("Godkendt leje pr. m², årlig inkl. hensættelser" %in% names(Omkostningsbestemt_leje))) {
  stop("Kolonnen 'Godkendt leje pr. m², årlig inkl. hensættelser' findes ikke i datasættet.")
}


# ── 4. ROBUST KONVERTERING AF LEJEKOLONNEN ──────────────────

parse_leje <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  
  x <- as.character(x)
  x <- trimws(x)
  x <- gsub("kr\\.?", "", x, ignore.case = TRUE)
  x <- gsub("\\s+", "", x)
  
  has_comma <- grepl(",", x)
  
  # Dansk format, fx 1.520,75 -> 1520.75
  x[has_comma] <- gsub("\\.", "", x[has_comma])
  x[has_comma] <- gsub(",", ".", x[has_comma])
  
  # Behold almindelige numeriske formater som 1520.75
  x <- gsub("[^0-9.\\-]", "", x)
  
  suppressWarnings(as.numeric(x))
}


# ── 5. KLARGØR OMKOSTNINGSBESTEMT LEJE ──────────────────────

Omkostningsbestemt_leje <- Omkostningsbestemt_leje %>%
  mutate(
    År = as.integer(`Lejen pr.`),
    Leje_pr_m2 = parse_leje(`Godkendt leje pr. m², årlig inkl. hensættelser`)
  ) %>%
  mutate(
    Er_outlier = År == 2020 & !is.na(Leje_pr_m2) & abs(Leje_pr_m2 - 1520.75) < 0.01
  )

print(
  Omkostningsbestemt_leje %>%
    select(`Lejen pr.`, År, `Godkendt leje pr. m², årlig inkl. hensættelser`, Leje_pr_m2, Er_outlier)
)


# ── 6. BEREGNINGSDATA UDEN OUTLIER ──────────────────────────

Omkostningsbestemt_leje_beregning <- Omkostningsbestemt_leje %>%
  filter(
    !is.na(År),
    !is.na(Leje_pr_m2),
    År >= 2016,
    År <= 2022,
    !Er_outlier
  )


# ── 7. OBSERVEREDE ÅRSMEDIANER ──────────────────────────────

observeret_okb <- Omkostningsbestemt_leje_beregning %>%
  group_by(År) %>%
  summarise(
    Antal_obs = n(),
    Median_leje_pr_m2 = median(Leje_pr_m2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(År) %>%
  mutate(
    Pct_ændring = (Median_leje_pr_m2 / lag(Median_leje_pr_m2) - 1) * 100,
    Pct_label = case_when(
      is.na(Pct_ændring) ~ "",
      Pct_ændring >= 0 ~ paste0("+", round(Pct_ændring, 1), "%"),
      TRUE ~ paste0(round(Pct_ændring, 1), "%")
    ),
    Serie = "Observeret"
  )

print(observeret_okb)


# ── 8. SAMLET MEDIAN UDEN OUTLIER ───────────────────────────

samlet_median <- median(observeret_okb$Median_leje_pr_m2, na.rm = TRUE)

cat("Samlet median uden outlier:", round(samlet_median, 2), "\n")


# ── 9. KLARGØR HUS3 ─────────────────────────────────────────

tid_kolonner <- as.character(unlist(HUS3[3, 4:ncol(HUS3)]))
hus3_wide <- HUS3[4, , drop = FALSE]

names(hus3_wide)[1:3] <- c("region", "ejendomskategori", "enhed")
names(hus3_wide)[4:ncol(hus3_wide)] <- tid_kolonner

hus3_long <- hus3_wide %>%
  pivot_longer(
    cols = 4:ncol(hus3_wide),
    names_to = "TID",
    values_to = "indeks"
  ) %>%
  mutate(
    region = trimws(as.character(region)),
    ejendomskategori = trimws(as.character(ejendomskategori)),
    enhed = trimws(as.character(enhed)),
    indeks = as.numeric(indeks),
    År = as.integer(substr(TID, 1, 4))
  ) %>%
  filter(!is.na(indeks), !is.na(År))

print(hus3_long)


# ── 10. HUS3 FOR REGION HOVEDSTADEN / PRIVATE BOLIGER ──────

hus3_aar <- hus3_long %>%
  filter(
    region == "Region Hovedstaden",
    ejendomskategori == "Private boliger",
    enhed == "Indeks"
  ) %>%
  group_by(År) %>%
  summarise(
    Huslejeindeks = mean(indeks, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(År) %>%
  mutate(
    Pct_ændring_indeks = (Huslejeindeks / lag(Huslejeindeks) - 1) * 100
  )

print(hus3_aar)


# ── 11. FREMSKRIVNING FRA SAMLET MEDIAN ─────────────────────

v2023 <- hus3_aar %>% filter(År == 2023) %>% pull(Pct_ændring_indeks)
v2024 <- hus3_aar %>% filter(År == 2024) %>% pull(Pct_ændring_indeks)
v2025 <- hus3_aar %>% filter(År == 2025) %>% pull(Pct_ændring_indeks)

if (length(v2023) == 0 || is.na(v2023)) stop("Manglende HUS3-vækstrate for 2023.")
if (length(v2024) == 0 || is.na(v2024)) stop("Manglende HUS3-vækstrate for 2024.")
if (length(v2025) == 0 || is.na(v2025)) stop("Manglende HUS3-vækstrate for 2025.")

fremskrivning_okb <- tibble(
  År = 2023:2026,
  Pct_ændring = c(v2023, v2024, v2025, v2025),
  Median_leje_pr_m2 = NA_real_
)

fremskrivning_okb$Median_leje_pr_m2[1] <- samlet_median * (1 + v2023 / 100)
fremskrivning_okb$Median_leje_pr_m2[2] <- fremskrivning_okb$Median_leje_pr_m2[1] * (1 + v2024 / 100)
fremskrivning_okb$Median_leje_pr_m2[3] <- fremskrivning_okb$Median_leje_pr_m2[2] * (1 + v2025 / 100)
fremskrivning_okb$Median_leje_pr_m2[4] <- fremskrivning_okb$Median_leje_pr_m2[3] * (1 + v2025 / 100)

fremskrivning_okb <- fremskrivning_okb %>%
  mutate(
    Antal_obs = NA_integer_,
    Pct_label = case_when(
      is.na(Pct_ændring) ~ "",
      Pct_ændring >= 0 ~ paste0("+", round(Pct_ændring, 1), "%"),
      TRUE ~ paste0(round(Pct_ændring, 1), "%")
    ),
    Serie = "Fremskrevet"
  )

print(fremskrivning_okb)


# ── 12. SERIER TIL GRAF ─────────────────────────────────────

grafdata_okb <- bind_rows(
  observeret_okb %>%
    transmute(År, Leje_niveau = Median_leje_pr_m2, Antal_obs, Pct_ændring, Pct_label, Serie),
  fremskrivning_okb %>%
    transmute(År, Leje_niveau = Median_leje_pr_m2, Antal_obs, Pct_ændring, Pct_label, Serie)
)

observeret_vis <- grafdata_okb %>% filter(Serie == "Observeret")
fremskrevet_vis <- grafdata_okb %>% filter(Serie == "Fremskrevet")

linje_obs_okb <- observeret_vis %>%
  select(År, Leje_niveau)

linje_fremsk_okb <- bind_rows(
  tibble(
    År = 2022,
    Leje_niveau = samlet_median
  ),
  fremskrevet_vis %>%
    select(År, Leje_niveau)
)

outlier_punkt_okb <- Omkostningsbestemt_leje %>%
  filter(Er_outlier) %>%
  transmute(
    År,
    Leje_pr_m2,
    label_værdi = paste0(
      format(round(Leje_pr_m2, 0), big.mark = ".", scientific = FALSE),
      " kr."
    )
  )

y_top <- max(
  observeret_vis$Leje_niveau,
  fremskrevet_vis$Leje_niveau,
  outlier_punkt_okb$Leje_pr_m2,
  samlet_median,
  na.rm = TRUE
)


# ── 13. GRAF ────────────────────────────────────────────────

p_okb_fremskriv <- ggplot() +
  
  annotate(
    "rect",
    xmin = min(observeret_vis$År),
    xmax = max(observeret_vis$År) + 0.15,
    ymin = 0,
    ymax = max(observeret_vis$Leje_niveau, na.rm = TRUE) * 0.98,
    fill = "white",
    alpha = 0.35
  ) +
  
  geom_line(
    data = linje_obs_okb,
    aes(x = År, y = Leje_niveau),
    color = "#2C3E50",
    linewidth = 1.2,
    lineend = "round"
  ) +
  
  geom_line(
    data = linje_fremsk_okb,
    aes(x = År, y = Leje_niveau),
    color = "#2C3E50",
    linewidth = 1.2,
    linetype = "dashed",
    lineend = "round"
  ) +
  
  geom_hline(
    yintercept = samlet_median,
    color = "#C0392B",
    linewidth = 0.8,
    linetype = "22",
    alpha = 0.75
  ) +
  
  geom_point(
    data = observeret_vis,
    aes(x = År, y = Leje_niveau, size = Antal_obs),
    shape = 21,
    fill = "#BFC7CC",
    color = "#7F8C8D",
    stroke = 1.4,
    alpha = 0.95
  ) +
  
  geom_point(
    data = fremskrevet_vis,
    aes(x = År, y = Leje_niveau),
    shape = 21,
    size = 8,
    fill = "#F5F5F5",
    color = "#7F8C8D",
    stroke = 1.4
  ) +
  
  geom_text(
    data = observeret_vis,
    aes(
      x = År,
      y = Leje_niveau,
      label = paste0(format(round(Leje_niveau, 0), big.mark = ".", scientific = FALSE), " kr.")
    ),
    vjust = -2.0,
    size = 4.1,
    color = "#2B2B2B",
    fontface = "bold",
    family = "serif"
  ) +
  
  geom_text(
    data = fremskrevet_vis,
    aes(
      x = År,
      y = Leje_niveau,
      label = paste0(format(round(Leje_niveau, 0), big.mark = ".", scientific = FALSE), " kr.")
    ),
    vjust = -1.9,
    size = 4.1,
    color = "#2B2B2B",
    fontface = "bold",
    family = "serif"
  ) +
  
  geom_text(
    data = observeret_vis,
    aes(x = År, y = Leje_niveau, label = Pct_label),
    vjust = -3.8,
    size = 3.9,
    color = "#2C7BB6",
    fontface = "bold",
    family = "serif"
  ) +
  
  geom_text(
    data = fremskrevet_vis,
    aes(x = År, y = Leje_niveau, label = Pct_label),
    vjust = -3.8,
    size = 3.9,
    color = "#2C7BB6",
    fontface = "bold",
    family = "serif"
  ) +
  
  geom_text(
    data = observeret_vis,
    aes(x = År, y = Leje_niveau, label = paste0("(n = ", Antal_obs, ")")),
    vjust = 3.5,
    size = 3,
    color = "grey45",
    family = "serif"
  ) +
  
  geom_vline(
    xintercept = 2022.5,
    linetype = "dotted",
    color = "grey45",
    linewidth = 0.7
  ) +
  
  geom_point(
    data = outlier_punkt_okb,
    aes(x = År, y = Leje_pr_m2),
    shape = 24,
    size = 6,
    fill = "#E74C3C",
    color = "#E74C3C",
    stroke = 1.1
  ) +
  
  geom_text(
    data = outlier_punkt_okb,
    aes(x = År, y = Leje_pr_m2, label = label_værdi),
    vjust = -0.8,
    hjust = -0.15,
    size = 4.2,
    color = "#E74C3C",
    family = "serif"
  ) +
  
  geom_text(
    data = outlier_punkt_okb,
    aes(x = År, y = Leje_pr_m2),
    label = "Outlier",
    vjust = 2.0,
    size = 3.0,
    color = "#E74C3C",
    family = "serif"
  ) +
  
  annotate(
    "text",
    x = 2026.22,
    y = samlet_median,
    label = paste0(
      "Median: ",
      format(round(samlet_median, 0), big.mark = ".", scientific = FALSE),
      " kr."
    ),
    hjust = 0,
    vjust = -0.55,
    size = 3.2,
    color = "#C0392B",
    family = "serif"
  ) +
  
  scale_x_continuous(
    breaks = 2016:2026,
    labels = as.character,
    expand = expansion(mult = c(0.05, 0.13))
  ) +
  
  scale_y_continuous(
    limits = c(0, y_top * 1.18),
    labels = function(x) paste0(formatC(x, format = "f", digits = 0, big.mark = "."), " kr."),
    expand = expansion(mult = c(0, 0.02))
  ) +
  
  scale_size_continuous(
    name = "Antal observationer",
    range = c(7, 12),
    breaks = c(1, 2),
    labels = c("1", "2"),
    guide = guide_legend(
      override.aes = list(
        shape = 21,
        fill = "#BFC7CC",
        color = "#7F8C8D",
        stroke = 1.4,
        alpha = 1
      ),
      title.position = "top",
      nrow = 1
    )
  ) +
  
  labs(
    title = "Udvikling i godkendt leje pr. m2 – Omkostningsbestemt leje (2016-2026)",
    subtitle = paste(
      "2016-2022 er observerede år. 2023-2026 er fremskrevet fra samlet median uden outlier.",
      "Outlieren i 2020 vises særskilt og indgår ikke i fremskrivningen."
    ),
    x = "År for lejefastsættelse",
    y = "Median godkendt leje pr. m2 (kr./år)",
    caption = paste(
      "Note: Punktstørrelse angiver antal observationer for observerede år.",
      "Procentuelle ændringer er beregnet i forhold til forrige observerede/fremskrevne år.",
      "Den røde stiplede linje viser samlet median uden outlier.",
      "Fremskrivningen tager udgangspunkt i medianen og ikke i den observerede 2022-værdi."
    )
  ) +
  
  theme_classic(base_size = 13, base_family = "serif") +
  theme(
    plot.title = element_text(face = "bold", size = 19, margin = margin(b = 6)),
    plot.subtitle = element_text(color = "grey35", size = 12.5, margin = margin(b = 14)),
    plot.caption = element_text(color = "grey45", size = 9, hjust = 0, lineheight = 1.25, margin = margin(t = 16)),
    plot.margin = margin(25, 35, 40, 25),
    
    axis.title = element_text(face = "bold", size = 15),
    axis.text = element_text(size = 12, color = "grey20"),
    axis.text.x = element_text(margin = margin(t = 8)),
    axis.text.y = element_text(margin = margin(r = 6)),
    axis.line = element_line(color = "grey35", linewidth = 0.6),
    axis.ticks = element_line(color = "grey35", linewidth = 0.5),
    
    panel.grid.major.y = element_line(color = "grey82", linewidth = 0.55, linetype = "dotted"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 11),
    legend.key.size = unit(1.5, "lines"),
    legend.key = element_blank(),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.box.margin = margin(t = 8),
    
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

print(p_okb_fremskriv)


# ── 14. GEM GRAF ────────────────────────────────────────────

ggsave(
  filename = "omkostningsbestemt_leje_fremskrivning_HUS3_median.pdf",
  plot = p_okb_fremskriv,
  width = 24,
  height = 15,
  units = "cm",
  dpi = 300,
  bg = "#F3F3F3"
)