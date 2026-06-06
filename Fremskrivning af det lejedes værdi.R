# ============================================================
# ANALYSE + FREMSKRIVNING:
# Vægtet gennemsnitlig godkendt leje pr. m² pr. år
# Observeret 2018-2023 + fremskrevet 2024-2026
# Region: Region Hovedstaden
# Fremskrivning baseret på HUS3, Private boliger
# ============================================================


# ── 1. PAKKER ────────────────────────────────────────────────

if (!require(dplyr))      install.packages("dplyr")
if (!require(ggplot2))    install.packages("ggplot2")
if (!require(scales))     install.packages("scales")
if (!require(knitr))      install.packages("knitr")
if (!require(kableExtra)) install.packages("kableExtra")
if (!require(readr))      install.packages("readr")
if (!require(stringr))    install.packages("stringr")
if (!require(tibble))     install.packages("tibble")
if (!require(tidyr))      install.packages("tidyr")
if (!require(grid))       install.packages("grid")
if (!require(readxl))     install.packages("readxl")

library(dplyr)
library(ggplot2)
library(scales)
library(knitr)
library(kableExtra)
library(readr)
library(stringr)
library(tibble)
library(tidyr)
library(grid)
library(readxl)


# ── 2. INDLÆS HUS3.XLSX ─────────────────────────────────────

HUS3 <- read_excel("HUS3.xlsx", col_names = FALSE)


# ── 3. KLARGØR DET LEJEDES VÆRDI ─────────────────────────────

Det_lejedes_værdi_ <- Det_lejedes_værdi_ %>%
  mutate(
    År = as.integer(`Lejefastsættelses-tidspunktet`)
  )

leje_per_år <- Det_lejedes_værdi_ %>%
  filter(
    !is.na(År),
    !is.na(`Godkendt leje, årlig`),
    !is.na(`m²`)
  ) %>%
  group_by(År) %>%
  summarise(
    Sum_leje       = sum(`Godkendt leje, årlig`, na.rm = TRUE),
    Sum_m2         = sum(`m²`, na.rm = TRUE),
    Gns_leje_pr_m2 = Sum_leje / Sum_m2,
    Antal_obs      = n(),
    .groups        = "drop"
  ) %>%
  arrange(År) %>%
  mutate(
    Pct_ændring = (Gns_leje_pr_m2 / lag(Gns_leje_pr_m2) - 1) * 100,
    Pct_label   = case_when(
      is.na(Pct_ændring) ~ "",
      Pct_ændring >= 0   ~ paste0("+", round(Pct_ændring, 1), "%"),
      TRUE               ~ paste0(round(Pct_ændring, 1), "%")
    ),
    Serie = "Observeret"
  )

print(leje_per_år)


# ── 4. KLARGØR HUS3.XLSX ─────────────────────────────────────

# Række 3 indeholder tidskolonnerne
tid_kolonner <- as.character(unlist(HUS3[3, 4:ncol(HUS3)]))

# Række 4 er datarækken
hus3_wide <- HUS3[4, , drop = FALSE]

# Navngiv kolonner
names(hus3_wide)[1:3] <- c("region", "ejendomskategori", "enhed")
names(hus3_wide)[4:ncol(hus3_wide)] <- tid_kolonner

# Gør data lange
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


# ── 5. HUSLEJEINDEKS FOR REGION HOVEDSTADEN ─────────────────

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

if (!2023 %in% hus3_aar$År) {
  stop("HUS3 indeholder ikke 2023 i den filtrerede årsserie.")
}


# ── 6. FREMSKRIV DET LEJEDES VÆRDI FRA 2023 ──────────────────

basis_2023 <- leje_per_år %>%
  filter(År == 2023) %>%
  pull(Gns_leje_pr_m2)

if (length(basis_2023) == 0 || is.na(basis_2023)) {
  stop("Kunne ikke finde observeret basisværdi for 2023.")
}

v2024 <- hus3_aar %>% filter(År == 2024) %>% pull(Pct_ændring_indeks)
v2025 <- hus3_aar %>% filter(År == 2025) %>% pull(Pct_ændring_indeks)

if (length(v2024) == 0 || is.na(v2024)) stop("Manglende HUS3-vækstrate for 2024.")
if (length(v2025) == 0 || is.na(v2025)) stop("Manglende HUS3-vækstrate for 2025.")

fremskrivning_2425 <- tibble(
  År = 2024:2025,
  Pct_ændring = c(v2024, v2025),
  Gns_leje_pr_m2 = NA_real_
)

fremskrivning_2425$Gns_leje_pr_m2[fremskrivning_2425$År == 2024] <-
  basis_2023 * (1 + v2024 / 100)

fremskrivning_2425$Gns_leje_pr_m2[fremskrivning_2425$År == 2025] <-
  fremskrivning_2425$Gns_leje_pr_m2[fremskrivning_2425$År == 2024] * (1 + v2025 / 100)

niveau_2025 <- fremskrivning_2425 %>%
  filter(År == 2025) %>%
  pull(Gns_leje_pr_m2)

fremskrivning_2026 <- tibble(
  År = 2026,
  Pct_ændring = v2025,
  Gns_leje_pr_m2 = niveau_2025 * (1 + v2025 / 100)
)

fremskrivning <- bind_rows(fremskrivning_2425, fremskrivning_2026) %>%
  mutate(
    Antal_obs = NA_integer_,
    Pct_label = case_when(
      is.na(Pct_ændring) ~ "",
      Pct_ændring >= 0   ~ paste0("+", round(Pct_ændring, 1), "%"),
      TRUE               ~ paste0(round(Pct_ændring, 1), "%")
    ),
    Serie = "Fremskrevet"
  )

print(fremskrivning)


# ── 7. SAMLET SERIE ──────────────────────────────────────────

grafdata <- bind_rows(
  leje_per_år %>%
    select(År, Gns_leje_pr_m2, Antal_obs, Pct_ændring, Pct_label, Serie),
  fremskrivning %>%
    select(År, Gns_leje_pr_m2, Antal_obs, Pct_ændring, Pct_label, Serie)
)

observeret <- grafdata %>% filter(Serie == "Observeret")
fremskrevet <- grafdata %>% filter(Serie == "Fremskrevet")

linje_obs <- observeret %>% select(År, Gns_leje_pr_m2)

linje_fremsk <- bind_rows(
  observeret %>% filter(År == 2023) %>% select(År, Gns_leje_pr_m2),
  fremskrevet %>% select(År, Gns_leje_pr_m2)
)

print(grafdata)


# ── 8. GRAF ──────────────────────────────────────────────────

p_fremskriv <- ggplot() +
  
  geom_area(
    data = observeret,
    aes(x = År, y = Gns_leje_pr_m2),
    fill = "white",
    alpha = 0.06
  ) +
  
  geom_line(
    data = linje_obs,
    aes(x = År, y = Gns_leje_pr_m2),
    color = "#2C3E50",
    linewidth = 0.9,
    lineend = "round"
  ) +
  
  geom_line(
    data = linje_fremsk,
    aes(x = År, y = Gns_leje_pr_m2),
    color = "#2C3E50",
    linewidth = 0.9,
    linetype = "dashed",
    lineend = "round"
  ) +
  
  geom_point(
    data = observeret,
    aes(x = År, y = Gns_leje_pr_m2, size = Antal_obs),
    color  = "#7F8C8D",
    fill   = "#BDC3C7",
    shape  = 21,
    stroke = 1.2,
    alpha  = 0.95
  ) +
  
  geom_point(
    data = fremskrevet,
    aes(x = År, y = Gns_leje_pr_m2),
    color  = "#7F8C8D",
    fill   = "white",
    shape  = 21,
    stroke = 1.2,
    size   = 8,
    alpha  = 1
  ) +
  
  geom_text(
    data = observeret,
    aes(
      x = År,
      y = Gns_leje_pr_m2,
      label = paste0(format(round(Gns_leje_pr_m2, 0), big.mark = ".", scientific = FALSE), " kr.")
    ),
    vjust    = -3.8,
    size     = 3.3,
    color    = "grey20",
    fontface = "bold",
    family   = "serif"
  ) +
  
  geom_text(
    data = fremskrevet,
    aes(
      x = År,
      y = Gns_leje_pr_m2,
      label = paste0(format(round(Gns_leje_pr_m2, 0), big.mark = ".", scientific = FALSE), " kr.")
    ),
    vjust    = -3.8,
    size     = 3.3,
    color    = "grey20",
    fontface = "bold",
    family   = "serif"
  ) +
  
  geom_text(
    data = observeret,
    aes(x = År, y = Gns_leje_pr_m2, label = Pct_label),
    vjust    = -5.8,
    size     = 3.1,
    color    = "#2C7BB6",
    fontface = "bold",
    family   = "serif"
  ) +
  
  geom_text(
    data = fremskrevet,
    aes(x = År, y = Gns_leje_pr_m2, label = Pct_label),
    vjust    = -5.8,
    size     = 3.1,
    color    = "#2C7BB6",
    fontface = "bold",
    family   = "serif"
  ) +
  
  geom_text(
    data = observeret,
    aes(x = År, y = Gns_leje_pr_m2, label = paste0("(n = ", Antal_obs, ")")),
    vjust  = 3.8,
    size   = 2.8,
    color  = "grey50",
    family = "serif"
  ) +
  
  geom_vline(
    xintercept = 2023.5,
    linetype = "dotted",
    color = "grey50",
    linewidth = 0.5
  ) +
  
  scale_x_continuous(
    breaks = 2018:2026,
    labels = as.character,
    expand = expansion(mult = c(0.10, 0.10))
  ) +
  
  scale_y_continuous(
    limits = c(0, NA),
    labels = function(x) paste0(formatC(x, format = "f", digits = 0, big.mark = " "), " kr."),
    expand = expansion(mult = c(0, 0.24))
  ) +
  
  scale_size_continuous(
    name   = "Antal observationer",
    range  = c(5, 14),
    breaks = pretty_breaks()(observeret$Antal_obs)
  ) +
  
  labs(
    title    = "Udvikling i det lejedes værdi - vægtet gennemsnitlig godkendt leje pr. m2 (2018-2026)",
    subtitle = "2018-2023 er observerede værdier. 2024-2025 er fremskrevet med HUS3 for Region Hovedstaden, og 2026 er teknisk forlænget. Den stiplede linje angiver fremskrivningen.",
    x        = "År for lejefastsættelse",
    y        = "Vægtet gns. godkendt leje pr. m2 (kr./år)",
    caption  = paste(
      "Note: Punktstørrelse angiver antal observationer for observerede år.",
      "Procentuelle ændringer er beregnet ift. forrige år.",
      "Fremskrivningen for 2024-2025 er baseret på HUS3 for Region Hovedstaden, Private boliger.",
      "2026 er teknisk fremskrevet med samme vækstrate som 2025."
    )
  ) +
  
  theme_classic(base_size = 12, base_family = "serif") +
  theme(
    plot.title         = element_text(face = "bold", size = 13, margin = margin(b = 4)),
    plot.subtitle      = element_text(color = "grey35", size = 10, margin = margin(b = 12)),
    plot.caption       = element_text(color = "grey45", size = 8, hjust = 0, lineheight = 1.25, margin = margin(t = 12)),
    plot.margin        = margin(20, 30, 30, 20),
    axis.title         = element_text(face = "bold", size = 11),
    axis.text          = element_text(size = 10, color = "grey20"),
    axis.text.x        = element_text(margin = margin(t = 5)),
    axis.line          = element_line(color = "grey30", linewidth = 0.4),
    axis.ticks         = element_line(color = "grey30", linewidth = 0.4),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.4, linetype = "dotted"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position    = "bottom",
    legend.title       = element_text(face = "bold", size = 9),
    legend.text        = element_text(size = 9),
    legend.key.size    = unit(0.8, "lines"),
    plot.background    = element_rect(fill = "white", color = NA),
    panel.background   = element_rect(fill = "white", color = NA)
  )

print(p_fremskriv)


# ── 9. GEM GRAF ──────────────────────────────────────────────

ggsave(
  filename = "leje_pr_m2_per_aar_fremskrivning_HUS3.pdf",
  plot     = p_fremskriv,
  width    = 24,
  height   = 15,
  units    = "cm",
  dpi      = 300,
  bg       = "white"
)