# Manuel kontrol for 2019
Det_lejedes_værdi_ %>%
  filter(År == 2018) %>%
  select(`Godkendt leje, årlig`, `m²`, `Godkendt leje pr. m², årlig`) %>%
  mutate(
    Manuel_kontrol = `Godkendt leje, årlig` / `m²`
  ) %>%
  summarise(
    Sum_leje       = sum(`Godkendt leje, årlig`, na.rm = TRUE),
    Sum_m2         = sum(`m²`,                   na.rm = TRUE),
    Vægtet_gns     = Sum_leje / Sum_m2,
    Antal_obs      = n()
  )

