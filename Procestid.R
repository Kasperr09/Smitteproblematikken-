library(gt)

# Data
df <- data.frame(
  Sagstype = "Ejendomsmatrikulering",
  Pessimistisk = "+ 12 måneder",
  Base = "9 måneder",
  Optimistisk = "6 måneder"
)

# Tabel
gt(df) |>
  tab_header(
    title = md("**Sagstype og scenarieanalyse**")
  ) |>
  cols_align(
    align = "center",
    -Sagstype
  ) |>
  tab_style(
    style = list(
      cell_fill(color = "#F3F4F6"),
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(everything())
  ) |>
  tab_style(
    style = cell_text(color = "#111827", size = "medium"),
    locations = cells_body()
  ) |>
  cols_width(
    Sagstype ~ px(300),
    everything() ~ px(180)
  ) |>
  tab_options(
    table.border.top.color = "#D1D5DB",
    table.border.bottom.color = "#D1D5DB",
    table_body.hlines.color = "#E5E7EB",
    table_body.vlines.color = "#E5E7EB",
    column_labels.border.bottom.color = "#D1D5DB"
  )
