library(gt)
library(sf)

read.csv('egdi_quantiles.csv') %>%
  gt() |>
  fmt_number(
    columns = -Indicador
  ) |>
  cols_label(
    Indicador = 'Índice/Subíndice'
  ) |>
  opt_stylize(
    style = 5, 
    color = "blue"
  )


