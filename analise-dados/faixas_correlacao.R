library(readxl)
library(gt)

read_xlsx('faixas_correlacao.xlsx') |>
  gt() |>
  opt_stylize(
    style = 5, 
    color = "blue" 
  )
