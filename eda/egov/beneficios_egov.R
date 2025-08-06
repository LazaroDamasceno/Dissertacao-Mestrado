library(readxl)
library(gt)

read_xlsx('beneficios_egov.xlsx') |>
  gt() |>
  opt_stylize(
    style = 5, 
    color = "blue" 
  )
