library(gt)
library(readxl)

read_excel('tic_domicilios_2024_criterios_g3.xlsx') |>
  gt() |>
  opt_stylize(
    style = 5, 
    color = "blue" 
  )

