library(tidyverse)
library(gt)

read.csv('egdi_quantiles.csv') %>%
  gt() %>%
  fmt_number(
    columns = -Indicador
  )
  cols_label(
    Indicador = 'Índice/Subíndice'
  )

