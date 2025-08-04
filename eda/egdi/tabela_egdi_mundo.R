library(tidyverse)
library(gt)

read.csv('egdi_quantiles.csv') %>%
  gt() %>%
  cols_label(
    Indicador = 'Índice/Subíndice'
  )
