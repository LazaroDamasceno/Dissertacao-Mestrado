library(tidyverse)
library(gt)
library(webshot2)

read.csv('egdi_quantiles.csv') %>%
  gt(df) %>%
    cols_label(
      Indicador = 'Índice/Subíndice'
    )
