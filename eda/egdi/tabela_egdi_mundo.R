library(tidyverse)
library(gt)
library(webshot2)

df <- read.csv('egdi_quantiles.csv')

gt(df) %>%
  cols_label(
    Indicador = 'Índice/Subíndice'
  ) %>%
  gtsave("C:/Users/lazar/Master-Dissertation/figuras/tabela_egdi_mundi.png")

