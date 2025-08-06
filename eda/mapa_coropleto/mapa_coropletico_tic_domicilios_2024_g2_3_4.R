library(tidyverse)
library(sf)
library(readxl)
library(patchwork)

ne = c('MA', 'PI', 'CE', 'RN', 'PB', 'PE',  'BA', 'SE', 'AL')
co = c('MT', 'MS', 'DF', 'GO')
sul = c('RS', 'SC', 'PR')
sudeste = c('SP', 'MG', 'ES', 'RJ')
norte = c('AC', 'AP', 'AM', 'PA', 'RO', 'RR', 'TO')

df3 <- st_read('brazil_geo.json') %>%
  mutate(Regiao = case_when(
    id %in% ne ~ 'Nordeste',
    id %in% co ~ 'Centro-Oeste',
    id %in% sul ~ 'Sul',
    id %in% sudeste ~ 'Sudeste',
    id %in% norte ~ 'Norte'
  )) %>% 
  mutate(Valor = case_when(
    id %in% ne ~ 17,
    id %in% co ~ 13,
    id %in% sul ~ 22,
    id %in% sudeste ~ 25,
    id %in% norte ~ 26
  )) %>% 
  ggplot(aes(fill = Valor)) +
    geom_sf() +
    labs(
      fill = 'Valor (%)',
      title = 'Critério 3'
    ) +
    theme_void() +
    scale_fill_viridis_b()

df4 <- st_read('brazil_geo.json') %>%
  mutate(Regiao = case_when(
    id %in% ne ~ 'Nordeste',
    id %in% co ~ 'Centro-Oeste',
    id %in% sul ~ 'Sul',
    id %in% sudeste ~ 'Sudeste',
    id %in% norte ~ 'Norte'
  )) %>% 
  mutate(Valor = case_when(
    id %in% ne ~ 20,
    id %in% co ~ 18,
    id %in% sul ~ 32,
    id %in% sudeste ~ 28,
    id %in% norte ~ 23
  )) %>%
  ggplot(aes(fill = Valor)) +
    geom_sf() +
    theme_void() +
    scale_fill_viridis_b() +
    labs(
      title = 'Critério 4',
      fill = 'Valor (%)'
    )

df3 + df4
