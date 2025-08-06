library(tidyverse)
library(sf)
library(readxl)
library(patchwork)

ne = c('MA', 'PI', 'CE', 'RN', 'PB', 'PE',  'BA', 'SE', 'AL')
co = c('MT', 'MS', 'DF', 'GO')
sul = c('RS', 'SC', 'PR')
sudeste = c('SP', 'MG', 'ES', 'RJ')
norte = c('AC', 'AP', 'AM', 'PA', 'RO', 'RR', 'TO')

df5 <- st_read('brazil_geo.json') %>%
  mutate(Regiao = case_when(
    id %in% ne ~ 'Nordeste',
    id %in% co ~ 'Centro-Oeste',
    id %in% sul ~ 'Sul',
    id %in% sudeste ~ 'Sudeste',
    id %in% norte ~ 'Norte'
  )) %>%
  mutate(Valor = case_when(
    id %in% sudeste ~ 33,
    id %in% ne ~ 19,
    id %in% sul ~ 38,
    id %in% norte ~ 23,
    id %in% co ~ 22
  )) %>%
  ggplot(aes(fill = Valor)) +
  geom_sf() +
  theme_void() +
  labs(
    title = 'Critério 5',
    fill = 'Valor (%)'
  ) + 
  scale_fill_viridis_b() +
  theme(
    legend.position = 'bottom',
    legend.title.position = 'top'
  )

df6 <- st_read('brazil_geo.json') %>%
  mutate(Regiao = case_when(
    id %in% ne ~ 'Nordeste',
    id %in% co ~ 'Centro-Oeste',
    id %in% sul ~ 'Sul',
    id %in% sudeste ~ 'Sudeste',
    id %in% norte ~ 'Norte'
  )) %>%
  mutate(Valor = case_when(
    id %in% sudeste ~  10,
    id %in% ne ~ 6,
    id %in% sul ~ 13,
    id %in% norte ~ 12,
    id %in% co ~ 8
  )) %>%
  ggplot(aes(fill = Valor)) +
  geom_sf() +
  theme_void() +
  labs(
    title = 'Critério 6',
    fill = 'Valor (%)'
  ) +
  scale_fill_viridis_b() +
  theme(
    legend.position = 'bottom',
    legend.title.position = 'top'
  )

df5 + df6
