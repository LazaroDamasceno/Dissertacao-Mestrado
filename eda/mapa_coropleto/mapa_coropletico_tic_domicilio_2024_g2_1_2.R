library(gt)
library(tidyverse)
library(sf)
library(readxl)
library(patchwork)

ne = c('MA', 'PI', 'CE', 'RN', 'PB', 'PE',  'BA', 'SE', 'AL')
co = c('MT', 'MS', 'DF', 'GO')
sul = c('RS', 'SC', 'PR')
sudeste = c('SP', 'MG', 'ES', 'RJ')
norte = c('AC', 'AP', 'AM', 'PA', 'RO', 'RR', 'TO')

g2_1 <- st_read('brazil_geo.json') %>%
  mutate(Regiao = case_when(
    id %in% ne ~ 'Nordeste',
    id %in% co ~ 'Centro-Oeste',
    id %in% sul ~ 'Sul',
    id %in% sudeste ~ 'Sudeste',
    id %in% norte ~ 'Norte',
  )) %>%
  mutate(Valor = case_when(
    id %in% ne ~ 23,
    id %in% co ~ 23,
    id %in% sul ~ 33,
    id %in% sudeste ~ 36,
    id %in% norte ~ 30,
  )) %>%
  ggplot(aes(fill = Valor)) +
    geom_sf() +
    labs(fill = 'Valor (%)', title='G2-1') +
    scale_fill_viridis_b() +
    theme_void()

g2_2 <- st_read('brazil_geo.json') %>%
  mutate(Regiao = case_when(
    id %in% ne ~ 'Nordeste',
    id %in% co ~ 'Centro-Oeste',
    id %in% sul ~ 'Sul',
    id %in% sudeste ~ 'Sudeste',
    id %in% norte ~ 'Norte',
  )) %>%
  mutate(Valor = case_when(
    id %in% ne ~ 24,
    id %in% co ~ 27,
    id %in% sul ~ 32,
    id %in% sudeste ~ 37,
    id %in% norte ~ 32,
  )) %>%
  ggplot(aes(fill = Valor)) +
  geom_sf() +
  labs(fill = 'Valor (%)', title='G2-2') +
  scale_fill_viridis_b() +
  theme_void()

g2_1 + g2_2
