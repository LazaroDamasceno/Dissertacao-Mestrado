library(tidyverse)
library(sf)
library(readxl)
library(patchwork)

ne = c('MA', 'PI', 'CE', 'RN', 'PB', 'PE',  'BA', 'SE', 'AL')
co = c('MT', 'MS', 'DF', 'GO')
sul = c('RS', 'SC', 'PR')
sudeste = c('SP', 'MG', 'ES', 'RJ')
norte = c('AC', 'AP', 'AM', 'PA', 'RO', 'RR', 'TO')

df1 <- st_read('brazil_geo.json') %>%
  mutate(Regiao = case_when(
    id %in% ne ~ 'Nordeste',
    id %in% co ~ 'Centro-Oeste',
    id %in% sul ~ 'Sul',
    id %in% sudeste ~ 'Sudeste',
    id %in% norte ~ 'Norte',
  )) %>%
  mutate(Valor = case_when(
    id %in% sudeste ~ 8,
    id %in% ne ~ 5,
    id %in% sul ~ 12,
    id %in% norte ~ 8,
    id %in% co ~ 7,
  )) %>%
  ggplot(aes(fill = Valor)) +
  geom_sf() +
  labs(
    fill = 'Valor (%)',
    title = 'SC1'
  ) +
  theme_void() +
  scale_fill_viridis_b() +
  theme(
    legend.position = 'bottom',
    legend.title.position = 'top'
  )

df2 <- st_read('brazil_geo.json') %>%
  mutate(Regiao = case_when(
    id %in% ne ~ 'Nordeste',
    id %in% co ~ 'Centro-Oeste',
    id %in% sul ~ 'Sul',
    id %in% sudeste ~ 'Sudeste',
    id %in% norte ~ 'Norte',
  )) %>%
  mutate(Valor = case_when(
    id %in% sudeste ~ 4,
    id %in% ne ~ 2,
    id %in% sul ~ 4,
    id %in% norte ~ 4,
    id %in% co ~ 2,
  )) %>%
  ggplot(aes(fill = Valor)) +
  geom_sf() +
  labs(
    fill = 'Valor (%)',
    title = 'SC2'
  ) +
  theme_void() +
  scale_fill_viridis_b() +
  theme(
    legend.position = 'bottom',
    legend.title.position = 'top'
  )

df3 <- st_read('brazil_geo.json') %>%
  mutate(Regiao = case_when(
    id %in% ne ~ 'Nordeste',
    id %in% co ~ 'Centro-Oeste',
    id %in% sul ~ 'Sul',
    id %in% sudeste ~ 'Sudeste',
    id %in% norte ~ 'Norte',
  )) %>%
  mutate(Valor = case_when(
    id %in% sudeste ~ 13,
    id %in% ne ~ 10,
    id %in% sul ~ 6,
    id %in% norte ~ 13,
    id %in% co ~ 4,
  )) %>%
  ggplot(aes(fill = Valor)) +
  geom_sf() +
  labs(
    fill = 'Valor (%)',
    title = 'SC3'
  ) +
  theme_void() +
  scale_fill_viridis_b() +
  theme(
    legend.position = 'bottom',
    legend.title.position = 'top'
  )

df1 + df2 + df3
