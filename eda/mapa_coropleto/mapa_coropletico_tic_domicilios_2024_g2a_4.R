library(tidyverse)
library(sf)
library(readxl)
library(patchwork)

ne = c('MA', 'PI', 'CE', 'RN', 'PB', 'PE',  'BA', 'SE', 'AL')
co = c('MT', 'MS', 'DF', 'GO')
sul = c('RS', 'SC', 'PR')
sudeste = c('SP', 'MG', 'ES', 'RJ')
norte = c('AC', 'AP', 'AM', 'PA', 'RO', 'RR', 'TO')

df1 <- st_read('BR_UF_2024/') %>%
  mutate(Regiao = case_when(
    SIGLA_UF %in% ne ~ 'Nordeste',
    SIGLA_UF %in% co ~ 'Centro-Oeste',
    SIGLA_UF %in% sul ~ 'Sul',
    SIGLA_UF %in% sudeste ~ 'Sudeste',
    SIGLA_UF %in% norte ~ 'Norte',
  )) %>%
  mutate(Valor = case_when(
    SIGLA_UF %in% sudeste ~ 12,
    SIGLA_UF %in% ne ~ 5,
    SIGLA_UF %in% sul ~ 13,
    SIGLA_UF %in% norte ~ 7,
    SIGLA_UF %in% co ~ 8,
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

df2 <- st_read('BR_UF_2024/') %>%
  mutate(Regiao = case_when(
    SIGLA_UF %in% ne ~ 'Nordeste',
    SIGLA_UF %in% co ~ 'Centro-Oeste',
    SIGLA_UF %in% sul ~ 'Sul',
    SIGLA_UF %in% sudeste ~ 'Sudeste',
    SIGLA_UF %in% norte ~ 'Norte',
  )) %>%
  mutate(Valor = case_when(
    SIGLA_UF %in% sudeste ~ 4,
    SIGLA_UF %in% ne ~ 3,
    SIGLA_UF %in% sul ~ 8,
    SIGLA_UF %in% norte ~ 4,
    SIGLA_UF %in% co ~ 2,
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

df3 <- st_read('BR_UF_2024/') %>%
  mutate(Regiao = case_when(
    SIGLA_UF %in% ne ~ 'Nordeste',
    SIGLA_UF %in% co ~ 'Centro-Oeste',
    SIGLA_UF %in% sul ~ 'Sul',
    SIGLA_UF %in% sudeste ~ 'Sudeste',
    SIGLA_UF %in% norte ~ 'Norte',
  )) %>%
  mutate(Valor = case_when(
    SIGLA_UF %in% sudeste ~ 12,
    SIGLA_UF %in% ne ~ 13,
    SIGLA_UF %in% sul ~ 11,
    SIGLA_UF %in% norte ~ 12,
    SIGLA_UF %in% co ~ 7,
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
