library(tidyverse)
library(sf)
library(readxl)
library(patchwork)

ne = c('MA', 'PI', 'CE', 'RN', 'PB', 'PE',  'BA', 'SE', 'AL')
co = c('MT', 'MS', 'DF', 'GO')
sul = c('RS', 'SC', 'PR')
sudeste = c('SP', 'MG', 'ES', 'RJ')
norte = c('AC', 'AP', 'AM', 'PA', 'RO', 'RR', 'TO')

df5 <- st_read('BR_UF_2024/') %>%
  mutate(Regiao = case_when(
    SIGLA_UF %in% ne ~ 'Nordeste',
    SIGLA_UF %in% co ~ 'Centro-Oeste',
    SIGLA_UF %in% sul ~ 'Sul',
    SIGLA_UF %in% sudeste ~ 'Sudeste',
    SIGLA_UF %in% norte ~ 'Norte'
  )) %>%
  mutate(Valor = case_when(
    SIGLA_UF %in% sudeste ~ 33,
    SIGLA_UF %in% ne ~ 19,
    SIGLA_UF %in% sul ~ 38,
    SIGLA_UF %in% norte ~ 23,
    SIGLA_UF %in% co ~ 22
  )) %>%
  ggplot(aes(fill = Valor)) +
  geom_sf() +
  theme_void() +
  labs(
    title = 'Critério 5',
    fill = 'Valor (%)'
  ) +
  scale_fill_viridis_b()

st_read('BR_UF_2024/') %>%
  mutate(Regiao = case_when(
    SIGLA_UF %in% ne ~ 'Nordeste',
    SIGLA_UF %in% co ~ 'Centro-Oeste',
    SIGLA_UF %in% sul ~ 'Sul',
    SIGLA_UF %in% sudeste ~ 'Sudeste',
    SIGLA_UF %in% norte ~ 'Norte'
  )) %>%
  mutate(Valor = case_when(
    SIGLA_UF %in% sudeste ~  20,
    SIGLA_UF %in% ne ~ 6,
    SIGLA_UF %in% sul ~ 16,
    SIGLA_UF %in% norte ~ 10,
    SIGLA_UF %in% co ~ 8
  )) %>%
  ggplot(aes(fill = Valor)) +
  geom_sf() +
  theme_void() +
  labs(
    fill = 'Valor (%)'
  ) +
  scale_fill_viridis_b() 
