library(tidyverse)
library(sf)
library(readxl)
library(patchwork)

ne = c('MA', 'PI', 'CE', 'RN', 'PB', 'PE',  'BA', 'SE', 'AL')
co = c('MT', 'MS', 'DF', 'GO')
sul = c('RS', 'SC', 'PR')
sudeste = c('SP', 'MG', 'ES', 'RJ')
norte = c('AC', 'AP', 'AM', 'PA', 'RO', 'RR', 'TO')

df1 <- st_read('BR_UF_2024') %>%
  mutate(Regiao = case_when(
    SIGLA_UF %in% ne ~ 'Nordeste',
    SIGLA_UF %in% co ~ 'Centro-Oeste',
    SIGLA_UF %in% sul ~ 'Sul',
    SIGLA_UF %in% sudeste ~ 'Sudeste',
    SIGLA_UF %in% norte ~ 'Norte',
  )) %>%
  mutate(Valor = case_when(
    SIGLA_UF %in% ne ~ 23,
    SIGLA_UF %in% co ~ 23,
    SIGLA_UF %in% sul ~ 33,
    SIGLA_UF %in% sudeste ~ 36,
    SIGLA_UF %in% norte ~ 30,
  )) %>%
  ggplot(aes(fill = Valor)) +
    geom_sf() +
    labs(
      fill = 'Valor (%)',
      title = 'Critério 1'
    ) +
    scale_fill_viridis_b() +
    theme_void()

df2 <- st_read('BR_UF_2024') %>%
  mutate(Regiao = case_when(
    SIGLA_UF %in% ne ~ 'Nordeste',
    SIGLA_UF %in% co ~ 'Centro-Oeste',
    SIGLA_UF %in% sul ~ 'Sul',
    SIGLA_UF %in% sudeste ~ 'Sudeste',
    SIGLA_UF %in% norte ~ 'Norte',
  )) %>%
  mutate(Valor = case_when(
    SIGLA_UF %in% ne ~ 24,
    SIGLA_UF %in% co ~ 27,
    SIGLA_UF %in% sul ~ 32,
    SIGLA_UF %in% sudeste ~ 37,
    SIGLA_UF %in% norte ~ 32,
  )) %>%
  ggplot(aes(fill = Valor)) +
  geom_sf() +
  labs(
    fill = 'Valor (%)', 
    title = 'Critério 2'
  ) +
  scale_fill_viridis_b() +
  theme_void()

df1 + df2

