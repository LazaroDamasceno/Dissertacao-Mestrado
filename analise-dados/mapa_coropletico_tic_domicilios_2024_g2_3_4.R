library(tidyverse)
library(sf)
library(readxl)
library(patchwork)

ne = c('MA', 'PI', 'CE', 'RN', 'PB', 'PE',  'BA', 'SE', 'AL')
co = c('MT', 'MS', 'DF', 'GO')
sul = c('RS', 'SC', 'PR')
sudeste = c('SP', 'MG', 'ES', 'RJ')
norte = c('AC', 'AP', 'AM', 'PA', 'RO', 'RR', 'TO')

df3 <- st_read('BR_UF_2024/') %>%
  mutate(Regiao = case_when(
    SIGLA_UF %in% ne ~ 'Nordeste',
    SIGLA_UF %in% co ~ 'Centro-Oeste',
    SIGLA_UF %in% sul ~ 'Sul',
    SIGLA_UF %in% sudeste ~ 'Sudeste',
    SIGLA_UF %in% norte ~ 'Norte'
  )) %>% 
  mutate(Valor = case_when(
    SIGLA_UF %in% ne ~ 17,
    SIGLA_UF %in% co ~ 13,
    SIGLA_UF %in% sul ~ 22,
    SIGLA_UF %in% sudeste ~ 25,
    SIGLA_UF %in% norte ~ 26
  )) %>% 
  ggplot(aes(fill = Valor)) +
    geom_sf() +
    labs(
      fill = 'Valor (%)',
      title = 'Critério 3'
    ) +
    theme_void() +
    scale_fill_viridis_b()

df4 <- st_read('BR_UF_2024/') %>%
  mutate(Regiao = case_when(
    SIGLA_UF %in% ne ~ 'Nordeste',
    SIGLA_UF %in% co ~ 'Centro-Oeste',
    SIGLA_UF %in% sul ~ 'Sul',
    SIGLA_UF %in% sudeste ~ 'Sudeste',
    SIGLA_UF %in% norte ~ 'Norte'
  )) %>% 
  mutate(Valor = case_when(
    SIGLA_UF %in% ne ~ 20,
    SIGLA_UF %in% co ~ 18,
    SIGLA_UF %in% sul ~ 32,
    SIGLA_UF %in% sudeste ~ 28,
    SIGLA_UF %in% norte ~ 23
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
