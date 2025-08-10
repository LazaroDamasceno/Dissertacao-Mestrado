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
    SIGLA_UF %in% norte ~ 'Norte'
  )) %>%
  mutate(Valor = case_when(
    SIGLA_UF %in% sudeste ~ 39,
    SIGLA_UF %in% ne ~ 28,
    SIGLA_UF %in% sul ~ 40,
    SIGLA_UF %in% norte ~ 35,
    SIGLA_UF %in% co ~ 34
  )) %>%
  ggplot(aes(fill = Valor)) +
  geom_sf() +
  scale_fill_viridis_b() +
  theme_void() +
  labs(
    title = 'G3-1',
    fill = 'Valor (%)'
  )

df2 <- st_read('BR_UF_2024/') %>%
  mutate(Regiao = case_when(
    SIGLA_UF %in% ne ~ 'Nordeste',
    SIGLA_UF %in% co ~ 'Centro-Oeste',
    SIGLA_UF %in% sul ~ 'Sul',
    SIGLA_UF %in% sudeste ~ 'Sudeste',
    SIGLA_UF %in% norte ~ 'Norte'
  )) %>%
  mutate(Valor = case_when(
    SIGLA_UF %in% sudeste ~ 39,
    SIGLA_UF %in% ne ~ 24,
    SIGLA_UF %in% sul ~ 41,
    SIGLA_UF %in% norte ~ 35,
    SIGLA_UF %in% co ~ 30
  )) %>%
  ggplot(aes(fill = Valor)) +
  geom_sf() +
  scale_fill_viridis_b() +
  theme_void() +
  labs(
    title = 'G3-2',
    fill = 'Valor (%)'
  )

df3 <- st_read('BR_UF_2024/') %>%
  mutate(Regiao = case_when(
    SIGLA_UF %in% ne ~ 'Nordeste',
    SIGLA_UF %in% co ~ 'Centro-Oeste',
    SIGLA_UF %in% sul ~ 'Sul',
    SIGLA_UF %in% sudeste ~ 'Sudeste',
    SIGLA_UF %in% norte ~ 'Norte'
  )) %>%
  mutate(Valor = case_when(
    SIGLA_UF %in% sudeste ~ 50,
    SIGLA_UF %in% ne ~ 63,
    SIGLA_UF %in% sul ~ 46,
    SIGLA_UF %in% norte ~ 52,
    SIGLA_UF %in% co ~ 56
  )) %>%
  ggplot(aes(fill = Valor)) +
  geom_sf() +
  scale_fill_viridis_b() +
  theme_void() +
  labs(
    title = 'G3-3',
    fill = 'Valor (%)'
  )
  
layout <- '
  AB
  CD
'  

df1 + df2 + df3 + plot_layout(design = layout)
