library(tidyverse)
library(sf)

geo_data <- st_read('BR_UF_2024/')

geo_data

ne <- c('MA', 'PI', 'CE', 'RN', 'PB', 'PE', 'BA', 'SE', 'AL')
co <- c('MT', 'MS', 'DF', 'GO')
sul <- c('RS', 'SC', 'PR')
sudeste <- c('SP', 'MG', 'ES', 'RJ')
norte <- c('AC', 'AP', 'AM', 'PA', 'RO', 'RR', 'TO')

geo_data %>%
  mutate(Regiao = case_when(
    SIGLA_UF %in% ne ~ 'Nordeste',
    SIGLA_UF %in% co ~ 'Centro-Oeste',
    SIGLA_UF %in% sul ~ 'Sul',
    SIGLA_UF %in% sudeste ~ 'Sudeste',
    SIGLA_UF %in% norte ~ 'Norte'
  )) %>%
  mutate(Uso.Egov = case_when(
    SIGLA_UF %in% ne ~ 49.0,
    SIGLA_UF %in% co ~ 56.0,
    SIGLA_UF %in% sul ~ 72.0,
    SIGLA_UF %in% sudeste ~ 65.0,
    SIGLA_UF %in% norte ~ 60.0
  )) %>%
  ggplot(aes(fill = Uso.Egov)) +
    geom_sf() +
    labs(
      fill = "Valor (%)",
    ) +
    scale_fill_viridis_c() +
    theme_void() 
