library(tidyverse)
library(sf)

geo_data <- st_read('brazil_geo.json')

ne <- c('MA', 'PI', 'CE', 'RN', 'PB', 'PE', 'BA', 'SE', 'AL')
co <- c('MT', 'MS', 'DF', 'GO')
sul <- c('RS', 'SC', 'PR')
sudeste <- c('SP', 'MG', 'ES', 'RJ')
norte <- c('AC', 'AP', 'AM', 'PA', 'RO', 'RR', 'TO')

geo_data <- geo_data %>%
  mutate(Regiao = case_when(
    id %in% ne ~ 'Nordeste',
    id %in% co ~ 'Centro-Oeste',
    id %in% sul ~ 'Sul',
    id %in% sudeste ~ 'Sudeste',
    id %in% norte ~ 'Norte'
  )) %>%
  mutate(Uso.Egov = case_when(
    id %in% ne ~ 49.0,
    id %in% co ~ 56.0,
    id %in% sul ~ 72.0,
    id %in% sudeste ~ 65.0,
    id %in% norte ~ 60.0
  )) %>%
  ggplot(aes(fill = Uso.Egov)) +
    geom_sf() +
    labs(
      fill = "Uso de e-gov (%)",
      caption = "E-gov significa governo eletrônico."
    ) +
    scale_fill_viridis_c()

ggsave(
  'C:/Users/lazar/Master-Dissertation/figuras/mapa_coropleto_tic_domicilio_g1.png',
  dpi = 300,
  width = 10,
  height = 6,
  units = 'in'
)

print(geo_data)
