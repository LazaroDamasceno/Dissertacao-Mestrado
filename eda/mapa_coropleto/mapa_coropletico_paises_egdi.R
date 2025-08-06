library(tidyverse)
library(sf)

egdi = read.csv('EGOV_DATA_2024.csv') %>%
  arrange(Country.Name)

geo_data <- st_read('global_geo.json') %>%
  arrange(name)
  
setdiff(egdi$Country.Name, geo_data$name)

geo_data <- geo_data %>% 
  mutate(name = case_when(
    name == 'eSwatini' ~ 'Eswatini',
    name == 'The Bahamas' ~ 'Bahamas',
    name == 'Brunei' ~ "Brunei Darussalam",
    name == 'Republic of the Congo' ~ 'Congo',
    name == 'Czechia' ~ "Czech Republic",
    name == "Micronesia (Federated States of)"  ~ "Federated States of Micronesia",
    name == 'Vietnam' ~ 'Viet Nam',
    name == 'United Kingdom' ~ "United Kingdom of Great Britain and Northern Ireland",
    name == 'Republic of Serbia' ~ 'Serbia',
    name == 'Russia' ~ "Russian Federation",
    name == "Syria" ~ "Syrian Arab Republic",
    name == "Moldova" ~ "Republic of Moldova",
    name == "São Tomé and Principe" ~ "Sao Tome and Principe",
    name == 'Georgia' ~ 'Georgia (Country)',
    name == 'Laos' ~ "Lao People's Democratic Republic",
    name == "Ivory Coast" ~ "Côte d'Ivoire",
    name == 'Federated States of Micronesia' ~ 'Micronesia (Federated States of)',
    name == 'Iran' ~ 'Iran (Islamic Republic of)',
    name == 'Turkey' ~ 'Türkiye',
    name == 'East Timor' ~ 'Timor-Leste',
    name == 'South Korea' ~ 'Republic of Korea',
    name == 'North Korea' ~ 'Democratic People\'s Republic of Korea',
    TRUE ~ name
  )) %>% 
  arrange(name) %>%
  filter(name %in% egdi$Country.Name) %>%
  mutate(EGDI = egdi$E.Government.Index)

setdiff(egdi$Country.Name, geo_data$name)

geo_data$name == egdi$Country.Name

geo_data %>% ggplot(aes(fill = EGDI)) +
  geom_sf() +
  scale_fill_viridis_b(limits = c(0, 1)) +
  theme_void() +
  theme(
    legend.position = 'bottom',
    legend.title.position = 'top'
  )
