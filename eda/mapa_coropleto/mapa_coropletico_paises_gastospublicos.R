library(tidyverse)
library(sf)
library(readxl)

egdi <- read.csv('EGOV_DATA_2024.csv')
mapa <- st_read('global_geo.json')
gastos_publicos <- read_xls('imf_government_expenditure.xls')

mapa <- mapa %>%
  mutate(name = case_when(
    name == 'The Bahamas' ~ 'Bahamas',
    name == 'Georgia' ~ 'Georgia (Country)',
    name == 'Vietnam' ~ 'Viet Nam',
    name == 'Brunei' ~ 'Brunei Darussalam',
    name == 'East Timor' ~ 'Timor-Leste',
    name == 'Ivory Coast' ~ 'Côte d\'Ivoire',
    name == 'Czechia' ~ 'Czech Republic',
    name == 'Syria' ~ 'Syrian Arab Republic',
    name == 'Russia' ~ 'Russian Federation',
    name == 'eSwatini' ~ 'Eswatini',
    name == 'Moldova' ~ 'Republic of Moldova',
    name == 'Laos' ~'Lao People\'s Democratic Republic',
    name == 'Republic of the Congo' ~ 'Congo',
    name == 'Republic of Serbia' ~ 'Serbia',
    name == 'Turkey' ~ 'Türkiye',
    name == 'South Korea' ~ 'Republic of Korea',
    name == 'North Korea' ~ 'Democratic People\'s Republic of Korea',
    name == 'United Kingdom' ~ 'United Kingdom of Great Britain and Northern Ireland',
    name == 'São Tomé and Principe' ~ 'Sao Tome and Principe',
    name == 'Iran' ~ 'Iran (Islamic Republic of)',
    name == "Federated States of Micronesia" ~ "Micronesia (Federated States of)",
    TRUE ~ name
  )) %>%
  arrange(name)

#print(setdiff(egdi$Country.Name, mapa$name))

gastos_publicos <- gastos_publicos %>%
  mutate(Country = case_when(
    Country == 'Korea, Republic of' ~ 'Republic of Korea',
    Country == 'Republic of Congo' ~ 'Congo',
    Country == 'Slovak Republic' ~ 'Slovakia',
    Country == 'United States' ~ 'United States of America',
    Country == 'Tanzania' ~ 'United Republic of Tanzania',
    Country == 'Iran' ~ 'Iran (Islamic Republic of)',
    Country == 'Micronesia, Fed. States of' ~ 'Micronesia (Federated States of)',
    Country == 'Moldova' ~ 'Republic of Moldova',
    Country == 'Türkiye, Republic of' ~ 'Türkiye',
    Country == 'Georgia' ~ 'Georgia (Country)',
    Country == 'Vietnam' ~ 'Viet Nam',
    Country == 'Kyrgyz Republic' ~ 'Kyrgyzstan',
    Country == 'United Kingdom' ~ 'United Kingdom of Great Britain and Northern Ireland',
    Country == 'China, People\'s Republic of' ~ 'China',
    TRUE ~ Country
  )) %>%
  arrange(Country) %>%
  filter(Country %in% egdi$Country.Name)

#print(setdiff(gastos_publicos$Country, egdi$Country.Name))

#print(length(gastos_publicos$Country))

mapa %>%
  left_join(gastos_publicos, by = c("name" = "Country")) %>%
  rename(Gastos.Publicos = Percentage) %>%
  ggplot(aes(fill = Gastos.Publicos)) +
  labs(fill = 'Gastos públicos (% do PIB)') +
  geom_sf() +
  scale_fill_viridis_b(lim = c(0, 100)) +
  theme_void() +
  theme(legend.position = 'bottom')
