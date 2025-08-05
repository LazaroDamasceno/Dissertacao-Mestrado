library(tidyverse)
library(sf)
library(readxl)

egdi <- read.csv('EGOV_DATA_2024.csv')
mapa <- st_read('global_geo.json')
pib <- read.csv('pib_percapita_ppc.csv')

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

#print(length(pib$Country.Name))

pib <- pib %>%
  mutate(Country.Name = case_when(
    Country.Name == 'Bahamas, The' ~ 'Bahamas',
    Country.Name == 'Tanzania' ~ 'United Republic of Tanzania',
    Country.Name == 'Moldovia' ~ 'Republic of Korea',
    Country.Name == 'Georgia' ~ 'Georgia (Country)',
    Country.Name == 'Slovak Republic' ~ 'Slovakia',
    Country.Name == 'United Kingdom' ~ 'United Kingdom of Great Britain and Northern Ireland',
    Country.Name == 'United States' ~ 'United States of America',
    Country.Name == 'St. Kitts and Nevis' ~ 'Saint Kitts and Nevis',
    Country.Name == 'St. Vincent and the Grenadines' ~ 'Saint Vincent and the Grenadines',
    Country.Name == 'St. Lucia' ~ 'Saint Lucia',
    Country.Name == 'Czechia' ~ 'Czech Republic',
    Country.Name == 'Gambia, The' ~ 'Gambia',
    Country.Name == 'Kyrgyz Republic' ~ 'Kyrgyzstan',
    Country.Name == 'Lao PDR' ~ 'Lao People\'s Democratic Republic',
    Country.Name == 'Venezuela, RB' ~ 'Venezuela',
    Country.Name == 'Korea, Rep.' ~ 'Republic of Korea',
    Country.Name == 'Korea, Dem. People\'s Rep.' ~ 'Democratic People\'s Republic of Korea',
    Country.Name == 'Congo, Rep.' ~ 'Congo',
    Country.Name == 'Egypt, Arab Rep.' ~ 'Egypt',
    Country.Name == 'Moldova' ~ 'Republic of Moldova',
    Country.Name == 'Turkiye' ~ 'Türkiye',
    Country.Name == 'Yemen, Rep.' ~ 'Yemen',
    Country.Name == 'Micronesia, Fed. Sts.' ~ 'Micronesia (Federated States of)',
    Country.Name == 'Iran, Islamic Rep.' ~ 'Iran (Islamic Republic of)',
    Country.Name == 'Congo, Dem. Rep.' ~ 'Democratic Republic of the Congo',
    Country.Name == 'Cote d\'Ivoire' ~ 'Côte d\'Ivoire',
    TRUE ~ Country.Name 
  )) %>%
  arrange(Country.Name)

#print(setdiff(egdi$Country.Name, pib$Country.Name))

mapa %>%
  left_join(pib, by = c("name" = "Country.Name")) %>%
  rename(PIB = X2024) %>%
  ggplot(aes(fill = PIB)) +
  labs(
    fill = 'PIB'
  ) +
  geom_sf() +
  scale_fill_viridis_b()
