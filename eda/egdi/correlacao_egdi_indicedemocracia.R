library(tidyverse)

egdi <- read.csv('EGOV_DATA_2024.csv') %>%
  arrange(Country.Name)

indice_democracia <- read.csv('electoral-democracy-index.csv') %>%
  rename(Index = Electoral.democracy.index..central.estimate.) %>%
  select(Entity, Year, Index) %>%
  filter(Year == 2024) %>%
  arrange(Entity) %>%
  mutate(Entity = case_when(
    Entity == 'Georgia' ~ 'Georgia (Country)',
    Entity == 'Brunei' ~ 'Brunei Darussalam',
    Entity == 'Czechia' ~ 'Czech Republic',
    Entity == 'Laos' ~ 'Lao People\'s Democratic Republic',
    Entity == 'Vietnam' ~ 'Viet Nam',
    Entity == 'Cape Verde' ~ 'Cabo Verde',
    Entity == 'United Kingdom' ~ 'United Kingdom of Great Britain and Northern Ireland',
    Entity == 'Tanzania' ~ 'United Republic of Tanzania',
    Entity == 'United States' ~ 'United States of America',
    Entity == 'Micronesia (country)' ~ 'Micronesia (Federated States of)',
    Entity == 'South Korea' ~ 'Republic of Korea',
    Entity == 'Iran' ~ 'Iran (Islamic Republic of)',
    Entity == 'Russia' ~ 'Russian Federation',
    Entity == 'Syria' ~ 'Syrian Arab Republic',
    Entity == 'North Korea' ~ 'Democratic People\'s Republic of Korea',
    Entity == 'Democratic Republic of Congo' ~  'Democratic Republic of the Congo',
    Entity == 'Cote d\'Ivoire' ~ 'Côte d\'Ivoire',
    Entity == 'Moldova' ~  'Republic of Moldova',
    Entity == 'Turkey' ~  'Türkiye',
    Entity == 'East Timor' ~  'Timor-Leste',
    TRUE ~ Entity
  )) %>%
  filter(Entity %in% egdi$Country.Name)

egdi <- egdi %>%
  filter(Country.Name %in% indice_democracia$Entity)

cor(egdi$E.Government.Index, indice_democracia$Index, method = 'spearman')
