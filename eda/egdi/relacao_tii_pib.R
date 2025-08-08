library(tidyverse)
library(tidyr)

egdi <- read.csv('EGOV_DATA_2024.csv') %>%
    select(Country.Name, Telecommunication.Infrastructure.Index) %>%
    arrange(Country.Name)

countries <- unique(egdi$Country.Name)

gdp <- read.csv('pib_percapita_ppc.csv') %>%
    select(Country.Name, X2024) %>%
    arrange(Country.Name) %>% 
    replace_na(list(X2024 = 0))

setdiff(egdi$Country.Name, gdp$Country.Name)

gdp <- gdp %>% 
    mutate(Country.Name = case_when(
        Country.Name == 'Bahamas, The' ~ 'Bahamas',
        Country.Name == 'Congo, Rep.' ~ 'Congo',
        Country.Name == 'Czechia' ~ 'Czech Republic',
        Country.Name == 'Cote d\'Ivoire' ~ 'Côte d\'Ivoire',
        Country.Name == 'Egypt, Arab Rep.' ~ 'Egypt',
        Country.Name == 'Gambia, The' ~ 'Gambia',
        Country.Name == 'Georgia' ~ 'Georgia (Country)',
        Country.Name == 'Micronesia, Fed. Sts.' ~ 'Micronesia (Federated States of)',
        Country.Name == 'Turkiye' ~ 'Türkiye',
        Country.Name == 'Iran, Islamic Rep.' ~ 'Iran (Islamic Republic of)',
        Country.Name == 'Congo, Dem. Rep.' ~ 'Democratic Republic of the Congo',
        Country.Name == 'Korea, Rep.' ~ 'Republic of Korea',
        Country.Name == 'Moldova' ~ 'Republic of Moldova',
        Country.Name == 'Korea, Dem. People\'s Rep.' ~ 'Democratic People\'s Republic of Korea',
        Country.Name == 'Slovak Republic' ~ 'Slovakia',
        Country.Name == 'St. Kitts and Nevis' ~ 'Saint Kitts and Nevis',
        Country.Name == 'St. Lucia' ~ 'Saint Lucia',
        Country.Name == 'St. Vincent and the Grenadines' ~ 'Saint Vincent and the Grenadines',
        Country.Name == 'Yemen, Rep.' ~ 'Yemen',
        Country.Name == 'Venezuela, RB' ~ 'Venezuela',
        Country.Name == 'United Kingdom' ~ 'United Kingdom of Great Britain and Northern Ireland',
        Country.Name == 'Tanzania' ~ 'United Republic of Tanzania',
        Country.Name == 'United States' ~ 'United States of America',
        Country.Name == 'Lao PDR' ~ 'Lao People\'s Democratic Republic',
        Country.Name == 'Kyrgyz Republic' ~ 'Kyrgyzstan',
        TRUE ~ Country.Name
    )) %>%
    filter(Country.Name %in% egdi$Country.Name) %>%
    arrange(Country.Name)

setdiff(egdi$Country.Name, gdp$Country.Name)

length(gdp$Country.Name)

df <- data.frame(
    egdi = egdi$Telecommunication.Infrastructure.Index,
    gdp = gdp$X2024
)

ggplot(df, aes(x = egdi, y = gdp)) +
    geom_point() +
    geom_smooth(method = 'lm') +
    labs(
        x = 'TII',
        y = 'PIB per capita PPC'
    ) +
    coord_cartesian(xlim = c(0, 1)) +
  theme_bw()
