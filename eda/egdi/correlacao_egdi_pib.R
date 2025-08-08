library(tidyverse)
library(tidyr)

egdi <- read.csv('EGOV_DATA_2024.csv') %>%
    arrange(Country.Name) %>%
    select(
            Country.Name,
            E.Government.Index,
            E.Participation.Index, 
            Online.Service.Index, 
            Human.Capital.Index, 
            Telecommunication.Infrastructure.Index
        )

pib <- read.csv('pib_percapita_ppc.csv') %>%
    select(Country.Name, X2024) %>%
    arrange(Country.Name) %>% 
    replace_na(list(X2024 = 0))

pib <- pib %>% 
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

df <- data.frame(
    correlacoes = c(
        cor(pib$X2024, egdi$E.Government.Index, method = 'spearman'),
        cor(pib$X2024, egdi$E.Participation.Index, method = 'spearman'),
        cor(pib$X2024, egdi$Online.Service.Index, method = 'spearman'),
        cor(pib$X2024, egdi$Human.Capital.Index, method = 'spearman'),
        cor(pib$X2024, egdi$Telecommunication.Infrastructure.Index, method = 'spearman')
    ),
    indices = c(
        'EGDI',
        'EPI',
        'OSI',
        'HCI',
        'TCI'
    )
)

ggplot(df, aes(indices, correlacoes)) +
    geom_col(fill = "steelblue") +
    labs(
        x = 'Componentes',
        y = 'Valor'
    ) +
    coord_cartesian(xlim = c(0, 1)) +
    coord_flip()
