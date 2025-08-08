library(tidyverse)
library(readxl)

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

gastos_publicos <- read_excel('imf_government_expenditure.xls') %>% 
    arrange(Country)

gastos_publicos <- gastos_publicos %>% 
    mutate(Country = case_when(
        Country == 'Vietnam' ~ 'Viet Nam',
        Country == 'Georgia' ~ 'Georgia (Country)',
        Country == 'China, People\'s Republic of' ~ 'China',
        Country == 'China, People\'s Republic of' ~ 'China',
        Country == 'Republic of Congo' ~ 'Congo',
        Country == 'Türkiye, Republic of' ~ 'Türkiye',
        Country == "Micronesia, Fed. States of" ~ "Micronesia (Federated States of)",
        Country == "United States" ~ "United States of America",
        Country == "United Kingdom" ~ "United Kingdom of Great Britain and Northern Ireland",
        Country == "Tanzania" ~ "United Republic of Tanzania",
        Country == "Slovak Republic" ~ "Slovakia",
        Country == "Moldova" ~ "Republic of Moldova",
        Country == "Kyrgyz Republic" ~ "Kyrgyzstan",
        Country == "Korea, Republic of" ~ "Republic of Korea",
        Country == "Iran" ~ "Iran (Islamic Republic of)",
        TRUE ~ Country
    ))

gastos_publicos <- gastos_publicos %>%
    filter(Country %in% egdi$Country.Name)

egdi <- egdi %>%
    filter(Country.Name %in% gastos_publicos$Country)

df <- data.frame(
    indices = c(
        'EGDI',
        'EPI ',
        'OSI',
        'HCI',
        'TCI'
    ),
    correlacoes = c(
        cor(gastos_publicos$Percentage, egdi$E.Government.Index, method = 'spearman'),
        cor(gastos_publicos$Percentage, egdi$E.Participation.Index, method = 'spearman'),
        cor(gastos_publicos$Percentage, egdi$Online.Service.Index, method = 'spearman'),
        cor(gastos_publicos$Percentage, egdi$Human.Capital.Index, method = 'spearman'),
        cor(gastos_publicos$Percentage, egdi$Telecommunication.Infrastructure.Index, method = 'spearman')
    )
)

ggplot(df, aes(indices, correlacoes)) +
    geom_col(fill = "steelblue") +
    labs(
        y = 'Valor',
        x = 'Componentes'
    ) +
    theme_bw() +
    coord_flip()
