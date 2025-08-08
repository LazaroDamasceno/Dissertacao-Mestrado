library(tidyverse)
library(tidyr)
library(e1071)

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

egdi_brasil <- egdi %>% 
    filter(Country.Name == 'Brazil')

egdi_mundo <- egdi %>% 
    filter(Country.Name != 'Brazil')

print(skewness(egdi_mundo$E.Government.Index))

df = data.frame(
    paises = c(
        'Mundo', 
        'Brasil'),
    egdi = c(
        median(egdi_mundo$E.Government.Index),
        egdi_brasil$E.Government.Index
    ),
    epart = c(
        median(egdi_mundo$E.Participation.Index),
        egdi_brasil$E.Participation.Index
    ),
    osi = c(
        median(egdi_mundo$Online.Service.Index),
        egdi_brasil$Online.Service.Index
    ),
    hci = c(
        median(egdi_mundo$Human.Capital.Index),
        egdi_brasil$Human.Capital.Index
    ),
    tci = c(
        median(egdi_mundo$Telecommunication.Infrastructure.Index),
        egdi_brasil$Telecommunication.Infrastructure.Index
    )
)

df_long <- df %>%
  pivot_longer(cols = -paises,
               names_to = "indicador",
               values_to = "valor")

ggplot(df_long, aes(indicador, valor, fill = paises)) +
    geom_col(position = "dodge") +
    scale_x_discrete(labels = c('EGDI', 'E-Participation Index', 'HCI', 'OSI', 'TII')) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(
        y = 'Índices',
        x = 'Escala'
    ) +
    theme_bw() +
    coord_flip() +
    scale_fill_brewer(palette = 'Dark2')
