library(tidyverse)
library(tidyr)

df_1 <- data.frame(
    regioes = c('Sudeste', 'Nordeste', 'Sul', 'Norte', 'Centro-Oeste'),
    C1 = c(12, 5, 13, 7, 7),
    C2 = c(12, 6, 9, 9, 10),
    C3 = c(12, 12, 10, 13, 5)
) %>% pivot_longer(
    cols = -regioes,
    names_to = 'criterios',
    values_to = 'valores'
) %>% ggplot(aes(x = criterios, y = valores, fill = regioes)) +
    geom_col(position = 'dodge') +
    scale_fill_brewer(palette = 'Dark2') +
    labs(
        y = 'Porcentagem',
        x = 'Subcritério',
        fill = 'Regiões'
    ) +
    theme(legend.title = element_text())

df_2 <- data.frame(
    regioes = c('Sudeste', 'Nordeste', 'Sul', 'Norte', 'Centro-Oeste'),
    C1 = c(10, 6, 9, 6, 7),
    C2 = c(11, 8, 16, 14, 15),
    C3 = c(16, 10, 7, 11, 6)
) %>% pivot_longer(
    cols = -regioes,
    names_to = 'criterios',
    values_to = 'valores'
) %>% ggplot(aes(x = criterios, y = valores, fill = regioes)) +
    geom_col(position = 'dodge') +
    scale_fill_brewer(palette = 'Dark2') +
    labs(
        y = 'Porcentagem',
        x = 'Subcritério',
        fill = 'Regiões'
    ) +
    theme(legend.title = element_text())

ggsave(
    'C:/Users/lazar/Master-Dissertation/figuras/tic_domiciilios_2024_g2a_2.png', 
    dpi=300,
    width = 25.4,
    height = 15.24,
    units = "cm"
)