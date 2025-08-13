library(tidyverse)
library(tidyr)
library(readxl)

orgaos <-  read_xlsx('orçamento_justiça_2010_2025.xlsx') %>%
    mutate(A2010 = (A2010 / sum(A2010)) * 100) %>%
    mutate(A2011 = (A2011 / sum(A2011)) * 100) %>%
    mutate(A2012 = (A2012 / sum(A2012)) * 100) %>%
    mutate(A2013 = (A2013 / sum(A2013)) * 100) %>%
    mutate(A2014 = (A2014 / sum(A2014)) * 100) %>%
    mutate(A2015 = (A2015 / sum(A2015)) * 100) %>%
    mutate(A2016 = (A2016 / sum(A2016)) * 100) %>%
    mutate(A2017 = (A2017 / sum(A2017)) * 100) %>%
    mutate(A2018 = (A2018 / sum(A2018)) * 100) %>%
    mutate(A2019 = (A2019 / sum(A2019)) * 100) %>%
    mutate(A2020 = (A2020 / sum(A2020)) * 100) %>%
    mutate(A2021 = (A2021 / sum(A2021)) * 100) %>%
    mutate(A2022 = (A2022 / sum(A2022)) * 100) %>%
    mutate(A2023 = (A2023 / sum(A2023)) * 100) %>%
    mutate(A2024 = (A2024 / sum(A2024)) * 100) %>%
    mutate(A2025 = (A2025 / sum(A2025)) * 100) %>%
    pivot_longer(
        cols=-Orgao,
        names_to='Anos',
        values_to='Valores'
    ) %>%
    ggplot(aes(y = Anos, x = Valores, fill = Orgao)) +
    geom_col()  +
    scale_fill_brewer(palette = 'Dark2') +
    scale_y_discrete(labels = seq(2010, 2025)) +
    labs(
      x = 'Porcentagem', 
      fill = 'Órgãos'
    ) +
    theme_minimal() +
    theme(legend.title = element_text()) 
