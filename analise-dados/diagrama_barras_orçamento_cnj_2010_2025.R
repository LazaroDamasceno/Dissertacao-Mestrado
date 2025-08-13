library(tidyverse)
library(tidyr)
library(readxl)
library(patchwork)

df_2010_2017 <- read_xlsx('orçamento_justiça_2010_2025.xlsx') %>%
  filter(Orgao == 'CNJ') %>%
  select(Orgao, A2010, A2011, A2012, A2013, A2014, A2015, A2016, A2017) %>%
  pivot_longer(
    cols=-Orgao,
    names_to='Anos',
    values_to='Valores'
  ) %>%
  ggplot(aes(y = Anos, x = Valores)) +
  geom_col()  +
  geom_text(aes(label = Valores), hjust = 1.5, color = 'orange') +
  scale_y_discrete(labels = seq(2010, 2017)) +
  labs(
    x = '',
    y = ''
  ) +
  theme(
    x = element_blank(),
    y = element_blank()
  ) +
  theme_minimal()

df_2018_2025 <- read_xlsx('orçamento_justiça_2010_2025.xlsx') %>%
  filter(Orgao == 'CNJ') %>%
  select(Orgao, A2018, A2019, A2020, A2021, A2022, A2023, A2024, A2025) %>%
  pivot_longer(
    cols=-Orgao,
    names_to='Anos',
    values_to='Valores'
  ) %>%
  ggplot(aes(y = Anos, x = Valores)) +
  geom_col()  +
  geom_text(aes(label = Valores), hjust = 1.5, color = 'orange') +
  scale_y_discrete(labels = seq(2018, 2025)) +
  labs(
    x = '',
    y = ''
  ) +
  theme(
    x = element_blank(),
    y = element_blank()
  ) +
  theme_minimal()

 df_2018_2025 / df_2010_2017
