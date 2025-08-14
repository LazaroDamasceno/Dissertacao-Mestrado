library(tidyverse)
library(tidyr)
library(readxl)

read_xlsx('orçamento_justiça_2010_2025.xlsx') %>%
  pivot_longer(
    cols = -Orgao,
    names_to = 'Anos',
    values_to = 'Valores'
  )  %>%
  group_by(Anos) %>%
  summarise(Total_por_ano = sum(Valores)) %>%
  ggplot(aes(x = Anos, y = Total_por_ano)) +
  geom_point() +
  theme_minimal() +
  labs(
    x = '', 
    y = ''
  ) +
  theme(
    axis.x.text = element_blank(), 
    axis.y.text = element_blank()
  ) +
  scale_x_discrete(labels = seq(2010, 2025))
