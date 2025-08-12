library(tidyverse)
library(gt)

dados <- read.csv('judicial-constraints-on-the-executive-index.csv')

dados <- dados %>% 
  rename(Index = Judicial.constraints.on.the.executive.index..central.estimate.) %>%
  filter(!is.na(Index)) %>%
  filter(Year == 2024)

summary(dados$Index)

ggplot(dados, aes(x = Index)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = 'Valores') +
  theme(axis.text.y = element_blank())
