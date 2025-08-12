library(tidyverse)

dados <- read.csv('judicial-corruption-score.csv') %>%
  rename(Index = Judicial.corruption.score..central.estimate.) %>%
  filter(Year == 2024)

summary(dados$Index)

ggplot(dados, aes(x = Index)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = '') +
  theme(axis.text.y = element_blank())