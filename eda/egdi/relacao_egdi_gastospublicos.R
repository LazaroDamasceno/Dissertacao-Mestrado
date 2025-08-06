library(tidyverse)
library(readxl)

egdi <- read.csv('EGOV_DATA_2024.csv') %>% 
    select(Country.Name, E.Government.Index) %>%
    arrange(Country.Name)

gastos_publicos <- read_excel('imf_government_expenditure.xls') %>% 
    arrange(Country)

setdiff(gastos_publicos$Country, egdi$Country.Name)

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

setdiff(gastos_publicos$Country, egdi$Country.Name)

gastos_publicos <- gastos_publicos %>%
    filter(Country %in% egdi$Country.Name)

egdi <- egdi %>%
    filter(Country.Name %in% gastos_publicos$Country)

data.frame(
    egdi = egdi$E.Government.Index,
    gastos_publicos = gastos_publicos$Percentage
  ) %>%
  ggplot(aes(egdi, gastos_publicos)) +
      geom_point() +
      geom_smooth(method = 'lm') +
      coord_cartesian(ylim = c(0, 100), xlim = c(0, 1)) +
      labs(
          x = 'EGDI',
          y = 'Gastos públicos (% do PIB)'
      ) +
      theme_bw()
