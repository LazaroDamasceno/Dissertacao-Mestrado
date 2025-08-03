library(tidyverse)
library(tidyr)

df <- data.frame(
    EGDI = 16800,
    DGI = 563,
    GTMI = 418
)

df_longer <- pivot_longer(
    data = df,
    cols = everything(),
    names_to = 'Indices',
    values_to = 'Quantidade'
) %>%
mutate(Total = sum(Quantidade)) %>%
mutate(Quantidade = Quantidade / Total)

ggplot(df_longer, aes(x = "", y = Quantidade, fill = Indices)) +
    geom_bar(stat = "identity", width=1, color="white") +
    coord_polar("y", start=0) +
    theme_void() +
    labs(fill = 'Índice')

ggsave(
    'C:/Users/lazar/Master-Dissertation/figuras/egov_indices.png', 
    dpi=300,
    width = 10,
    height = 6,
    units = "in"
)
