library(tidyverse)
library(googlesheets4)
library(ggthemes)
library(lubridate)
library(ggrepel)
library(scales)
library(corrplot)


biom <- read_sheet("1KkLM7bz-Az-etHUeENou-BjX4mDUfJCccwcCIo0k0CU", 2)


# Produ;cão por Ciclo -----------------------------------------------------

v1_kg_ciclo <- biom %>%
  filter(viveiro == 1) %>%
  mutate(ano_desp = factor(year(data_desp)),
         ciclo = factor(ciclo)) %>%
  group_by(ciclo) %>% 
  summarize(kg_total = sum(biom_real))
  



v1_kg_ciclo %>%
  ggplot(aes(ciclo, kg_total)) +
  geom_bar(stat = "identity", width = 0.5, show.legend = FALSE,
           fill = "steelblue" ) +
  theme_light()+
  theme(legend.position = "none",
        panel.border = element_blank()) +
  labs(title = "Produção por Ciclo de Cultivo",
       subtitle = "Viveiro 01",
       caption = "Fonte: Azul Marinho Aquicultura",
       y = "Produção (kg)",
       x = "Ciclo de Cultivo") +
  theme(plot.caption = element_text(size = 8, color = "grey60")) +
  geom_text(aes(label = format(kg_total,
                               big.mark = ".",
                               decimal.mark = ",")),
            vjust = 0.5,
            hjust = 1.1,
            color = "white",
            size = 4.0,
            angle = 90)


# Produção Anual ----------------------------------------------------------

v1_kg_ano <- biom %>%
  filter(viveiro == 1) %>%
  mutate(ano_desp = factor(year(data_desp)),
         ciclo = factor(ciclo)) %>%
  group_by(ano_desp) %>% 
  summarize(kg_total = sum(biom_real))

v1_kg_ano %>%
  ggplot(aes(ano_desp, kg_total)) +
  geom_bar(stat = "identity", width = 0.5, show.legend = FALSE,
           fill = "steelblue" ) +
  theme_light()+
  theme(legend.position = "none",
        panel.border = element_blank()) +
  labs(title = "Produção Anual",
       subtitle = "Viveiro 01",
       caption = "Fonte: Azul Marinho Aquicultura",
       y = "Produção (kg)",
       x = "Ano") +
  theme(plot.caption = element_text(size = 8, color = "grey60")) +
  geom_text(aes(label = format(kg_total,
                               big.mark = ".",
                               decimal.mark = ",")),
            vjust = 1.5,
            color = "white",
            size = 4.0,
            angle = 0)



