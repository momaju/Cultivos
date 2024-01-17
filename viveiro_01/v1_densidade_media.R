library(tidyverse)
library(googlesheets4)
library(ggthemes)
library(lubridate)
library(ggrepel)
library(scales)
library(corrplot)


biom <- read_sheet("1KkLM7bz-Az-etHUeENou-BjX4mDUfJCccwcCIo0k0CU", 2)




# Densidade Por Ciclo -------------------------------------------------------

biom %>%
  filter(viveiro == 1) %>% 
  mutate(ano_desp = factor(year(data_desp)),
         ciclo = factor(ciclo)) %>%
  group_by(ciclo) %>%
  summarize(densidade_media = round(mean(densidade), 2)) %>%
  ggplot(aes(x = ciclo, y = densidade_media)) +
  geom_bar(stat = "identity", width = 0.5, show.legend = FALSE,
           fill = "steelblue" ) +
  labs(title = "Densidade Média no Povoamento Por Ciclo de Cultivo",
       subtitle = "Viveiro 01",
       y = "Densidade Média",
       x = "Ciclo",
       caption = "Fonte: Azul Marinho Aquicultura") +
  theme_minimal() +
  theme(plot.caption = element_text(size = 7, color = "grey60")) +
  geom_text(aes(label = format(densidade_media,
                               big.mark = ".",
                               decimal.mark = ",")),
            vjust = 0.5,
            hjust = 1.1,
            color = "white",
            size = 4.0,
            angle = 90)




# Densidade por Ano -------------------------------------------------------

biom %>%
  filter(viveiro == 1) %>% 
  mutate(ano_desp = factor(year(data_desp)),
         ciclo = factor(ciclo)) %>%
  group_by(ano_desp) %>%
  summarize(densidade_media = round(mean(densidade), 2)) %>%
  ggplot(aes(x = ano_desp, y = densidade_media)) +
  #geom_line(aes(group = 1))+
  geom_bar(stat = "identity", width = 0.7, show.legend = FALSE,
           fill = "steelblue" ) +
  labs(title = "Densidade Média no Povoamento Por Ano de Cultivo",
       subtitle = "Viveiro 01",
       y = "Densidade Média",
       x = "Ano",
       caption = "Fonte: Azul Marinho Aquicultura") +
  theme_minimal() +
  theme(plot.caption = element_text(size = 7, color = "grey60")) +
  geom_text(aes(label = format(densidade_media,
                               big.mark = ".",
                               decimal.mark = ",")),
            vjust = 1.6,
            color = "white",
            size = 4.0)

