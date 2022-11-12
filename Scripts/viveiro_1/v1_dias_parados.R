library(tidyverse)
library(googlesheets4)
library(ggthemes)
library(lubridate)
library(ggrepel)
library(scales)
library(corrplot)


biom <- read_sheet("1KkLM7bz-Az-etHUeENou-BjX4mDUfJCccwcCIo0k0CU", 2)


# Dias Parados Entre Ciclos -----------------------------------------------

v1_fallow <- biom %>%
  filter(viveiro == 1) %>%
  mutate(ano_desp = factor(year(data_desp)),
         ciclo = factor(ciclo)) %>%
  drop_na(fallow)

v1_fallow %>%
  ggplot(aes(ciclo, fallow)) +
  geom_bar(stat = "identity", width = 0.5, show.legend = FALSE,
           fill = "steelblue" ) +
  theme_light()+
  theme(legend.position = "none",
        panel.border = element_blank()) +
  labs(title = "Número de Dias Parados Entre Cultivos ",
       subtitle = "Viveiro 01",
       caption = "Fonte: Azul Marinho Aquicultura",
       y = "Dias Parados",
       x = "Ciclo de Cultivo") +
  theme(plot.caption = element_text(size = 8, color = "grey60")) +
  geom_text(aes(label = format(fallow,
                               big.mark = ".",
                               decimal.mark = ",")),
            vjust = 1.6,
            color = "white",
            size = 4.0,
            angle = 0)


# Dias Parados por Ano ----------------------------------------------------

v1_fallow <- biom %>%
  filter(viveiro == 1) %>%
  mutate(ano_desp = factor(year(data_desp)),
         ciclo = factor(ciclo)) %>%
  drop_na(fallow) %>% 
  group_by(ano_desp) %>%
  summarize(fallow = round(mean(fallow), 2))


  
  v1_fallow %>%
  ggplot(aes(ano_desp, fallow)) +
  geom_bar(stat = "identity", width = 0.5, show.legend = FALSE,
           fill = "steelblue" ) +
  theme_light()+
  theme(legend.position = "none",
        panel.border = element_blank()) +
  labs(title = "Número Médio de Dias Parados Entre Cultivos ",
       subtitle = "Viveiro 01",
       caption = "Fonte: Azul Marinho Aquicultura",
       y = "Dias Parados",
       x = "Ano") +
  theme(plot.caption = element_text(size = 8, color = "grey60")) +
  geom_text(aes(label = format(fallow,
                               big.mark = ".",
                               decimal.mark = ",")),
            vjust = 1.6,
            color = "white",
            size = 4.0,
            angle = 0)

