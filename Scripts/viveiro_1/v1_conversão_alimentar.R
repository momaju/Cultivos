library(tidyverse)
library(googlesheets4)
library(ggthemes)
library(lubridate)
library(ggrepel)
library(scales)
library(corrplot)


biom <- read_sheet("1KkLM7bz-Az-etHUeENou-BjX4mDUfJCccwcCIo0k0CU", 2)

# tca por ciclo de cultivo -----------------------------------------------

v1_tca_ciclo <- biom %>%
  filter(viveiro == 1) %>%
  mutate(ano_desp = factor(year(data_desp)),
         ciclo = factor(ciclo))

v1_tca_ciclo %>%
  ggplot(aes(ciclo, tca)) +
  geom_bar(stat = "identity", width = 0.7, show.legend = FALSE,
           fill = "steelblue" ) +
  theme_light()+
  theme(legend.position = "none",
        panel.border = element_blank()) +
  labs(title = "Taxa de Conversão Alimentar por Ciclo",
       subtitle = "Viveiro 01",
       caption = "Fonte: Azul Marinho Aquicultura",
       y = "Conversão Alimentar",
       x = "Ciclo") +
  theme(plot.caption = element_text(size = 8, color = "grey60")) +
  geom_text(aes(label = format(tca,
                               big.mark = ".",
                               decimal.mark = ",")),
            vjust = 1.5,
            hjust = 0.55,
            color = "white",
            size = 3.5,
            angle = 0)  

# tca média anual ----------------------------------------------------------


v1_tca_ano <- biom %>%
  filter(viveiro == 1) %>%
  mutate(ano_desp = factor(year(data_desp)),
         ciclo = factor(ciclo)) %>%  
  group_by(ano_desp) %>% 
  summarize(tca = round(mean(tca, na.rm = TRUE),2))

v1_tca_ano %>%
  ggplot(aes(ano_desp, tca)) +
  geom_bar(stat = "identity", width = 0.5, show.legend = FALSE,
           fill = "steelblue" ) +
  theme_light()+
  theme(legend.position = "none",
        panel.border = element_blank()) +
  labs(title = "Conversão Alimentar Média Anual",
       subtitle = "Viveiro 01",
       caption = "Fonte: Azul Marinho Aquicultura",
       y = "Conversão Alimentar",
       x = "Ano") +
  theme(plot.caption = element_text(size = 8, color = "grey60")) +
  geom_text(aes(label = format(tca,
                               big.mark = ".",
                               decimal.mark = ",")),
            vjust = 1.5,
            color = "white",
            size = 4.0,
            angle = 0)

