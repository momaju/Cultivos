library(tidyverse)
library(googlesheets4)
library(ggthemes)
library(lubridate)
library(ggrepel)
library(scales)
library(corrplot)


biom <- read_sheet("1KkLM7bz-Az-etHUeENou-BjX4mDUfJCccwcCIo0k0CU", 2)

# ddc por Ciclo de Cultivo -----------------------------------------------

v1_ddc_ciclo <- biom %>%
  filter(viveiro == 1) %>%
  mutate(ano_desp = factor(year(data_desp)),
         ciclo = factor(ciclo))

v1_ddc_ciclo %>%
  ggplot(aes(ciclo, ddc)) +
  geom_bar(stat = "identity", width = 0.7, show.legend = FALSE,
           fill = "steelblue" ) +
  theme_light()+
  theme(legend.position = "none",
        panel.border = element_blank()) +
  labs(title = "Dias de Cultivo por Ciclo",
       subtitle = "Viveiro 01",
       caption = "Fonte: Azul Marinho Aquicultura",
       y = "Dias",
       x = "Ciclo") +
  theme(plot.caption = element_text(size = 8, color = "grey60")) +
  geom_text(aes(label = format(ddc,
                               big.mark = ".",
                               decimal.mark = ",")),
            vjust = 1.5,
            hjust = 0.55,
            color = "white",
            size = 3.5,
            angle = 0)  


# ddc média anual ----------------------------------------------------------


v1_ddc_ano <- biom %>%
  filter(viveiro == 1) %>%
  mutate(ano_desp = factor(year(data_desp)),
         ciclo = factor(ciclo)) %>%  
  group_by(ano_desp) %>% 
  summarize(ddc = round(mean(ddc, na.rm = TRUE),2))

v1_ddc_ano %>%
  ggplot(aes(ano_desp, ddc)) +
  geom_bar(stat = "identity", width = 0.5, show.legend = FALSE,
           fill = "steelblue" ) +
  theme_light()+
  theme(legend.position = "none",
        panel.border = element_blank()) +
  labs(title = "Média Anual Dias de Cultivo",
       subtitle = "Viveiro 01",
       caption = "Fonte: Azul Marinho Aquicultura",
       y = "Dias",
       x = "Ano") +
  theme(plot.caption = element_text(size = 8, color = "grey60")) +
  geom_text(aes(label = format(ddc,
                               big.mark = ".",
                               decimal.mark = ",")),
            vjust = 1.5,
            color = "white",
            size = 4.0,
            angle = 0)

# ddc total anual ----------------------------------------------------------

v1_ddc_ano <- biom %>%
  filter(viveiro == 1) %>%
  mutate(ano_desp = factor(year(data_desp)),
         ciclo = factor(ciclo)) %>%  
  group_by(ano_desp) %>% 
  summarize(ddc = round(sum(ddc, na.rm = TRUE),2))

v1_ddc_ano %>%
  ggplot(aes(ano_desp, ddc)) +
  geom_bar(stat = "identity", width = 0.5, show.legend = FALSE,
           fill = "steelblue" ) +
  theme_light()+
  theme(legend.position = "none",
        panel.border = element_blank()) +
  labs(title = "Total Anual Dias de Cultivo",
       subtitle = "Viveiro 01",
       caption = "Fonte: Azul Marinho Aquicultura",
       y = "Dias",
       x = "Ano") +
  theme(plot.caption = element_text(size = 8, color = "grey60")) +
  geom_text(aes(label = format(ddc,
                               big.mark = ".",
                               decimal.mark = ",")),
            vjust = 1.5,
            color = "white",
            size = 4.0,
            angle = 0)

