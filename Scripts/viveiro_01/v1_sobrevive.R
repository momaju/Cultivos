library(tidyverse)
library(googlesheets4)
library(ggthemes)
library(lubridate)
library(ggrepel)
library(scales)
library(corrplot)


biom <- read_sheet("1KkLM7bz-Az-etHUeENou-BjX4mDUfJCccwcCIo0k0CU", 2)

# Sobrevivência por Ciclo de Cultivo -----------------------------------------------

v1_sobrevive <- biom %>%
  filter(viveiro == 1) %>%
  mutate(ano_desp = factor(year(data_desp)),
         ciclo = factor(ciclo)) %>% 
  group_by(ciclo) %>% 
  summarize(sobrevive = sum(sobrevive))

v1_sobrevive %>%
  ggplot(aes(ciclo, sobrevive)) +
  geom_bar(stat = "identity", width = 0.5, show.legend = FALSE,
           fill = "steelblue" ) +
  theme_light()+
  theme(legend.position = "none",
        panel.border = element_blank()) +
  labs(title = "Sobrevivência por Ciclo de Cultivo",
       subtitle = "Viveiro 01",
       caption = "Fonte: Azul Marinho Aquicultura",
       y = "Sobrevivência (%)",
       x = "Ciclo") +
  theme(plot.caption = element_text(size = 8, color = "grey60")) +
  geom_text(aes(label = format(sobrevive,
                               big.mark = ".",
                               decimal.mark = ",")),
            vjust = 0.5,
            hjust = 1.1,
            color = "white",
            size = 4.0,
            angle = 90)


# Sobrevivência Anual ----------------------------------------------------------



v1_sobrevive_ano <- biom %>%
  filter(viveiro == 1) %>%
  mutate(ano_desp = factor(year(data_desp)),
         ciclo = factor(ciclo)) %>% 
  group_by(ano_desp) %>% 
  summarize(sobrevive = round(mean(sobrevive, na.rm = TRUE),2))

v1_sobrevive_ano %>%
  ggplot(aes(ano_desp, sobrevive)) +
  geom_bar(stat = "identity", width = 0.5, show.legend = FALSE,
           fill = "steelblue" ) +
  theme_light()+
  theme(legend.position = "none",
        panel.border = element_blank()) +
  labs(title = "Sobrevivência Média Anual",
       subtitle = "Viveiro 01",
       caption = "Fonte: Azul Marinho Aquicultura",
       y = "Sobrevivência (%)",
       x = "Ano") +
  theme(plot.caption = element_text(size = 8, color = "grey60")) +
  geom_text(aes(label = format(sobrevive,
                               big.mark = ".",
                               decimal.mark = ",")),
            vjust = 1.5,
            color = "white",
            size = 4.0,
            angle = 0)
