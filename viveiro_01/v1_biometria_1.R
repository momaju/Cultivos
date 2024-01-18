library(tidyverse)
library(googlesheets4)
library(ggthemes)
library(lubridate)
library(ggrepel)
library(scales)
library(corrplot)


biom <- read_sheet("1KkLM7bz-Az-etHUeENou-BjX4mDUfJCccwcCIo0k0CU", 2)

# primeira biometria por ciclo de cultivo -----------------------------------------------


v1_b1_ciclo <- biom %>%
  filter(viveiro == 1) %>%
  mutate(ano_desp = factor(year(data_desp)),
         ciclo = factor(ciclo))

v1_b1_ciclo %>%
  ggplot(aes(ciclo, biometria_1)) +
  geom_bar(stat = "identity", width = 0.7, show.legend = FALSE,
           fill = "steelblue" ) +
  theme_light()+
  theme(legend.position = "none",
        panel.border = element_blank()) +
  labs(title = "Pdeso na Primeira Biometria (g)",
       subtitle = "Viveiro 01",
       caption = "Fonte: Azul Marinho Aquicultura",
       y = "Peso (g)",
       x = "Ciclo") +
  theme(plot.caption = element_text(size = 8, color = "grey60")) +
  geom_text(aes(label = format(biometria_1,
                               big.mark = ".",
                               decimal.mark = ",")),
            vjust = 1.5,
            hjust = 0.55,
            color = "white",
            size = 3.5,
            angle = 0)  

# peso médio anual primeira biometria -------------------------------------


v1_b1_ano <- biom %>%
  filter(viveiro == 1) %>%
  mutate(ano_desp = factor(year(data_desp)),
         ciclo = factor(ciclo)) %>%  
  group_by(ano_desp) %>% 
  summarize(biometria_1 = round(mean(tca, na.rm = TRUE),2))

v1_b1_ano %>%
  ggplot(aes(ano_desp, biometria_1)) +
  geom_bar(stat = "identity", width = 0.5, show.legend = FALSE,
           fill = "steelblue" ) +
  theme_light()+
  theme(legend.position = "none",
        panel.border = element_blank()) +
  labs(title = "Peso Médio na Primeira Biometria (g)",
       subtitle = "Viveiro 01",
       caption = "Fonte: Azul Marinho Aquicultura",
       y = "Peso (g)",
       x = "Ano") +
  theme(plot.caption = element_text(size = 8, color = "grey60")) +
  geom_text(aes(label = format(biometria_1,
                               big.mark = ".",
                               decimal.mark = ",")),
            vjust = 1.5,
            color = "white",
            size = 4.0,
            angle = 0)

# dias até a  primeira biometria por ciclo ------------------------------------


v1_db1_ciclo <- biom %>%
  filter(viveiro == 1) %>%
  mutate(ano_desp = factor(year(data_desp)),
         ciclo = factor(ciclo))

v1_db1_ciclo %>%
  ggplot(aes(ciclo, dbiometria_1)) +
  geom_bar(stat = "identity", width = 0.7, show.legend = FALSE,
           fill = "steelblue" ) +
  theme_light()+
  theme(legend.position = "none",
        panel.border = element_blank()) +
  labs(title = "Dias Até a Primeira Biometria",
       subtitle = "Viveiro 01",
       caption = "Fonte: Azul Marinho Aquicultura",
       y = "Dias",
       x = "Ciclo") +
  theme(plot.caption = element_text(size = 8, color = "grey60")) +
  geom_text(aes(label = format(dbiometria_1,
                               big.mark = ".",
                               decimal.mark = ",")),
            vjust = 1.5,
            hjust = 0.55,
            color = "white",
            size = 3.5,
            angle = 0)  

# média dias até primeira biometria por ano -------------------------------------


v1_db1_ano <- biom %>%
  filter(viveiro == 1) %>%
  mutate(ano_desp = factor(year(data_desp)),
         ciclo = factor(ciclo)) %>%  
  group_by(ano_desp) %>% 
  summarize(dbiometria_1 = round(mean(dbiometria_1, na.rm = TRUE),2))

v1_db1_ano %>%
  ggplot(aes(ano_desp, dbiometria_1)) +
  geom_bar(stat = "identity", width = 0.5, show.legend = FALSE,
           fill = "steelblue" ) +
  theme_light()+
  theme(legend.position = "none",
        panel.border = element_blank()) +
  labs(title = "Média de Dias Até a Primeira Biometria",
       subtitle = "Viveiro 01",
       caption = "Fonte: Azul Marinho Aquicultura",
       y = "Dias",
       x = "Ano") +
  theme(plot.caption = element_text(size = 8, color = "grey60")) +
  geom_text(aes(label = format(dbiometria_1,
                               big.mark = ".",
                               decimal.mark = ",")),
            vjust = 1.5,
            color = "white",
            size = 4.0,
            angle = 0)

