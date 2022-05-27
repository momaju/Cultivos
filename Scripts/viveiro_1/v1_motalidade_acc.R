library(tidyverse)
library(googlesheets4)
library(ggthemes)
library(lubridate)
library(ggrepel)
library(scales)
library(corrplot)


biom <- read_sheet("1KkLM7bz-Az-etHUeENou-BjX4mDUfJCccwcCIo0k0CU", 2)


# Mortalidade por Ciclo de Cultivo -----------------------------------------------

v1_mortalidade_ac <- biom %>%
  filter(viveiro == 1) %>%
  mutate(ano_desp = factor(year(data_desp)),
         ciclo = factor(ciclo)) 


v1_mortalidade_ac %>%
  ggplot(aes(ciclo, baixas_ac)) +
  geom_bar(stat = "identity", width = 0.5, show.legend = FALSE,
           fill = "steelblue" ) +
  theme_light()+
  theme(legend.position = "none",
        panel.border = element_blank()) +
  labs(title = "Motalidade por Ciclo de Cultivo",
       subtitle = "Viveiro 01",
       caption = "Fonte: Azul Marinho Aquicultura",
       y = "Total de Camarões Moetos (n)",
       x = "Ciclo de Cultivo") +
  theme(plot.caption = element_text(size = 8, color = "grey60")) +
  geom_text(aes(label = format(baixas_ac,
                               big.mark = ".",
                               decimal.mark = ",")),
            vjust = -0.5,
            color = "black",
            size = 3.0,
            angle = 0)


# Mortalidade Anual ----------------------------------------------------------


v1_mortalidade_ac <- biom %>%
  filter(viveiro == 1) %>%
  mutate(ano_desp = factor(year(data_desp)),
         ciclo = factor(ciclo)) %>% 
  group_by(ano_desp) %>% 
  summarize(baixas_ac = sum(baixas_ac))


v1_mortalidade_ac %>%
  ggplot(aes(ano_desp, baixas_ac)) +
  geom_bar(stat = "identity", width = 0.5, show.legend = FALSE,
           fill = "steelblue" ) +
  theme_light()+
  theme(legend.position = "none",
        panel.border = element_blank()) +
  labs(title = "Mortalidade Total Anual",
       subtitle = "Viveiro 01",
       caption = "Fonte: Azul Marinho Aquicultura",
       y = "Total de Camarões Moetos (n)",
       x = "Ano") +
  theme(plot.caption = element_text(size = 8, color = "grey60")) +
  geom_text(aes(label = format(baixas_ac,
                               big.mark = ".",
                               decimal.mark = ",")),
            vjust = 1.5,
            color = "white",
            size = 4.0,
            angle = 0)




