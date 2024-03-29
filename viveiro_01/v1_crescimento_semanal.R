library(tidyverse)
library(googlesheets4)
library(ggthemes)
library(lubridate)
library(ggrepel)
library(scales)
library(corrplot)


biom <- read_sheet("1KkLM7bz-Az-etHUeENou-BjX4mDUfJCccwcCIo0k0CU", 2)

v1_crescimento <- biom %>%
  filter(viveiro == 1) %>%
  select(ciclo, crescimento = g_semana) %>%
  mutate(ciclo = factor(ciclo)) %>%
  group_by(ciclo)
  
         

v1_crescimento %>%
  ggplot(aes(ciclo, crescimento)) +
  geom_line(aes(group = 1), color = "steelblue") +
  geom_point(shape = 1, size = 3, color = "steelblue") +
  theme_light() +
  labs(title = "Crescimento Médio Semanal ",
       subtitle = "Viveiro 01",
       caption = "Fonte: Azul Marinho Aquicultura",
       y = "Crescimento (g/semana)",
       x = "Ciclo de Cultivo") +
  theme(legend.position = "none",
        panel.border = element_blank(),
        plot.caption = element_text(size = 8, color = "grey60"))
