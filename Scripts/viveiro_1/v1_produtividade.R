library(tidyverse)
library(googlesheets4)
library(ggthemes)
library(lubridate)
library(ggrepel)
library(scales)
library(corrplot)


biom <- read_sheet("1KkLM7bz-Az-etHUeENou-BjX4mDUfJCccwcCIo0k0CU", 2)



# Produtividade por Ciclo de Cultivo --------------------------------------

biom %>%
  filter(viveiro == 1) %>%
  mutate(ano_desp = factor(year(data_desp)),
         ciclo = factor(ciclo)) %>%
  group_by(ciclo) %>%
  summarize(produtividade_media = round(mean(produtividade), 2)) %>%
  ggplot(aes(x = ciclo, y = produtividade_media)) +
  geom_bar(stat = "identity", width = 0.5, show.legend = FALSE,
           fill = "steelblue") +
  labs(title = "Produtividade Por Ciclo de Cultivo",
       subtitle = "Viveiro 01",
       y = "Produtividae (kg/ha)",
       x = "Ciclo",
       caption = "Fonte: Azul Marinho Aquicultura") +
  theme_light() +
  theme(plot.caption = element_text(size = 7, color = "grey60"),
        panel.border = element_blank()) +
  geom_text(aes(label = format(produtividade_media,
                               big.mark = ".",
                               decimal.mark = ",")),
            vjust = 0.45,
            hjust = 1.1,
            color = "white",
            size = 4.0,
            angle = 90)

# Produtividade por Ano ---------------------------------------------------

biom %>%
  filter(viveiro == 1) %>%
  mutate(ano_desp = factor(year(data_desp)),
         ciclo = factor(ciclo)) %>%
  group_by(ano_desp) %>%
  summarize(produtividade_media = round(mean(produtividade), 2)) %>%
  ggplot(aes(x = ano_desp, y = produtividade_media)) +
  geom_bar(stat = "identity", width = 0.5, show.legend = FALSE,
           fill = "steelblue") +
  labs(title = "Produtividade Média Anual",
       subtitle = "Viveiro 01",
       y = "Produtividae (kg/ha)",
       x = "Ano",
       caption = "Fonte: Azul Marinho Aquicultura") +
  theme_minimal() +
  theme(plot.caption = element_text(size = 7, color = "grey60")) +
  geom_text(aes(label = format(produtividade_media, big.mark = ".",
                               decimal.mark = ",")),
            vjust = 1.6,
            color = "white",
            size = 4.0,
            angle = 0)
