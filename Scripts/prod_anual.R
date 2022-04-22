
library(tidyverse)
library(googlesheets4)
library(ggthemes)
library(lubridate)
library(ggrepel)
library(scales)

Biom <- read_sheet("1KkLM7bz-Az-etHUeENou-BjX4mDUfJCccwcCIo0k0CU", 2)


Biom %>% 
  mutate(ano_desp = factor(year(data_desp))) %>% #extrai o ano da data de despesca
  group_by(ano_desp) %>%
  summarize(biom_real = round(sum(biom_real),2)) %>% 
  ggplot(aes(x=ano_desp, y=biom_real, fill = ano_desp)) +
  geom_bar(stat="identity", width = 0.5,show.legend = FALSE) +
  scale_y_continuous(breaks = seq(0, 65000, by = 10000),
                     labels = label_number(big.mark = ".",
                                           decimal.mark = ","))+
  scale_fill_brewer(palette = "Dark2")+
  labs(title = "Produção Anual (kg)",
       y = "Kg Produzidos",
       x = "Ano",
       caption = "Fonte: Azul Marinho Aquicultura")+
  theme_minimal() +
  theme(plot.caption = element_text(size = 9, color = "grey60")) +
  geom_text(aes(label = format(biom_real, big.mark = ".", decimal.mark = ",")), vjust=1.6, color="white", size=4.0)


# Trocando a a palete de cores (Viridis) test

Biom %>% 
  mutate(ano_desp = factor(year(data_desp))) %>% #extrai o ano da data de despesca
  group_by(ano_desp) %>%
  summarize(biom_real = round(sum(biom_real),2)) %>% 
  ggplot(aes(x=ano_desp, y=biom_real, fill = ano_desp)) +
  geom_bar(stat="identity", width = 0.5,show.legend = FALSE) +
  labs(title = "Produção Anual (kg)",
       y = "Kg Produzidos",
       x = "Ano",
       caption = "Fonte: Azul Marinho Aquicultura")+
  expand_limits(y = 60000) +
  theme_minimal() +
  theme(plot.caption = element_text(size = 9, color = "grey60")) +
  geom_text(aes(label = format(biom_real, big.mark = ".", decimal.mark = ",")), vjust= -0.5, color="red", size=4.0) +
  scale_fill_viridis_d(option = "viridis")

