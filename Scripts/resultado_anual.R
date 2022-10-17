library(tidyverse)
library(googlesheets4)
library(ggthemes)
library(lubridate)
library(ggrepel)
library(scales)

biom <- read_sheet("1KkLM7bz-Az-etHUeENou-BjX4mDUfJCccwcCIo0k0CU", 2)


biom %>%
  mutate(ano_desp = factor(year(data_desp))) %>%
  group_by(ano_desp) %>%
  summarize(biom_real = round(sum(biom_real), 2)) %>%
  ggplot(aes(x = ano_desp, y = biom_real, fill = ano_desp)) +
  geom_bar(stat = "identity", width = 0.5, show.legend = FALSE) +
  labs(title = "Produção Anual (kg)",
       y = "Kg Produzidos",
       x = "Ano",
       caption = "Fonte: Azul Marinho Aquicultura") +
  scale_y_continuous(
    labels = scales::label_number(big.mark = ".",
                                  decimal.mark = ",")) +
  expand_limits(y = 65000) +
  theme_minimal() +
  theme(plot.caption = element_text(size = 9, color = "grey60"),
        axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.title.y = element_text(size = 20,
                                    margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(size = 20),
        plot.title = element_text(size = 25)) +
  geom_text(aes(label = format(biom_real, big.mark = ".",
                                        decimal.mark = ",")),
               vjust = 1.6, color = "white", size = 5)


# Trocando a a palete de cores (Viridis) test

biom %>%
  mutate(ano_desp = factor(year(data_desp))) %>%
  group_by(ano_desp) %>%
  summarize(biom_real = round(sum(biom_real), 2)) %>%
  ggplot(aes(x = ano_desp, y = biom_real, fill = ano_desp)) +
  geom_bar(stat = "identity", width = 0.5, show.legend = FALSE) +
  labs(title = "Produção Anual (kg)",
       y = "Kg Produzidos",
       x = "Ano",
       caption = "Fonte: Azul Marinho Aquicultura") +
  scale_y_continuous(
    labels = scales::label_number(big.mark = ".",
                                  decimal.mark = ",")) +
  expand_limits(y = 65000) +
  theme_minimal() +
  theme(plot.caption = element_text(size = 9, color = "grey60"),
        axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.title.y = element_text(size = 20,
                                    margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(size = 20),
        plot.title = element_text(size = 25)) +
  geom_text(aes(label = format(biom_real, big.mark = ".", decimal.mark = ",")),
   vjust = -0.5, color = "red", size = 4.0) +
  scale_fill_viridis_d(option = "viridis")


# Produção de um único ano ------------------------------------------------


ano_2022 <- biom %>%
  mutate(ano_desp = factor(year(data_desp))) %>% 
  filter(ano_desp == "2022") %>% 
  summarize((total_kg = sum(biom_real, na.rm = TRUE)))




# Produção total por viveiro------------------------------

biom %>% 
  group_by(viveiro) %>% 
  summarise(cultivos = n(), 
            total_kg = sum(biom_real),
            kg_ha = mean(produtividade),
            sovrevive = mean(sobrevive),
            ddc = sum(ddc),
            dias_parados = sum(fallow, na.rm = TRUE),
            pct_parado = sum(fallow/ddc*100, na.rm = TRUE),
            fcr = mean(tca))
