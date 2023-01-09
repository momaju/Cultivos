library(tidyverse)
library(googlesheets4)
library(ggthemes)
library(lubridate)
library(ggrepel)
library(scales)

biom <- read_sheet("1KkLM7bz-Az-etHUeENou-BjX4mDUfJCccwcCIo0k0CU", 2)


biom %>%
  mutate(ano_desp = year(data_desp)) %>% 
  group_by(ano_desp) %>%
  summarize(biom_real = round(sum(biom_real), 2)) %>%
  ggplot(aes(x = ano_desp, y = biom_real,)) +
  geom_bar(stat = "identity", width = 0.5, 
           show.legend = FALSE,
           fill = "#2e98fe") +
  labs(title = "Produção Anual",
       subtitle = "Azul Marinho Aquicultura",
       y = "Kg Produzidos",
       x = "Ano",
       caption = "Azul Marinho Aquicultura") +
  scale_y_continuous(
    labels = scales::label_number(big.mark = ".",
                                  decimal.mark = ","),
    expand = expansion(0),) + #faz as barras encostarem no eixo
  expand_limits(y = 65000) +
  theme_minimal() +
  theme(plot.caption = element_text(size = 9, color = "#8080c0"),
        axis.text.y = element_text(size = 15, color = "#000080"),
        axis.text.x = element_text(size = 15, color = "#000080"),
        axis.title.y = element_text(size = 20,
                                    color = "#000080",
                                    margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(size = 20, color = "#000080"),
        plot.title = element_text(size = 25, color = "#000080"),
        plot.subtitle = element_text(size = 12, color = "#000080"),
        axis.line.y = element_line(color = "#000080"),
        axis.line.x = element_line(color = "#000080"),
        panel.grid.major = element_blank()) +
  geom_text(aes(label = format(biom_real, big.mark = ".", decimal.mark = ",")),
            vjust = -0.5, color = "#000080", size = 4.0) 

# Trocando a a palete de cores (Viridis) test

biom %>%
  mutate(ano_desp = factor(year(data_desp))) %>%
  group_by(ano_desp) %>%
  summarize(biom_real = round(sum(biom_real), 2)) %>%
  ggplot(aes(x = ano_desp, y = biom_real, fill = ano_desp)) +
  geom_bar(stat = "identity", width = 0.5, show.legend = FALSE) +
  labs(title = "Produção Anual",
       subtitle = "Azul Marinho Aquicultura",
       y = "Kg Produzidos",
       x = "Ano",
       caption = "Fonte: Azul Marinho Aquicultura") +
  scale_y_continuous(
    labels = scales::label_number(big.mark = ".",
                                  decimal.mark = ",")) +
  expand_limits(y = 65000) +
  theme_minimal() +
  theme(plot.caption = element_text(size = 9, color = "#3FA0FF"),
        axis.text.y = element_text(size = 15, color = "#000080"),
        axis.text.x = element_text(size = 15, color = "#000080"),
        axis.title.y = element_text(size = 20,
                                    color = "#000080",
                                    margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(size = 20, color = "#000080"),
        plot.title = element_text(size = 34, color = "#000080"),
        plot.subtitle = element_text(size = 17, color = "#000080"),
        axis.line.y = element_line(color = "#264DFF"),
        axis.line.x = element_line(color = "#264DFF")) +
  geom_text(aes(label = format(biom_real, big.mark = ".", decimal.mark = ",")),
   vjust = -0.5, color = "#264DFF", size = 4.0) +
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


# Produção por vieiro por ciclo e por ano específico ---------------------------------

kg_ano <- biom %>% 
  group_by(viveiro, ciclo) %>%
  mutate(ano_desp = year(data_desp)) %>% 
  filter(ano_desp == "2021") %>% 
  summarise( ano = ano_desp,total_kg = sum(biom_real), .groups = "drop") %>% 
  # .groups = "drop" elimina a soma cumulativa por grupo e soma todo o ano,
  # caso contrário acumula por grupo de viveiro.
  mutate(acumulado_kg = cumsum(total_kg)) %>%
  select(ano, everything())
kg_ano


# Using dataxray package --------------------------------------------------

biom %>% 
  make_xray() %>% 
  view_xray()
# por alguma razão não funcionou            