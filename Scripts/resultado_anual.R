library(tidyverse)
library(googlesheets4)
library(ggthemes)
library(lubridate)
library(ggrepel)
library(scales)
library(magick)

biom <- read_sheet("1KkLM7bz-Az-etHUeENou-BjX4mDUfJCccwcCIo0k0CU", 2)


biom_ano <- biom %>%
  mutate(ano_desp = year(data_desp)) %>% 
  group_by(ano_desp) %>%
  summarize(biom_real = round(sum(biom_real), 2))

biom_ano %>% 
  ggplot(aes(x = ano_desp, y = biom_real)) +
  geom_bar(stat = "identity", width = 0.8, 
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
  scale_x_continuous(breaks = biom_ano$ano_desp) +
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


# Inserindo o Logo --------------------------------------------------------

image_url <- "https://drive.google.com/uc?id=1SN4gu5VzJYlfacpgoVycXNI8JRuswynA"
logo <- image_read(image_url)
grid::grid.raster(logo, 
                  x = 0.9, 
                  y = 0.8, 
                  just = c('left', 'bottom'), 
                  width = unit(1.3, 'inches'))

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
                                    margin = margin(t = 0, 
                                                    r = 20, 
                                                    b = 0, 
                                                    l = 0)),
        axis.title.x = element_text(size = 20, color = "#000080"),
        plot.title = element_text(size = 34, color = "#000080"),
        plot.subtitle = element_text(size = 17, color = "#000080"),
        axis.line.y = element_line(color = "#264DFF"),
        axis.line.x = element_line(color = "#264DFF")) +
  geom_text(aes(label = format(biom_real, big.mark = ".", decimal.mark = ",")),
   vjust = -0.5, color = "#264DFF", size = 4.0) +
  scale_fill_viridis_d(option = "viridis")


# Produção de um único ano ------------------------------------------------


ano_2023 <- biom %>%
  mutate(ano_desp = factor(year(data_desp))) %>% 
  filter(ano_desp != 2024) %>% 
  mutate(mes_desp = factor(month(data_desp,
                                 label = TRUE, 
                                 abbr = TRUE))) %>% 
  group_by(ano_desp) %>%
  summarize(biom_real = round(mean(biom_real, na.rm = TRUE), 2))
            
ano_2023

View(ano_2023)




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
# carregar: library(dataxray)     



# Despescas Mensais -------------------------------------------------------
biom_mes <- biom %>% 
  mutate(ano = year(data_desp), 
                mes = month(data_desp, label = TRUE, abbr = TRUE)) %>% 
  select(ano, viveiro, mes, biom_real) %>% 
  group_by(mes) %>% 
  summarise(mes = unique(mes), mean_kg = round(mean(biom_real),2), total_kg = sum(biom_real)) 
  
biom_mes %>% ggplot(aes(x = mes, y = mean_kg,)) +
  geom_bar(stat = "identity", width = 0.8, 
           show.legend = FALSE,
           fill = "#2e98fe") +
  labs(title = "Produção Média Mensal",
       subtitle = "De 2015 a 2023",
       y = "Kg Produzidos",
       x = "",
       caption = "Azul Marinho Aquicultura") +
  scale_y_continuous(
    limits = (c(0, 4000)),
    breaks = (seq(0, 4000, 500)),
    labels = scales::label_number(big.mark = ".",
                                  decimal.mark = ","),
    expand = expansion(0)) + #faz as barras encostarem no eixo
    #expand_limits(y = 4000) +
  geom_hline(yintercept = mean(biom_mes$mean_kg), 
             color = "#1a0080", 
             linetype = "solid",
             linewidth = 0.8,
             alpha = 0.2) +
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
        panel.grid.major = element_blank(),
        plot.margin = margin(25,25,25,30)) +
  annotate("curve", x = 3, y = 3500, xend = 2, yend = 2651,
           curvature = 0.3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", 
           x = 3.1, y = 3500, 
           label = "média mensal", 
           hjust = "left",
           color = "red") +
  geom_text(aes(label = format(mean_kg, 
                               big.mark = ".", 
                               decimal.mark = ",")),
            vjust = -0.5, 
            color = "#000080", 
            size = 4.0)

# Inserindo o Logo --------------------------------------------------------

image_url <- "https://drive.google.com/uc?id=1SN4gu5VzJYlfacpgoVycXNI8JRuswynA"
logo <- image_read(image_url)
grid::grid.raster(logo, 
                  x = 0.9, 
                  y = 0.8, 
                  just = c('left', 'bottom'), 
                  width = unit(1.3, 'inches'))

biom_mes


# Produção 2023 -----------------------------------------------------------

biom_2023 <- biom %>%
  mutate(ano_desp = factor(year(data_desp))) %>% 
  filter(ano_desp == "2023") %>%
  mutate(mes_desp = month(data_desp,
                          label = TRUE, 
                          abbr = TRUE)) %>% 
  group_by(mes_desp) %>%
  summarize(biom_real = round(sum(biom_real, na.rm = TRUE), 2))

biom_2023

biom_2023 %>% ggplot(aes(x = mes_desp, y = biom_real)) +
  geom_bar(stat = "identity", width = 0.8, 
           show.legend = FALSE,
           fill = "#2e98fe") +
  labs(title = "2023 - Produção Mensal",
       #subtitle = "2023",
       y = "Kg Produzidos",
       x = "",
       caption = "Azul Marinho Aquicultura") +
  scale_y_continuous(
    limits = (c(0, 5000)),
    breaks = (seq(0, 5000, 500)),
    labels = scales::label_number(big.mark = ".",
                                  decimal.mark = ","),
    expand = expansion(0)) + #faz as barras encostarem no eixo
  #expand_limits(y = 4000) +
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
        panel.grid.major = element_blank(),
        plot.margin = margin(25,25,25,30)) +
  geom_text(aes(label = format(biom_real, 
                               big.mark = ".", 
                               decimal.mark = ",")),
            vjust = -0.5, color = "#000080", size = 4.0)


# Inserindo o Logo --------------------------------------------------------

image_url <- "https://drive.google.com/uc?id=1SN4gu5VzJYlfacpgoVycXNI8JRuswynA"
logo <- image_read(image_url)
grid::grid.raster(logo, 
                  x = 0.9, 
                  y = 0.8, 
                  just = c('left', 'bottom'), 
                  width = unit(1.3, 'inches'))







