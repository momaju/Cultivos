
# Produção Mensal Anual ---------------------------------------------------



# Libraries and Data ------------------------------------------------------

library(tidyverse)
library(googlesheets4)
library(ggthemes)
library(lubridate)
library(ggrepel)
library(scales)
library(magick)

biom <- read_sheet("1KkLM7bz-Az-etHUeENou-BjX4mDUfJCccwcCIo0k0CU", 2)


biom_2023 <- biom %>%
  mutate(ano_desp = factor(year(data_desp))) %>% 
  filter(ano_desp == "2023") %>%
  mutate(mes_desp = factor(month(data_desp,
                          label = TRUE, 
                          abbr = TRUE))) %>% 
  group_by(mes_desp) %>%
  summarize(biom_real = round(sum(biom_real), 2))

biom_2023

biom_2023 %>% ggplot(aes(x = mes_desp, y = biom_real)) +
  geom_bar(stat = "identity", 
           width = 0.5, 
           show.legend = FALSE,
           fill = "#2e98fe") +
  labs(title = "2023 - Produção Mensal",
       #subtitle = "2023",
       y = "Kg Produzidos",
       x = "",
       caption = "Azul Marinho Aquicultura") +
  scale_y_continuous(
    limits = (c(0, 6500)),
    breaks = (seq(0, 6500, 500)),
    labels = scales::label_number(big.mark = ".",
                                  decimal.mark = ","),
    expand = expansion(0)) + #faz as barras encostarem no eixo
  scale_x_discrete(labels = biom_2023$mes_desp, 
                   breaks = biom_2023$mes_desp) +
 

# Linha média mensal ------------------------------------------------------

geom_hline(yintercept = mean(biom_2023$biom_real), 
           color = "#1a0080", 
           linetype = "solid",
           linewidth = 0.8,
           alpha = 0.2) +  
  
annotate("curve", 
         x = 3,
         y = 4700, 
         xend = 2, 
         yend = 3796,
         curvature = 0.5, 
         arrow = arrow(length = unit(2, "mm"))) +
  
  annotate(geom = "text", 
           x = 3, y = 4720, 
           label = "média mensal", 
           hjust = "left",
           color = "red") +
  

# Tema --------------------------------------------------------------------

  theme_minimal() +
  theme(plot.caption = element_text(size = 9, 
                                    color = "#8080c0"),
        axis.text.y = element_text(size = 15, 
                                   color = "#000080"),
        axis.text.x = element_text(size = 15, 
                                   color = "#000080"),
        axis.title.y = element_text(size = 20,
                                    color = "#000080",
                                    margin = margin(t = 0, 
                                                    r = 20, 
                                                    b = 0, 
                                                    l = 0)),
        axis.title.x = element_text(size = 20, 
                                    color = "#000080"),
        plot.title = element_text(size = 25, 
                                  color = "#000080"),
        plot.subtitle = element_text(size = 12, 
                                     color = "#000080"),
        axis.line.y = element_line(color = "#000080"),
        axis.line.x = element_line(color = "#000080"),
        panel.grid.major = element_blank(),
        plot.margin = margin(25,25,25,30)) +
  geom_text(aes(label = format(biom_real, 
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


# # Tentativa de colocar todos os meses no eixo-x -------------------------
# 


biom_2023 <- biom %>%
  mutate(ano_desp = factor(year(data_desp))) %>% 
  filter(ano_desp == "2023") %>%
  mutate(mes_desp = factor(month(data_desp,
                                 label = TRUE,
                                 abbr = TRUE))) %>% 
  group_by(mes_desp) %>%
  summarize(biom_real = round(sum(biom_real), 2))

biom_2023_todo <- biom_2023 %>% 
  add_row(mes_desp = "Fev", 
          biom_real = 0, 
          .before = 2)
biom_2023_todo <- biom_2023_todo %>% 
  add_row(mes_desp = "Jul", 
          biom_real = 0, 
          .before = 7)

biom_2023_todo

# Set the levels of the factor variable, since you already have it 
# ordered as you want, you can simply use forcats::fct_inorder()

biom_2023_todo %>% ggplot(aes(x = fct_inorder(mes_desp), y = biom_real)) +
  geom_bar(stat = "identity", 
           width = 0.5, 
           show.legend = FALSE,
           fill = "#2e98fe") +
  labs(title = "2023 - Produção Mensal",
       #subtitle = "2023",
       y = "Kg Produzidos",
       x = "",
       caption = "Azul Marinho Aquicultura") +
  scale_y_continuous(
    limits = (c(0, 6500)),
    breaks = (seq(0, 6500, 500)),
    labels = scales::label_number(big.mark = ".",
                                  decimal.mark = ","),
    expand = expansion(0)) + #faz as barras encostarem no eixo
  
  
  # Linha média mensal ------------------------------------------------------

geom_hline(yintercept = mean(biom_2023_todo$biom_real), 
           color = "#1a0080", 
           linetype = "solid",
           linewidth = 0.8,
           alpha = 0.2) +  
  
  annotate("curve", 
           x = 3.5,
           y = 5000, 
           xend = 1.7, 
           yend = 3200,
           curvature = 0.3, 
           arrow = arrow(length = unit(2, "mm"))) +
  
  annotate(geom = "text", 
           x = 3.5, 
           y = 5050, 
           label = "média mensal", 
           hjust = "left",
           color = "red") +
  
  
  # Tema --------------------------------------------------------------------

theme_minimal() +
  theme(plot.caption = element_text(size = 9, 
                                    color = "#8080c0"),
        axis.text.y = element_text(size = 15, 
                                   color = "#000080"),
        axis.text.x = element_text(size = 15, 
                                   color = "#000080"),
        axis.title.y = element_text(size = 20,
                                    color = "#000080",
                                    margin = margin(t = 0, 
                                                    r = 20, 
                                                    b = 0, 
                                                    l = 0)),
        axis.title.x = element_text(size = 20, 
                                    color = "#000080"),
        plot.title = element_text(size = 25, 
                                  color = "#000080"),
        plot.subtitle = element_text(size = 12, 
                                     color = "#000080"),
        axis.line.y = element_line(color = "#000080"),
        axis.line.x = element_line(color = "#000080"),
        panel.grid.major = element_blank(),
        plot.margin = margin(25,25,25,30)) +
  geom_text(aes(label = format(biom_real, 
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

