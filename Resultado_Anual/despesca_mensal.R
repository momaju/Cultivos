
library(tidyverse)
library(googlesheets4)
library(ggthemes)
library(lubridate)
library(ggrepel)
library(scales)
library(magick)

biom <- read_sheet("1KkLM7bz-Az-etHUeENou-BjX4mDUfJCccwcCIo0k0CU", 2)

# Despescas Mensais -------------------------------------------------------
biom_mes <- biom %>% 
  mutate(ano = year(data_desp), 
         mes = month(data_desp, label = TRUE, abbr = TRUE)) %>% 
  select(ano, viveiro, mes, biom_real) %>% 
  group_by(mes) %>% 
  summarise(mes = unique(mes), 
            mean_kg = round(mean(biom_real),2), 
            total_kg = sum(biom_real)) 

biom_mes %>% ggplot(aes(x = mes, y = mean_kg,)) +
  geom_bar(stat = "identity", width = 0.8, 
           show.legend = FALSE,
           fill = "#2e98fe") +
  labs(title = "Produção Média Mensal",
       #subtitle = "De 2015 a 2023",
       y = "Kg Produzidos",
       x = "",
       caption = "Azul Marinho Aquicultura") +
  scale_y_continuous(
    limits = (c(0, 4000)),
    breaks = (seq(0, 4000, 1000)),
    labels = scales::label_number(big.mark = ".",
                                  decimal.mark = ","),
    expand = expansion(0)) + #faz as barras encostarem no eixo
  #expand_limits(y = 4000) +
  
  # Linha média mensal ------------------------------------------------------
  
  geom_hline(yintercept = mean(biom_mes$mean_kg), 
             color = "#1a0080", 
             linetype = "solid",
             linewidth = 0.8,
             alpha = 0.2) +
  annotate("curve", 
           x = 4.5,
           y = 3000, 
           xend = 3.8, 
           yend = 2600,
           curvature = 0.3, 
           arrow = arrow(length = unit(2, "mm"))) +
  
  annotate(geom = "text", 
           x = 4.50, 
           y = 3000, 
           label = paste0("média mensal = \n              ",
                          format(round(mean(biom_mes$mean_kg, 
                                            na.rm = TRUE),0), 
                                 big.mark = ".")),
           hjust = "left",
           color = "red") +
  
  # Tema --------------------------------------------------------------------
  
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
mean(biom_mes$mean_kg)
