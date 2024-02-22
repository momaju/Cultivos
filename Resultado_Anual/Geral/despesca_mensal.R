
# Despescas Mensais -------------------------------------------------------

# Loading Libraries -------------------------------------------------------

library(tidyverse)
library(googlesheets4)
library(ggthemes)
library(lubridate)
library(ggrepel)
library(scales)
library(magick)


# Reading the data --------------------------------------------------------

biom <- read_sheet("1KkLM7bz-Az-etHUeENou-BjX4mDUfJCccwcCIo0k0CU", 2)

biom_mes <- biom %>% 
  mutate(ano_desp = (year(data_desp)), 
         mes = month(data_desp, 
                     label = TRUE, 
                     abbr = TRUE)) %>%
  #filter(ano_desp != 2024) %>%
  select(ano_desp, viveiro, mes, biom_real) %>% 
  group_by(mes) %>% 
  summarise(mes = unique(mes), 
            mean_kg = round(mean(biom_real),2), 
            total_kg = sum(biom_real),
            max_ano = max(ano_desp))

biom_mes %>% ggplot(aes(x = mes, y = mean_kg,)) +
  geom_bar(stat = "identity", width = 0.8, 
           show.legend = FALSE,
           fill = "#2e98fe") +
  labs(title = "Produção Média Mensal",
       subtitle = paste0("2015 a ", biom_mes$max_ano),
       y = "Kg Produzidos",
       x = "",
       caption = "Azul Marinho Aquicultura") +
  scale_y_continuous(
    limits = (c(0, 4000)),
    breaks = (seq(0, 4000, 500)),
    labels = scales::label_number(big.mark = ".",
                                  decimal.mark = ","),
    expand = expansion(0)) + #faz as barras encostarem no eixo
  geom_hline(yintercept = mean(biom_mes$mean_kg), 
             color = "#1a0080", 
             linetype = "solid",
             linewidth = 0.8,
             alpha = 0.2) +
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
        axis.title.x = element_text(size = 15, 
                                    color = "#000080"),
        plot.title = element_text(size = 20, 
                                  color = "#000080"),
        plot.subtitle = element_text(size = 15, 
                                     color = "#000080"),
        #axis.line.y = element_line(color = "#000080"),
        #axis.line.x = element_line(color = "#000080"),
        panel.grid.major = element_blank(),
        plot.margin = margin(25,25,25,30)) +
  annotate("curve", 
           x = 3, 
           y = 3500, 
           xend = 2, 
           yend = 2651,
           curvature = 0.3, 
           arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", 
           x = 3.1, y = 3500, 
           label = paste0("média mensal = ", 
                          format(round(mean(biom_mes$mean_kg, 
                                            na.rm = TRUE),0), 
                          big.mark = ".")), 
           hjust = "left",
           color = "red")


# Inserindo o logo

image_url <- "https://drive.google.com/uc?id=1SN4gu5VzJYlfacpgoVycXNI8JRuswynA"
logo <- image_read(image_url)
#logo <- image_read("G://My Drive//RWork//Projects//Azul Marinho//Cultivos//Images//azul_logo_transp.png")
grid::grid.raster(logo,
                  x = 0.9, 
                  y = 0.8, 
                  just = c('left', 'bottom'), 
                  width = unit(1.3, 'inches'))

