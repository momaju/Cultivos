
library(tidyverse)
library(googlesheets4)
library(ggthemes)
library(lubridate)
library(ggrepel)
library(scales)
library(corrplot)
library(magick)


biom <- read_sheet("1KkLM7bz-Az-etHUeENou-BjX4mDUfJCccwcCIo0k0CU", 2)

biom_2023_mensal <- biom %>%
  mutate(ano_desp = factor(year(data_desp))) %>% 
  filter(ano_desp == "2023") %>%
  mutate(mes_desp = factor(month(data_desp, # factor to sort months
                                 label = TRUE,
                                 abbr = TRUE))) %>% 
  group_by(mes_desp) %>%
  summarize(densidade_media = round(mean(densidade), 2))

biom_2023_mensal <-  biom_2023_mensal %>% 
  add_row(mes_desp = "Fev", 
          #biom_real = 0, # gives NA instead of zero
          .before = 2) %>% 
  add_row(mes_desp = "Jul", 
          #biom_real = 0, # gives NA instead of zero
          .before = 7)


biom_2023_mensal %>% ggplot(aes(x = fct_inorder(mes_desp), 
                                y = densidade_media)) +
  geom_bar(stat = "identity", 
           width = 0.5, 
           show.legend = FALSE,
           fill = "#2e98fe") +
  labs(title = "2023 - Densidade no Povoamento",
       subtitle = bquote("Camarões/"~m^2),
       y = "Densidade Média",
       x = "",
       caption = "Azul Marinho Aquicultura") +
  scale_y_continuous(
    limits = (c(0, 18)),
    breaks = (seq(0, 18, 2)),
    labels = scales::label_number(big.mark = ".",
                                  decimal.mark = ","),
    expand = expansion(0)) +  #faz as barras encostarem no eixo
  # Linha média mensal ------------------------------------------------------

geom_hline(yintercept = mean(biom_2023_mensal$densidade_media, 
                             na.rm = TRUE), 
           color = "#1a0080",  #1a0080 
           linetype = "solid",
           linewidth = 0.8,
           alpha = 0.5) +  
  
  annotate("curve", 
           x = 3,
           y = 14, 
           xend = 3.5, 
           yend = 11.66,
           curvature = -0.3, 
           arrow = arrow(length = unit(2, "mm"))) +
  
  annotate(geom = "text", 
           x = 2, 
           y = 14.5, 
           label = paste0("média mensal = ", 
                          format(round(mean(biom_2023_mensal$densidade_media, 
                                            na.rm = TRUE),2), 
                                 big.mark = ".")),
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
  geom_text(aes(label = format(densidade_media, 
                               big.mark = ".", 
                               decimal.mark = ",")),
            vjust = -0.5, 
            color = "#000080", 
            size = 4.0)
# Inserindo o logo --------------------------------------------------------


image_url <- "https://drive.google.com/uc?id=1SN4gu5VzJYlfacpgoVycXNI8JRuswynA"
logo <- image_read(image_url)
grid::grid.raster(logo, 
                  x = 0.9, 
                  y = 0.8, 
                  just = c('left', 'bottom'), 
                  width = unit(1.3, 'inches'))

  