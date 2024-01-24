# Monthly Feed Costs
# 


library(tidyverse) 
library(magick)

feed_costs <- tibble::tribble(
  ~mes,      ~`2020`,   ~`2021`,  ~`2022`,  ~`2023`,
  "jan",    18274.70,  13125.00,  4255.00, 12910.98, 
  "fev",    10788.44,   5280.00, 19600.00, 17668.02,
  "mar",    12367.78,  21217.00, 16755.00,  3200.00,
  "abr",    14920.50,  13842.00, 26875.00, 00000.00,
  "maio",    7815.10,   7290.00, 10150.00, 27592.60,
  "jun",    11770.65,  17430.00, 13000.00, 00000.00,
  "jul",    14406.00,  18430.00, 00000.00, 00000.00,
  "ago",    25733.29,  16878.00, 233400.00,39294.33,
  "set",    32361.95,  11200.00, 14375.00, 16813.78,
  "out",    18423.00,  25200.00, 36250.98, 00000.00,
  "nov",     6000.00,  23855.00, 17280.98, 26117.06,
  "dez",    27041.41,  22375.00, 15950.98,  3306.38, 
)

# Template Azul Marinho ---------------------------------------------------

d <- ggplot(feed_costs_long) + 
  geom_col(aes(factor(mes, 
                      levels = feed_costs$mes), 
               valor, 
               fill = ano), 
           position = position_dodge(width = 0.9), 
           na.rm = FALSE) + 
  
  
  ## add a hidden set of points to make the legend circles easily
  geom_point(aes(x = mes, y = -10, color = ano, fill = ano), 
             size = 5, 
             na.rm = TRUE) + 
  
  
  geom_text(aes(mes, 
                valor + 2, 
                label = format( valor, big.mark = ".",
                                decimal.mark = ",",), 
                group = ano),
            position = position_dodge(width = 0.9), 
            size = 3, 
            vjust = 0.5,
            hjust = 1.1,
            angle = 90,
            na.rm = TRUE,
            color = "white") +
  
  
  ## use similar colours to the original
  scale_fill_manual(values = c(`2020` = "#D39200", 
                               `2021` = "#008B8B", 
                               `2022` = "#0041d3", 
                               `2023` = "#8b0000")) +
  
  scale_color_manual(values = c(`2020` = "#D39200", 
                                `2021` = "#008B8B", 
                                `2022` = "#0041d3", 
                                `2023` = "#8b0000")) +
  
  
  ## hide the fill legend and make the color legend horizontal
  guides(fill = "none", 
         color = guide_legend(direction = "horizontal")) +
  scale_y_continuous(labels = scales::comma_format(scale = 1, prefix = "R$"), 
                     limits = c(0, 35000)) +
  labs(title = "Custos Mensais com Ração", 
       subtitle = "2020 a 2023", 
       caption = "Fonte: Azul Marinho Aquicultura",
       x = "", y = "") +
  theme_minimal() + 
  #theme(axis.text = element_text(size = 10),
  #      plot.title = element_text(size = 28, hjust= 0.5), 
  #      plot.subtitle = element_text(size = 20, hjust = 0.5),
  #      plot.caption = element_text(size = 7, color = "grey60"),
  #      plot.background = element_rect(fill = "#f4f7fc", size = 0),
  #      legend.title = element_blank(),
  #      legend.text= element_text(size = 12),
  #      panel.grid = element_blank(),
  
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
                                                    b = 0, l = 0)),
        axis.title.x = element_text(size = 20, 
                                    color = "#000080"),
        plot.title = element_text(size = 25, 
                                  color = "#000080"),
        plot.subtitle = element_text(size = 18, 
                                     color = "#000080"),
        #plot.subtitle = element_markdown(size = 15, color = "#000080"),
        axis.line.y = element_line(color = "#000080"),
        axis.line.x = element_line(color = "#000080"),
        panel.grid.major = element_blank(),
        legend.position = "top",
        legend.text = element_text(size =  20)) 
#  geom_text(aes(label = format(produtividade_media, 
#                               big.mark = ".", 
#                               decimal.mark = ",")),
#  vjust = -0.5, 
#  color = "#000080", 
#  size = 4.0)  

## move the color legend to an inset 
#legend.position = c(0.9, 0.9)
d


# Inserindo o logo --------------------------------------------------------


image_url <- "https://drive.google.com/uc?id=1SN4gu5VzJYlfacpgoVycXNI8JRuswynA"
logo <- image_read(image_url)
grid::grid.raster(logo, 
                  x = 0.9, 
                  y = 0.8, 
                  just = c('left', 'bottom'), 
                  width = unit(1.3, 'inches'))
