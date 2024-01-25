# Monthly Energy Costs



library(tidyverse) 
library(magick)

energy_costs <- tibble::tribble(
  ~mes,      ~`2020`,   ~`2021`,  ~`2022`,  ~`2023`,
  "jan",     2834.20,   2190.19,  3165.91,  2963.05, 
  "fev",     0000.00,   2679.27,  0000.00,  0000.00,
  "mar",     3158.37,   0000.00,  4805.83,  4575.29,
  "abr",     2060.97,   5604.27,  3134.57,  0000.00,
  "maio",    2082.82,   2414.85,  3036.50,  6326.02,
  "jun",     0000.00,   0000.00,  0000.00,  2961.39,
  "jul",     3101.30,   7227.92,  6216.45,  2546.11,
  "ago",     2203.39,   0000.00,  2775.57,  2872.81,
  "set",     0000.00,   5243.65,  3023.43,  3322.30,
  "out",     2203.39,   3319.91,  2486.78,  2946.30,
  "nov",     4960.46,   1473.22,  2952.89,  2732.84,
  "dez",     1271.46,   3322.52,  2686.11,  3205.62, 
)

energy_costs_long <- tidyr::pivot_longer(energy_costs, 
                                         cols = -"mes", 
                                         names_to = "ano", 
                                         values_to = "valor")

# Template Azul Marinho ---------------------------------------------------

d <- ggplot(energy_costs_long) + 
  geom_col(aes(factor(mes, 
                      levels = energy_costs$mes), 
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
         color = guide_legend(direction = "horizontal",
                              title = NULL)) +
  scale_y_continuous(labels = scales::comma_format(scale = 1, prefix = "R$"), 
                     limits = c(0, 8000)) +
  labs(title = "Custos Mensais com Energia", 
       subtitle = "2020 a 2023", 
       caption = "Fonte: Azul Marinho Aquicultura",
       x = "", y = "") +
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

d


# Inserindo o logo --------------------------------------------------------


image_url <- "https://drive.google.com/uc?id=1SN4gu5VzJYlfacpgoVycXNI8JRuswynA"
logo <- image_read(image_url)
grid::grid.raster(logo, 
                  x = 0.9, 
                  y = 0.8, 
                  just = c('left', 'bottom'), 
                  width = unit(1.3, 'inches'))







