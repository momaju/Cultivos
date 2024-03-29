# Custos Mensais com Ração em 2023

library(tidyverse) 
library(magick)

feed_costs <- tibble::tribble(
  ~mes,       ~valor,
  "jan",    12910.98, 
  "fev",    17668.02,
  "mar",     3200.00,
  "abr",          NA,
  "mai",     7592.60,
  "jun",          NA,
  "jul",          NA,
  "ago",    39294.33,
  "set",    16813.78,
  "out",          NA,
  "nov",    26117.06,
  "dez",     3306.38, 
)



feed_costs %>%
  mutate(mes = factor(mes)) %>% 
  ggplot(aes(x = fct_inorder(mes), y = valor)) +
  geom_bar(stat = "identity", 
           width = 0.5, 
           show.legend = FALSE,
           fill = "#2e98fe") +
  labs(title = "2023 - Custos Mensais com Ração (R$)",
       #subtitle = "2023",
       y = "Valores",
       x = "",
       caption = "Azul Marinho Aquicultura") +
  scale_y_continuous(
    limits = (c(0, 45000)),
    breaks = (seq(0, 45000, 5000)),
    labels = scales::label_number(big.mark = ".",
                                  decimal.mark = ","),
    expand = expansion(0)) + #faz as barras encostarem no eixo
  
  # Linha média mensal ------------------------------------------------------

geom_hline(yintercept = mean(feed_costs$valor, na.rm = TRUE), 
           color = "#1a0080",  #1a0080 
           linetype = "solid",
           linewidth = 0.8,
           alpha = 0.5) +  
  
  annotate("curve", 
           x = 3.5,
           y = 20700, 
           xend = 3.0, 
           yend = 17300,
           curvature = 0.3, 
           arrow = arrow(length = unit(2, "mm"))) +
  
  annotate(geom = "text", 
           x = 4.0, 
           y = 20000, 
           label = paste0("média mensal = \n R$",
                          format(round(mean(feed_costs$valor, 
                                            na.rm = TRUE),2), 
                                 big.mark = ".")),
           hjust = "center",
           color = "#fe942e") +
  
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
  geom_text(aes(label = format(valor, 
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

  
