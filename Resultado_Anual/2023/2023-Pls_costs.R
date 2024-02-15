

# Custos mensais com Pls em 2023 ------------------------------------------

library(tidyverse) 
library(magick)

pls_costs <- tibble::tribble(
  ~mes,       ~valor,
  "jan",    13000.00,   
  "fev",     2198.20, 
  "mar",     7133.00,  
  "abr",     5589.00,  
  "mai",          NA, 
  "jun",    11480.00,  
  "jul",          NA,  
  "ago",     9020.00,  
  "set",          NA,  
  "out",     6480.00,  
  "nov",     4725.00,  
  "dez",     4425.00,   
)


pls_costs %>%
  mutate(mes = factor(mes)) %>% 
  ggplot(aes(x = fct_inorder(mes), y = valor)) +
  geom_bar(stat = "identity", 
           width = 0.5, 
           show.legend = FALSE,
           fill = "#2e98fe") +
  labs(title = "2023 - Custos Mensais com PLs (R$)",
       #subtitle = "2023",
       y = "Valores",
       x = "",
       caption = "Azul Marinho Aquicultura") +
  scale_y_continuous(
    limits = (c(0, 15000)),
    breaks = (seq(0, 15000, 5000)),
    labels = scales::label_number(big.mark = ".",
                                  decimal.mark = ","),
    expand = expansion(0)) + #faz as barras encostarem no eixo
  
  # Linha média mensal ------------------------------------------------------

geom_hline(yintercept = mean(pls_costs$valor, na.rm = TRUE), 
           color = "#1a0080",  #1a0080 
           linetype = "solid",
           linewidth = 0.8,
           alpha = 0.5) +  
  
  annotate("curve", 
           x = 4.5,
           y = 8500, 
           xend = 5.0, 
           yend = 7117,
           curvature = -0.3, 
           arrow = arrow(length = unit(2, "mm"))) +
  
  annotate(geom = "text", 
           x = 4.0, 
           y = 8500, 
           label = paste0("média mensal = \n R$",
                          format(round(mean(pls_costs$valor, 
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

