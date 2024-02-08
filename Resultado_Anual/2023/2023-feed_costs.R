# Custos Mensais com Ração em 2023

library(tidyverse) 
library(magick)

feed_costs <- tibble::tribble(
  ~mes,      ~`2020`,   ~`2021`,  ~`2022`,  ~`2023`,
  "jan",    18274.70,  13125.00,  4255.00, 12910.98, 
  "fev",    10788.44,   5280.00, 19600.00, 17668.02,
  "mar",    12367.78,  21217.00, 16755.00,  3200.00,
  "abr",    14920.50,  13842.00, 26875.00,       NA,
  "maio",    7815.10,   7290.00, 10150.00, 27592.60,
  "jun",    11770.65,  17430.00, 13000.00,       NA,
  "jul",    14406.00,  18430.00,       NA,       NA,
  "ago",    25733.29,  16878.00, 23340.00, 39294.33,
  "set",    32361.95,  11200.00, 14375.00, 16813.78,
  "out",    18423.00,  25200.00, 36250.98,       NA,
  "nov",     6000.00,  23855.00, 17280.98, 26117.06,
  "dez",    27041.41,  22375.00, 15950.98,  3306.38, 
)

feed_costs_long <- tidyr::pivot_longer(feed_costs, 
                                         cols = -"mes", 
                                         names_to = "ano", 
                                         values_to = "valor")

feed_costs_long %>%
  filter(ano == "2023") %>%
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

geom_hline(yintercept = mean(feed_costs_long$valor, na.rm = TRUE), 
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
                          format(round(mean(feed_costs_long$valor, 
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
  
