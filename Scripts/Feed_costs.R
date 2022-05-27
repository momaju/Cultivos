# Custos Mensais de Ração

library(tidyverse)   

feed_costs <- tibble::tribble(
  ~mes,      ~`2020`,   ~`2021`,
  "jan",    18274.70,  13125.00,
  "fev",    10788.44,   5280.00,
  "mar",    12367.78,  21217.00,
  "abr",    14920.50,  13842.00,
  "maio",    7815.10,   7290.00,
  "jun",    11770.65,  17430.00,
  "jul",    14406.00,  18430.00,
  "ago",    25733.29,  16878.00,
  "set",    32361.95,  11200.00,
  "out",    18423.00,  25200.00,
  "nov",     6000.00,  23855.00,
  "dez",    27041.41,  22375.00,
  )



## pivot to long format with the 
## ano and valor as their own columns
feed_costs_long <- tidyr::pivot_longer(feed_costs, 
                                      cols = -"mes", 
                                      names_to = "ano", 
                                      values_to = "valor")

## plot the anos side-by-side in the original order

p <- ggplot(feed_costs_long) + 
  geom_col(aes(factor(mes, levels = feed_costs$mes), 
               valor, fill = ano), position = position_dodge(width = 0.9), na.rm = FALSE) + 
  
  
  ## add a hidden set of points to make the legend circles easily
  geom_point(aes(x = mes, y = -10, color = ano, fill = ano), size = 5, na.rm = TRUE) + 
  
  
  ## add the percentages just above each bar
 # geom_text(aes(mes, valor + 2, label = paste0("R$",valor), group = ano),
#          position = position_dodge(width = 0.9), size = 3, vjust = -1.3, na.rm = FALSE) +
  
  geom_text(aes(mes, valor + 2, label = format( valor, big.mark = ".", decimal.mark = ","), group = ano),
            position = position_dodge(width = 0.9), size = 3, vjust = -1, na.rm = FALSE) +
            
  
   ## use similar colours to the original
  scale_fill_manual(values = c(`2020` = "#D39200", `2021` = "darkcyan")) +
  scale_color_manual(values = c(`2020` = "#D39200", `2021` = "darkcyan")) + 
  
  
  ## hide the fill legend and make the color legend horizontal
  guides(fill = "none", color = guide_legend(direction = "horizontal")) +
  scale_y_continuous(labels = scales::comma_format(scale = 1, prefix = "R$"), 
                     limits = c(0, 35000)) +
  labs(title = "Custos Mensais com Ração", 
       subtitle = "2020 vs 2021", 
       caption = "Fonte: Azul Marinho Aquicultura",
       x = "", y = "") +
  theme_minimal() + 
  theme(axis.text = element_text(size = 10),
        plot.title = element_text(size = 28, hjust= 0.5), 
        plot.subtitle = element_text(size = 20, hjust = 0.5),
        plot.caption = element_text(size = 7, color = "grey60"),
        plot.background = element_rect(fill = "#f4f7fc", size = 0),
        legend.title = element_blank(),
        legend.text= element_text(size = 12),
        panel.grid = element_blank(),
  
        
        ## move the color legend to an inset 
        legend.position = c(0.9, 0.9)) 
p





