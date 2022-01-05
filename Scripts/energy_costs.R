
library(tidyverse)

#os dados foram lidos com read_csv. No estudo de Feed_costs os dados foram
# passados com s função tribble (veja o script Feed_costs.R)

energy_costs <- read_csv("mes,2020,2021
                         jan,2843.20,2190.19
                         fev,0.00,2679.27
                         mar,3158.37,0.00
                         abr,2060.97,5604.27
                         mai,2082.82,2414.85
                         jun, 0.00,0.00
                         jul,3101.30,7227.92
                         ago,2425.64,3960.34
                         set,0.00,2938.15
                         out,2203.39,3319.91
                         nov,4960.46,1473.22
                         dez,1271.31,3322.52")


energy_costs_long <- tidyr::pivot_longer(energy_costs, 
                                       cols = -"mes", 
                                       names_to = "ano", 
                                       values_to = "valor")


p <- ggplot(energy_costs_long) + 
  geom_col(aes(factor(mes, levels = energy_costs$mes), 
               valor, fill = ano), position = position_dodge(width = 0.9), na.rm = FALSE) + 
  
  
  ## add a hidden set of points to make the legend circles easily
  geom_point(aes(x = mes, y = -10, color = ano, fill = ano), size = 5, na.rm = TRUE) + 
  
  
  ## add the percentages just above each bar
  # geom_text(aes(mes, valor + 2, label = paste0("R$",valor), group = ano),
  #          position = position_dodge(width = 0.9), size = 3, vjust = -1.3, na.rm = FALSE) +
  
  geom_text(aes(mes, valor + 2, label = format( valor, big.mark = ".", decimal.mark = ","), group = ano),
            position = position_dodge(width = 0.9), size = 3, vjust = -1, na.rm = FALSE) +
  
  
  ## use similar colours to the original
  scale_fill_manual(values = c(2020 = "#088158", 2021 = "#BA2F2A")) +
  scale_color_manual(values = c(2020 = "#088158", 2021 = "#BA2F2A")) + 
  
  
  ## hide the fill legend and make the color legend horizontal
  guides(fill = "none", color = guide_legend(direction = "horizontal")) +
  scale_y_continuous(labels = scales::comma_format(scale = 1, prefix = "R$"), 
                     limits = c(0, 8000)) +
  labs(title = "Custos Mensais com Energia", 
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


# Saving the graphic ------------------------------------------------------


# ggsave("energia.png", width = 29, height = 21, units = "cm")

# ggsave("energia.pdf",width = 29, height = 21, units = "cm")



