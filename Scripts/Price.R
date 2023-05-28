

library(tidyverse)
library(lubridate)
library(googlesheets4)
library(ggthemes)
library(scales)
library(magick)



price <- read_sheet("https://docs.google.com/spreadsheets/d/1KkLM7bz-Az-etHUeENou-BjX4mDUfJCccwcCIo0k0CU/edit#gid=1319412488",
                    sheet = 6)
price_pivoted <- price %>% 
  rename(Corrigido ='C10G_Fev_2022', Nominal = 'C10G_nominal') %>% 
  select(Data, Corrigido, Nominal) %>% 
  pivot_longer(c(Corrigido,Nominal),
               names_to = "preço", values_to = "valor") %>% 
  mutate(ano = year(Data),
         mês = month(Data),
         dia = day(Data)) 
  


price_pivoted %>% 
  group_by(ano, preço) %>% 
  summarise(valor = mean(valor)) %>% 
  ggplot(aes(ano, valor, color = preço)) +
  geom_point(aes(shape = preço), size = 4) +
  #geom_line(size = 1)+
  geom_smooth(se = FALSE) +
  scale_color_brewer(palette = "Dark2") +
  #scale_color_manual(values = c("#E7B800","#FC4E07")) +
  #theme_fivethirtyeight()
  theme_tufte() +
  labs(title = "Preços Médios Anuais ao Produtor Para Camarão de 10g",
       subtitle = "Valores em Reais, nominais e corrigidos pela inflação até fev/2022",
       y = "Valor (R$)",
       x = "Ano",
       caption = "Mozart Marinho-Jr") +
  theme(plot.caption = element_text(color = "gray60", size = 8))



# Somente os preços corrigidos --------------------------------------------

price_selected <- price %>% 
  rename(corrigido ='C10G_Fev_2022') %>% 
  select(Data, corrigido) %>% 
  mutate(ano = year(Data),
         mês = month(Data),
         dia = day(Data))

price_selected %>% 
  group_by(ano) %>% 
  summarise(corrigido = mean(corrigido)) %>% 
  ggplot(aes(ano, corrigido)) +
  geom_point(size = 4, color = "#2e98f3") +
  #geom_line(size = 1)+
  #geom_line()+
  geom_smooth(se = FALSE, color = "#2e98f3") +
  scale_y_continuous(labels = label_dollar(prefix = "R$"),
                     expand = expansion(0),
                     limits = c(10,30)) +
  #scale_color_manual(values = c("#E7B800","#FC4E07")) +
  #theme_fivethirtyeight()
  theme_light() +
  #theme_classic()
  labs(title = "Preços Médios Anuais Ofertados ao Produtor Para Camarão de 10g",
       subtitle = "Valores em Reais (R$/kg), corrigidos pela inflação até fev/2022",
       y = "Valor",
       x = "Ano",
       caption = "Azul Marinho Aquicultura") +
  theme(plot.caption = element_text(color = "#8080c0", size = 9),
        axis.text.y = element_text(size = 15, color = "#000080"),
        axis.text.x = element_text(size = 15, color = "#000080"),
        #axis.text = element_text(colour = "#EE0000", size = 10),
        axis.title.y = element_text(size = 20,
                                    color = "#000080"),
        axis.title.x = element_text(size = 20, color = "#000080"),
        plot.title = element_text(size = 25, color = "#000080"),
        plot.subtitle = element_text(size = 12, color = "#000080"),
        panel.grid.major = element_line(color = "#9999cc", size = 0.1),
        panel.grid.minor = element_blank()) +
  geom_text(aes(label = round((corrigido),2)),
            check_overlap = T,
            nudge_y = 0.8,
            color ="#000080",
            fontface = "bold")

# Preços corrigidos até 2023-03 -------------------------------------------

## Os preços foram corrigidos utilizando-se a caculadora do cidadão, disponível
## no site do Banco Central do Brasil: 
## https://www3.bcb.gov.br/CALCIDADAO/publico/corrigirPorIndice.do?method=corrigirPorIndice


price2023_pivoted <- price %>% 
  rename(Corrigido ='C10G_Mar_2023', Nominal = 'C10G_nominal') %>% 
  select(Data, Corrigido, Nominal) %>% 
  pivot_longer(c(Corrigido,Nominal),
               names_to = "preço", values_to = "valor") %>% 
  mutate(ano = year(Data),
         mês = month(Data),
         dia = day(Data)) 

price2023_pivoted %>% 
  group_by(ano, preço) %>% 
  summarise(valor = mean(valor)) %>% 
  ggplot(aes(ano, valor, color = preço)) +
  geom_point(aes(shape = preço), size = 4) +
  #geom_line(size = 1)+
  geom_smooth(se = FALSE) +
  scale_color_brewer(palette = "Dark2") +
  #scale_color_manual(values = c("#E7B800","#FC4E07")) +
  #theme_fivethirtyeight()
  theme_tufte() +
  labs(title = "Preços Médios Anuais ao Produtor Para Camarão de 10g",
       subtitle = "Valores em Reais, nominais e corrigidos pela inflação até mar/2023",
       y = "Valor (R$)",
       x = "Ano",
       caption = "Mozart Marinho-Jr") +
  theme(plot.caption = element_text(color = "gray60", size = 8))


# Somente os preços corrigidos --------------------------------------------

price2023_selected <- price %>% 
  rename(corrigido ='C10G_Mar_2023') %>% 
  select(Data, corrigido) %>% 
  mutate(ano = year(Data),
         mês = month(Data),
         dia = day(Data))

price2023_plot <- price2023_selected %>% 
  group_by(ano) %>% 
  summarise(corrigido = mean(corrigido)) %>% 
  ggplot(aes(ano, corrigido)) +
  geom_point(size = 4, color = "#2e98f3") +
  #geom_line(size = 1)+
  #geom_line()+
  geom_smooth(se = FALSE, color = "#2e98f3",
              method = "loess",
              span = 0.5) +
  scale_y_continuous(labels = label_dollar(prefix = "R$"),
                     expand = expansion(0),
                     limits = c(10,35)) +
  scale_x_continuous(limits = c(2016,2023),
                     breaks = c(2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  #scale_color_manual(values = c("#E7B800","#FC4E07")) +
  #theme_fivethirtyeight()
  theme_light() +
  #theme_classic()
  labs(title = "Preços Médios Anuais Ofertados ao Produtor Para Camarão de 10g",
       subtitle = "Valores em Reais (R$/kg), corrigidos pela inflação até mar/2023",
       y = "Valor",
       x = "Ano",
       caption = "Azul Marinho Aquicultura") +
  theme(plot.caption = element_text(color = "#8080c0", size = 9),
        axis.text.y = element_text(size = 15, color = "#000080"),
        axis.text.x = element_text(size = 15, color = "#000080"),
        #axis.text = element_text(colour = "#EE0000", size = 10),
        axis.title.y = element_text(size = 20,
                                    color = "#000080"),
        axis.title.x = element_text(size = 20, color = "#000080"),
        plot.title = element_text(size = 25, color = "#000080"),
        plot.subtitle = element_text(size = 12, color = "#000080"),
        panel.grid.major = element_line(color = "#9999cc", size = 0.1),
        panel.grid.minor = element_blank()) +
  geom_text(aes(label = round((corrigido),2)),
            check_overlap = T,
            nudge_y = 0.8,
            color ="#000080",
            fontface = "bold")

price2023_plot


## inserindo a imagem do logo

logo <- image_read("Images/azul_logo_transp.png")
#grid::grid.raster(logo, x = 0.1, y = 0.02, just = c('left', 'bottom'), 
#width = unit(1.9, 'inches'))


grid::grid.raster(logo, 
                  x = 0.89,
                  y = 0.845, 
                  just =c("center", "center"),
                  width = unit(3.5, 'cm'))

ggsave("price2023_plot.png",
       width = 29.7, 
       height = 25,
       units = "cm")
       

