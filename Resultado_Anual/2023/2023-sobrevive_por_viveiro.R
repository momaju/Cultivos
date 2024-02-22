
## Sobrevivência anual por viveiro
## 

library(tidyverse)
library(googlesheets4)
library(ggthemes)
library(lubridate)
library(scales)
library(magick)

biom <- read_sheet("1KkLM7bz-Az-etHUeENou-BjX4mDUfJCccwcCIo0k0CU", 2)


# Boxplot sobrevivếncia ---------------------------------------------------

sobrevive_viveiro_2023 <- biom %>%
  mutate(ano_desp = (year(data_desp)), 
         viveiro = factor(viveiro)) %>%
  filter(ano_desp == 2023) %>% # exclui o ano de 2024
  group_by(viveiro)
  #summarize(sobrevive = median(sobrevive))

sobrevive_viveiro_2023 %>% 
  ggplot(aes(viveiro, sobrevive)) +
  geom_boxplot(fill = "#8080c0",
               col = "#000080",
               outlier.color = "#8080c0",
               outlier.size = 3,
               linetype = 1) +
  stat_summary(fun = mean, 
               geom = "point", 
               shape = 21, 
               size = 3, 
               color = "#c0c080", 
               fill = "#c0c080") + 
  coord_flip() +
  labs(title = paste0(max(sobrevive_viveiro_2023$ano_desp)," - Sobrevivência Anual por Viveiro"),
       #subtitle = paste0("Em ", max(sobrevive_viveiro_2023$ano_desp)),
       x = "Viveiro",
       y = "Percentual",
       caption = "Azul Marinho Aquicultura") +
  theme_minimal() +
  theme(plot.caption = element_text(size = 9, 
                                    color = "#8080c0"),
        axis.text.y = element_text(size = 15, 
                                   color = "#000080"),
        axis.text.x = element_text(size = 15, 
                                   color = "#000080"),
        axis.title.y = element_text(size = 15,
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
                                     color = "#000080" ),
        #axis.line.y = element_line(color = "#000080"),
        #axis.line.x = element_line(color = "#000080"),
        panel.grid.major = element_blank())

# Inserindo o logo

image_url <- "https://drive.google.com/uc?id=1SN4gu5VzJYlfacpgoVycXNI8JRuswynA"
logo <- image_read(image_url)
#logo <- image_read("G://My Drive//RWork//Projects//Azul Marinho//Cultivos//Images//azul_logo_transp.png")
grid::grid.raster(logo,
                  x = 0.9, 
                  y = 0.8, 
                  just = c('left', 'bottom'), 
                  width = unit(1.3, 'inches'))


# Boxplot sobrevivếncia ---------------------------------------------------

sobrevive_viveiro <- biom %>%
  mutate(ano_desp = factor(year(data_desp)), viveiro = factor(viveiro)) %>%
  #filter(ano_desp != 2024) %>% # exclui o ano de 2024
  group_by(viveiro)
#summarize(sobrevive = median(sobrevive))

sobrevive_viveiro %>% 
  ggplot(aes(viveiro, sobrevive)) +
  geom_boxplot(fill = "#8080c0",
               col = "#000080",
               outlier.color = "#8080c0",
               outlier.size = 3,
               linetype = 1) +
  stat_summary(fun = mean, 
               geom = "point", 
               shape = 21, 
               size = 3, 
               color = "#c0c080", 
               fill = "#c0c080") + 
  coord_flip() +
  labs(title = "Sobrevivência Anual por Viveiro",
       subtitle = "2015 a 2024",
       x = "Viveiro",
       y = "Percentual",
       caption = "Azul Marinho Aquicultura") +
  theme_minimal() +
  theme(plot.caption = element_text(size = 9, 
                                    color = "#8080c0"),
        axis.text.y = element_text(size = 15, 
                                   color = "#000080"),
        axis.text.x = element_text(size = 15, 
                                   color = "#000080"),
        axis.title.y = element_text(size = 15,
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
                                     color = "#000080" ),
        #axis.line.y = element_line(color = "#000080"),
        #axis.line.x = element_line(color = "#000080"),
        panel.grid.major = element_blank())

# Inserindo o logo

image_url <- "https://drive.google.com/uc?id=1SN4gu5VzJYlfacpgoVycXNI8JRuswynA"
logo <- image_read(image_url)
#logo <- image_read("G://My Drive//RWork//Projects//Azul Marinho//Cultivos//Images//azul_logo_transp.png")
grid::grid.raster(logo,
                  x = 0.9, 
                  y = 0.8, 
                  just = c('left', 'bottom'), 
                  width = unit(1.3, 'inches'))
