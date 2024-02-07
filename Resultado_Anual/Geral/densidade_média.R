

library(tidyverse)
library(googlesheets4)
library(ggthemes)
library(lubridate)
library(ggrepel)
library(scales)
library(corrplot)
library(magick)


biom <- read_sheet("1KkLM7bz-Az-etHUeENou-BjX4mDUfJCccwcCIo0k0CU", 2)

biom_ano <- biom %>%
  mutate(ano_desp = year(data_desp)) %>% 
  #filter(ano_desp != 2024) %>% # exclui o ano de 2024
  group_by(ano_desp) %>%
  summarize(densidade_media = round(mean(densidade), 2))

biom_ano %>%
  ggplot(aes(x = ano_desp, y = densidade_media)) +
  geom_bar(stat = "identity", 
           width = 0.5, 
           show.legend = FALSE,
           fill = "#2e98fe") +
  labs(title = "Densidade Média no Povoamento",
       subtitle = bquote("Camarões/"~m^2), #superscrit
       y = "Densidade Média",
       x = "Ano",
       caption = "Fonte: Azul Marinho Aquicultura") +
  scale_y_continuous(
    labels = scales::label_number(big.mark = ".",
                                  decimal.mark = ","),
  expand = expansion(0)) +
  expand_limits(y = 25) +
  scale_x_continuous(breaks = biom_ano$ano_desp) +
  theme_minimal() +
  theme(plot.caption = element_text(size = 9, 
                                    color = "#8080c0"),
        axis.text.y = element_text(size = 15, 
                                   color = "#000080"),
        axis.text.x = element_text(size = 15, 
                                   color = "#000080"),
        axis.title.y = element_text(size = 20,
                                    color = "#000080",
                                    margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(size = 20, 
                                    color = "#000080"),
        plot.title = element_text(size = 25, 
                                  color = "#000080"),
        plot.subtitle = element_text(size = 18, 
                                     color = "#000080"),
        #plot.subtitle = element_markdown(size = 15, color = "#000080"),
        axis.line.y = element_line(color = "#000080"),
        axis.line.x = element_line(color = "#000080"),
        panel.grid.major = element_blank()) +
  geom_text(aes(label = format(densidade_media, 
                               big.mark = ".", 
                               decimal.mark = ",")),
            vjust = -0.5, 
            color = "#000080", 
            size = 4.0) 



# Inserindo o logo --------------------------------------------------------

# logo <- image_read("G://My Drive//RWork//Projects//Azul Marinho//Cultivos//Images//azul_logo_transp.png") # no windows
# logo <-  image_read("/mnt/chromeos/GoogleDrive/MyDrive/RWork/Projects/Azul Marinho/Cultivos/Images/azul_logo_transp.png") # no ChromeOS 


image_url <- "https://drive.google.com/uc?id=1SN4gu5VzJYlfacpgoVycXNI8JRuswynA"
logo <- image_read(image_url)
grid::grid.raster(logo, 
                  x = 0.9, 
                  y = 0.8, 
                  just = c('left', 'bottom'), 
                  width = unit(1.3, 'inches'))
