
# Dados referentes ao Viveiro 04


library(tidyverse)
library(googlesheets4)
library(ggthemes)
library(lubridate)
library(ggrepel)
library(scales)
library(corrplot)
library(magick)

# lendo os dados da planilha biomassa no google sheets

biom <- read_sheet("1KkLM7bz-Az-etHUeENou-BjX4mDUfJCccwcCIo0k0CU", 2)


# Regressão: Biomassa Calculada x Biomassa Real ---------------------------

v4 <- biom %>%
  filter(viveiro == 4)


p <- v4 %>%
  ggplot(aes(biom_calc, biom_real)) +
  geom_point(size = 2.5, color = "#000080") +
  geom_smooth(method = lm, se = FALSE) +
  labs(title = "V04 -Total Despescado (kg) por ciclo: Calculado vs Real",
       y = "Biomassa Real",
       x = "Biomassa Calculada",
       caption = "Fonte: Azul Marinho Aquicultura") +
  theme_minimal() +
  theme(plot.caption = element_text(size = 9, color = "#8080c0"),
        axis.text.y = element_text(size = 12, color = "#000080"),
        axis.text.x = element_text(size = 12, color = "#000080"),
        axis.title.y = element_text(size = 20,
                                    color = "#000080",
                                    margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(size = 20, color = "#000080"),
        plot.title = element_text(size = 25, color = "#000080"),
        axis.line.y = element_line(color = "#000080"),
        axis.line.x = element_line(color = "#000080"),
        panel.grid.major = element_blank(),
        legend.position = "top",
        legend.key.size = unit(0.5, "cm"),
        legend.title = element_text(size = 20, color = "#8080c0")) +
  geom_text(
    aes(label = ciclo), nudge_y = 100, color = "#8080c0" )

p

# Escrevendo a equação da regressão ---------------------------------------


fit4 <- lm(biom_real ~ biom_calc, data = v4)
summary(fit4)


p + geom_text(x = 1500.00, y = 4500.00, 
              label = lm_eqn(fit4), 
              parse = TRUE,
              color = "#000080",
              size = 5)



# Inserindo o logo --------------------------------------------------------

logo <- image_read("G://My Drive//RWork//Projects//Azul Marinho//Cultivos//Images//azul_logo_transp.png")
grid::grid.raster(logo, x = 0.92, y = 0.85, just = c('left', 'bottom'), width = unit(1.3, 'inches'))


# Selecionando variéves do viveiro 4 ----------------------------------------


v4_selected <-  v4 %>% 
  select(data_desp, ciclo, ddc, g_final,biom_real, densidade, lab)

v4_ordered <- v4_selected[order(v4_selected$biom_real, decreasing = TRUE),]

v4_ordered
view(v4_ordered)



# Which.max ---------------------------------------------------------------

v4_selected[which.max(v4_selected$biom_real),]

# Utilizando a função slice()

v4_selected %>% slice_max(biom_real, n = 5)

# Top_five (ordered)-----------------------------------------------------------
# O mesmo resultado do código acima.
# top_n() has been superseded in favour of slice_min()/slice_max()

v4_ordered %>% top_n(5, biom_real)
