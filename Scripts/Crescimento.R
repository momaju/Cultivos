# Crescimento dos viveros da Azul Marinho em relação à densidade de povoamento.

library(tidyverse)
library(googlesheets4)
library(ggthemes)
library(lubridate)
library(ggrepel)
library(scales)
library(corrplot)


Biom <- read_sheet("1KkLM7bz-Az-etHUeENou-BjX4mDUfJCccwcCIo0k0CU", 2)


head(Biom)
summary(Biom)
str(Biom)



# Regressão ---------------------------------------------------------------


fit <- Biom %>% 
  lm(g_semana ~ densidade, data = .)
fit
summary(fit) # show results



# Gráfico sem agrupar por viveiro -----------------------------------------


sp <- Biom %>% 
  drop_na(densidade, g_semana) %>% 
  ggplot(aes(x = densidade, y = g_semana)) + 
  geom_point(size = 4, alpha = 0.5, color = "blue") + 
  geom_smooth(method = lm, se = F) +
  labs(title = "Crescimento Médio Semanal em Função de Densidade de Estocagem",
       y = "Peso (g)",
       x = "Densidade (ind/mq)",
       caption = "Fonte: Azul Marinho Aquicultura")+
  theme_minimal() +
  annotate("text",x=c(17,17,17),y=c(2.0, 1.9, 1.8), 
           label=c("p-value:  1.582e-07",
                   "R-squared:  0.2332",
                   "g = 1.52 * -0,04dens"), 
           size = 6, 
           hjust = 0,
           color = "blue") +
        theme(plot.caption = element_text(size = 9, color = "grey60"),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.title.y = element_text(size = 15, color = "black"),
        axis.title.x = element_text(size = 15, color = "black"))

sp




