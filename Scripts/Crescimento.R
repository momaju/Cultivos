# Crescimento dos viveros da Azul Marinho em relação à densidade de povoamento.

library(tidyverse)
library(googlesheets4)
library(ggthemes)
library(lubridate)
library(ggrepel)
library(scales)
library(corrplot)


biom <- read_sheet("1KkLM7bz-Az-etHUeENou-BjX4mDUfJCccwcCIo0k0CU", 2)


head(biom)
summary(biom)
str(biom)
glimpse(biom)
View(biom)

# Regressão ---------------------------------------------------------------


fit <- biom %>%
  lm(g_semana ~ densidade, data = .)
fit
summary(fit) # show results



# Gráfico sem agrupar por viveiro -----------------------------------------


sp <- biom %>%
  drop_na(densidade, g_semana) %>%
  ggplot(aes(x = densidade, y = g_semana)) +
  geom_point(size = 4, alpha = 0.5, color = "#eca11a") +
  geom_smooth(method = lm,
              se = F,
              color = "#c9543c") +
  xlim(0, 30) +
  ylim(0, 2.5) +
  labs(title = "Crescimento Médio Semanal em Função de Densidade de Estocagem",
       y = "Cescimento (g/semana)",
       x = "Densidade (ind/mq)",
       caption = "Fonte: Azul Marinho Aquicultura") +
  theme_minimal() +
  annotate("text", x = c(17, 17, 17), y = c(2.0, 1.9, 1.8),
           label = c("p-value:  1.582e-07",
                   "R-squared:  0.2332",
                   "g = 1.52 -0,04 * densidade"),
           size = 6,
           hjust = 0,
           color = "#eca11a") +
        theme(plot.caption = element_text(size = 9, color = "grey60"),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.title.y = element_text(size = 15, color = "black"),
        axis.title.x = element_text(size = 15, color = "black"))

sp

