
library(tidyverse)
library(googlesheets4)
library(ggthemes)
library(lubridate)
library(ggrepel)
library(scales)
library(corrplot)


biom <- read_sheet("1KkLM7bz-Az-etHUeENou-BjX4mDUfJCccwcCIo0k0CU", 2)
str(biom)
summary(biom)
glimpse(biom)
View(biom)


biom %>%
mutate(viveiro = factor(viveiro)) %>%
ggplot(aes(baixa_mil, biom_real)) +
  geom_point(aes(shape = viveiro, color = viveiro), size = 4) +
   geom_smooth(method = lm, se = FALSE) +
   labs(title = "Produção Total (kg)",
        subtitle = "Em Função de Camarões Mortos por\nMilheiro Povoado",
        x = "Mortalidade em no. de Camarões/Milheiro Povoado",
        y = "Total Despescado (kg)",
        caption = "Azul Marinho Aquicultura") +
  theme_classic() +
  theme(plot.caption = element_text(size = 8, color = "grey60", hjust = 1),
        plot.title = element_text(size = 30),
        axis.title.y = element_text(size = 25),
        axis.title.x = element_text(size = 25),
        plot.subtitle = element_text(size = 25))


sp <- biom %>%
mutate(viveiro = factor(viveiro)) %>%
ggplot(aes(baixa_mil, sobrevive)) +
  geom_point(aes(shape = viveiro, color = viveiro), size = 4) +
  geom_smooth(method = lm, se = FALSE) +
  labs(title = "Sobrevivência por ind.mortos/mil povoado",
        x = "Mortalidade em no. de Camarões/Milheiro Povoado",
        y = "Sobrevivência (%)",
        caption = "Azul Marinho Aquicultura") +
  theme_classic() +
  theme(plot.caption = element_text(size = 8, color = "grey60", hjust = 1),
        plot.title = element_text(size = 30),
        axis.title.y = element_text(size = 25),
        axis.title.x = element_text(size = 25))
sp

# Fit regression line------------------------------------
reg <- biom %>%
lm(sobrevive ~ baixa_mil, data = .)
reg


equation <- function(reg) {
  lm_coef <- list(a = round(coef(reg)[1], digits = 2),
                  b = round(coef(reg)[2], digits = 2),
                  r2 = round(summary(reg)$r.squared, digits = 2));
  lm_eq <- substitute(italic(y) == a + b %.% italic(x) * ","
  ~~italic(r)^2~"="~r2, lm_coef)
  as.character(as.expression(lm_eq));
}

sp + annotate("text", x = 1, y = 200, label = equation(reg),
 parse = TRUE, color = "Blue", size = 10)



 
# Incluindo R squared na regressão-----------------------------------

sp + annotate("text", x = 0.75, y = 180, parse = FALSE,
           label = "y = 99.03  -31.15 * baixa_mil",
           size = 10,
           color = "blue")
