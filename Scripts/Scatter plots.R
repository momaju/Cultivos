
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
  geom_point(aes(shape = viveiro, color = viveiro), size = 3) +
   geom_smooth(method = lm, se = FALSE)


sp <- biom %>%
mutate(viveiro = factor(viveiro)) %>%
ggplot(aes(baixa_mil, sobrevive)) +
  geom_point(aes(shape = viveiro, color = viveiro), size = 3) +
  geom_smooth(method = lm, se = FALSE) +
  labs(title = "Sobrevivência por ind.mortos/mil povoado",
        x = "Mortalidade em no. de Ind./milheiro",
        y = "Sobrevivência (%)",
        caption = "Azul Marinho Aquicultura") +
  theme_classic() +
  theme(plot.caption = element_text(size = 8, color = "grey60", hjust = 1))
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

coeff <- c(coefficients(reg),
summary(reg <- lm(sobrevive ~ baixa_mil, data = biom))$r.squared)

eq1 <- paste0("y = ", round(coeff[2], 2), "x + ", round(coeff[1], 2), "\nrˆ2 =",
 round(coeff[3], 2))

sp + geom_text(x = 1.0, y = 200, label = eq1, color = "navy blue", cex = 5)
  


# Outra forma:

sp + geom_label(x = 1.0, y = 200, label = eq1, color = "navy blue", cex = 5)
