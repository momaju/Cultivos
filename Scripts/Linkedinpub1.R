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



sp <- biom %>%
  ggplot(aes(x = baixa_mil, y = sobrevive)) +
  geom_point(shape = 21, color = "red", fill = "red", size = 3) +
  geom_smooth(method = lm, se = FALSE) +
  labs(title = "Shrimp Survival", x = "Dead shrimp per mille stocked",
  y = "Survival (%)") +
  theme_get()
sp


reg <- lm(sobrevivência ~ baixa.mil, data = biomassa)
reg
m1 <- summary(reg)

coeff <- coefficients(reg)

equation <- function(reg) {
  lm_coef <- list(a = round(coef(reg)[1], digits = 2),
                  b = round(coef(reg)[2], digits = 2),
                  r2 = round(summary(reg)$r.squared, digits = 2));
  lm_eq <- substitute(italic(y) == a + (b)
  %.% italic(x) * ""~~italic(r)^2~"="~r2, lm_coef)
  as.character(as.expression(lm_eq));
}


s <- sp + annotate("text", x = 1.00, y = 99, label = equation(reg),
 parse = TRUE, color = "Blue", size = 5)
s


### Residual plots
sobrevivência_res <- resid(reg)
plot(biomassa$baixa_mil, sobrevivência_res, ylab = "Residuals",
 xlab = "baixa.mil", main = "Sobrevivência")
abline(0, 0, col = "red")
