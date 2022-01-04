Biomassa <- read.csv(file = "Cultivo - Biomassa.csv", header = T)
Biomassa$viveiro = factor(Biomassa$viveiro, levels = c(1,2,3,4), labels = c("V1","V2","V3","V4"))

Biomassa$ciclo = factor(Biomassa$ciclo,levels = c(1,2,3,4,5,6,7,8,9,10,11,12), labels = c("C1","C2","C3","C4","C5","C6","C7","C8","C9","C10", "C11","C12"))

Biomassa$ano = factor(Biomassa$ano)
Biomassa$bionutri = factor(Biomassa$bionutri)
library(ggplot2)

ggplot(Biomassa, aes(x = baixa.mil, y = biom.real)) +
  geom_point(aes(shape=viveiro, color=viveiro)) + geom_smooth(method = lm, se= FALSE)


sp <- ggplot(Biomassa, aes(x = baixa.mil, y = sobrevivência)) +
  geom_point(aes(shape=viveiro, color=viveiro), size = 3) + 
  geom_smooth(method = lm, se = FALSE) +
  labs(title = "Sobrevivência por ind.mortos/mil povoado", x = "Ind./milheiro", y = "Sobrevivência (%)", caption = "Azul Marinho") +
  theme_classic()
sp

# Fit regression line
reg<-lm(sobrevivência ~ baixa.mil, data = Biomassa)
reg

# coeff = coefficients(reg)

equation = function(reg) {
  lm_coef <- list(a = round(coef(reg)[1], digits = 2),
                  b = round(coef(reg)[2], digits = 2),
                  r2 = round(summary(reg)$r.squared, digits = 2));
  lm_eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,lm_coef)
  as.character(as.expression(lm_eq));                 
}

s <- sp + annotate("text", x = 0.75, y = 60, label = equation(reg), parse = TRUE, color = "Blue", size=5)
s

# Equation of the line:
#eq = paste0("y = ", round(coeff[2],1), "x + ", round(coeff[1],1))

#sp + geom_text(x=0.6, y=86, label=eq, color = "navy blue", cex = 6)
 
# Incluindo R squared na regressão

coeff1 = c(coefficients(reg),summary(reg<-lm(sobrevivência ~ baixa.mil, data = Biomassa))$r.squared)

eq1 = paste0("y = ", round(coeff[2],2), "x + ", round(coeff[1],2),"\nrˆ2 =", round(coeff[3],2))

sp + geom_text(x=20, y=500, label=eq1, color = "navy blue", cex = 5) 
  


# Outra forma:

sp + geom_label(x=20, y=500, label=eq1, color = "navy blue", cex = 5)

