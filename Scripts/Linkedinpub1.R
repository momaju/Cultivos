Biomassa <- read.csv(file = "Cultivo - Biomassa.csv", header = T)
Biomassa$viveiro = factor(Biomassa$viveiro, levels = c(1,2,3,4), labels = c("V1","V2","V3","V4"))

Biomassa$ciclo = factor(Biomassa$ciclo,levels = c(1,2,3,4,5,6,7,8,9,10,11,12), labels = c("C1","C2","C3","C4","C5","C6","C7","C8","C9","C10", "C11", "C12"))
Biomassa$ano = factor(Biomassa$ano)


library(ggplot2)

sp <- ggplot(Biomassa, aes(x = baixa.mil, y = sobrevivência)) +
  geom_point(shape = 21, color = "red",fill = "red", size = 3) + 
  geom_smooth(method = lm, se = TRUE) +
  labs(title = "Shrimp Survival", x = "Dead shrimp per mille stocked", y = "Survival (%)") +
  theme_get()
sp


reg<-lm(sobrevivência ~ baixa.mil, data = Biomassa)
reg
m1 <- summary(reg)

coeff = coefficients(reg)

equation = function(reg) {
  lm_coef <- list(a = round(coef(reg)[1], digits = 2),
                  b = round(coef(reg)[2], digits = 2),
                  r2 = round(summary(reg)$r.squared, digits = 2));
  lm_eq <- substitute(italic(y) == a + (b) %.% italic(x)*""~~italic(r)^2~"="~r2,lm_coef)
  as.character(as.expression(lm_eq));                 
}


s <- sp + annotate("text", x = 1.00, y = 99, label = equation(reg), parse = TRUE, color = "Blue", size=5)
s


### Residual plots
sobrevivência.res = resid(reg)
plot(Biomassa$baixa.mil, sobrevivência.res, ylab="Residuals", xlab="baixa.mil", main="Sobrevivência") 
abline(0, 0, col = "red")       

