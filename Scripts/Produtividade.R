# Azul Marinho: Produtividade em kg/ha vs Densidade do povoamento em numero de camarões por metro quadrado. Os dados referem-se aos 10 ciclos de cultivo (2015 ~ 2017).

biomassa <- read.csv(file = "Cultivo - biomassa.csv", header = T)

head(Biomassa)

Biomassa$viveiro = factor(Biomassa$viveiro, levels = c(1,2,3,4), labels = c("V1","V2","V3","V4"))

Biomassa$ciclo = factor(Biomassa$ciclo,levels = c(1,2,3,4,5,6,7,8,9,10), labels = c("C1","C2","C3","C4","C5","C6","C7","C8","C9","C10"))

Biomassa$ano = factor(Biomassa$ano)
Biomassa$bionutri = factor(Biomassa$bionutri)

str(Biomassa)



library(ggplot2)
library(ggthemes)

a <- ggplot(data = biom, aes(x = densidade, y = produtividade)) + geom_point(shape = 1)

a <- a + geom_smooth(method = lm, se = FALSE)
a
# Títulos

a <- a + xlab("Densidade") + ylab("Produtividade kg/ha") + ggtitle("Azul Marinho\nProdutividade vs Densidade") 
  #caption("2015-2017")
a

# Regressão
fit <- lm(produtividade ~ densidade, data = biom)
summary(fit)

# Inserting the equation

equation = function(fit) {
  lm_coef <- list(a = round(coef(fit)[1], digits = 2),
                  b = round(coef(fit)[2], digits = 2),
                  r2 = round(summary(fit)$r.squared, digits = 2));
  lm_eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(R)^2~"="~r2,lm_coef)
  as.character(as.expression(lm_eq));                 
}

b <-  a + annotate("text", x = 20, y = 450.00, label = equation(fit), parse = TRUE, color = "Blue", size=6)
b
b + theme_economist()








