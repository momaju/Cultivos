# Crescimento dos viveros da Azul Marinho em relação à densidade de povoamento.

Biomassa <- read.csv(file = "Cultivo - Biomassa.csv", header = T)

# transformando algumas variáveis em fatores

Biomassa$viveiro = factor(Biomassa$viveiro, levels = c(1,2,3,4), labels = c("V1","V2","V3","V4"))

Biomassa$ciclo = factor(Biomassa$ciclo,levels = c(1,2,3,4,5,6,7,8,9,10), labels = c("C1","C2","C3","C4","C5","C6","C7","C8","C9","C10"))

Biomassa$ano = factor(Biomassa$ano)

Biomassa$bionutri = factor(Biomassa$bionutri)


head(Biomassa)
summary(Biomassa)
str(Biomassa)

library(ggplot2)

library(ggthemes)


# Regressão

fit <- lm(g.semana ~ densidade, data=Biomassa)
summary(fit) # show results

# Gráfico sem agrupar por viveiro:

sp <- ggplot(data = Biomassa, aes(x = densidade, y = g.semana)) + geom_point(shape =1, size = 2) + geom_smooth(method = lm, se = F)

sp

# Inserindo a equação da regressão:


equation = function(fit) {
  lm_coef <- list(a = round(coef(fit)[1], digits = 2),
                  b = round(coef(fit)[2], digits = 2),
                  r2 = round(summary(fit)$r.squared, digits = 2));
  lm_eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(R)^2~"="~r2,lm_coef)
  as.character(as.expression(lm_eq));                 
}


sp1 <- sp + annotate("text", x = 20.00, y = 1.30, label = equation(fit), parse = TRUE, color = "red", size = 7)
sp1

# Titles

# aplicando o theme desde o início 
theme_set(theme_bw())
sp1 <- sp1 +
xlab("Densidade") +
ylab("g/semana") +
ggtitle("Crescimento Semanal", subtitle = "2015-2017")

sp1

# Se quiser um teme personalizado:

sp1 + theme(
  axis.title.x = element_text(color = "Orangered", size= 20),
  axis.title.y = element_text(color = "Orangered", size =20),
  axis.text.x = element_text(size =12),
  axis.text.y = element_text(size =12),
  
  legend.title = element_text(size = 15),
  legend.text = element_text(size = 12),
  plot.caption = element_text(color = "Blue3", size = 8),
  plot.title = element_text(color = "Blue3",
                            size = 20,
                            family = "Courier"),
  plot.subtitle = element_text(color = "Blue3", size = 10))


# Outro método para os títulos
theme_set(theme_bw()) # aplicando este theme
sp1 + labs(title = "Crescimento Semanal",
           subtitle = "2015-2017",
           caption = "Azul Marinho",
           x = "Densidade", y = "g/semana")
# Neste caso, o caption permanece. Mesmo aplicando-se o theme

# Personalizando o theme:
sp1 + theme(
  axis.title.x = element_text(color = "Orangered", size= 20),
  axis.title.y = element_text(color = "Orangered", size =20),
  axis.text.x = element_text(size =12),
  axis.text.y = element_text(size =12),
  
  legend.title = element_text(size = 15),
  legend.text = element_text(size = 12),
  plot.caption = element_text(color = "Blue3", size = 8),
  plot.title = element_text(color = "Blue3",
                            size = 20,
                            family = "Courier"),
  plot.subtitle = element_text(color = "Red3", size = 10))

# neste caso, depois de aplicado o theme, desaparece o caption.


