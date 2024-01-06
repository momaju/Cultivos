# setwd("~/Google Drive/RWork/Azul Marinho")
# Esta é uma análise da mortalida semanal dos viveiros de camarão da Azul Marinho. Toda quinta feira, dia de biometria é feito um levantamento da quantidade de camarões mortos encontraodos nas bandejas de alimentação de cada viveiro. São uma média de 20 bandejas por hectare. Os dados estão no arquivo Cultivo - Mortalidade.csv e referem-se a 10 ciclos de cultivo.

Mortalidade = read.csv(file = "Cultivo - Mortalidade.csv", header = T)
head(Mortalidade)
tail(Mortalidade)
str(Mortalidade)
summary(Mortalidade)

# transformando algumas variáveis em fatores

Mortalidade$viveiro = factor(Mortalidade$viveiro, levels = c(1,2,3,4), labels = c("V1","V2","V3","V4"))

Mortalidade$ciclo = factor(Mortalidade$ciclo,levels = c(1,2,3,4,5,6,7,8,9,10), labels = c("C1","C2","C3","C4","C5","C6","C7","C8","C9","C10"))

## Para os próximos passos, transformar a variaável semana em integer. ##


Mortalidade$semana = as.integer(Mortalidade$semana)

str(Mortalidade)  # paraverificar se deu certo.

# Gráficos com ggplot

library(ggplot2)

# Mortalidade semanal por viveiro.
m <- ggplot(data = Mortalidade, aes(x = semana, y = baixa.mil, color = viveiro)) +
  geom_point() + geom_smooth(fill = NA) 
m


# Mortalidade semanal por ciclo.
n <- ggplot(data = Mortalidade, aes(x = ciclo, y = baixa.mil, color = viveiro)) +
  geom_point() + geom_smooth(fill = NA)

n

# Linear regression

fit <- lm(baixa.mil ~ semana, data = Mortalidade)
summary(fit)

d <- ggplot(data = Mortalidade, aes(x = semana, y = baixa.mil)) +
  geom_point(shape = 1) + geom_smooth(method = lm, se = FALSE) 
d

# Nomeando os eixos

d <- d +
  xlab("Semana") + #x axis title
  ylab("Mortalidade/Milheiro") + #y axis title
  ggtitle("Azul Marinho\nMortalidade Semanal") #plot title
d

#Axes titles:
d1 <- d + theme(axis.title.x = element_text(colour = "Blue", size = 20),
         axis.title.y = element_text(colour = "Blue", size = 20),

#Axes texts:
axis.text.x = element_text(size = 10),
axis.text.y = element_text(size = 10))
d1


# -------------------- Outro modo de adicional títulos
# d <- d + scale_x_continuous(name = "Semana") +
#  scale_y_continuous(name = "Mortalidade/Milheiro")
# d

# Adding a title

# d <- d + ggtitle("Mortalidade Semanal")
# d

# Including regression coefficients

d <- d + annotate("text", x = 5.5, y = 0.4, label = "R^2=0.03785") +
  annotate("text", x = 5.5, y = 0.37, label = "alpha = 0.00") +
  annotate("text", x = 5.5, y = 0.34, label = "beta = 0.67")
d

# To make the coefficients more clear we will add some elements to increase visibility.


# Inserting the equation

equation = function(fit) {
  lm_coef <- list(a = round(coef(fit)[1], digits = 2),
                  b = round(coef(fit)[2], digits = 2),
                  r2 = round(summary(fit)$r.squared, digits = 2));
  lm_eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(R)^2~"="~r2,lm_coef)
  as.character(as.expression(lm_eq));                 
}

p11 <- ggplot(data = Mortalidade, aes(x = semana, y = baixa.mil, col)) +
  geom_point(color = "Blue", shape = 1) + geom_smooth(method = lm, se = FALSE) +
  
  ggtitle("Azul Marinho\nMortalidade Semanal") +
  scale_y_continuous(name = "Mortalidade/Milheiro") +
  scale_x_continuous(name = "Semana") +
 # annotate("rect", xmin = 5.5, xmax = 6.5, ymin = 0.3, ymax = 0.4, fill="white", colour="red") +
  annotate("text", x = 6.0, y = 0.35, label = equation(fit), parse = TRUE, color = "Blue", size = 6)

p11

# Theme

install.packages("ggthemes") # Install 
library(ggthemes) # Load

p11 +
  theme(text = element_text(family = "Chalkboard"),
        
        #Axes titles:
        axis.title.x = element_text(colour = "Red", size = 15),
        axis.title.y = element_text(colour = "Red", size = 15),
        
        #Axes texts:
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),  
        
        #Plot title:
        plot.title = element_text(colour = "Black",
                                  size = 20, hjust = 0.5)) 
         

 p11 + theme_bw()


# histogram

s <- ggplot(data = Mortalidade, aes(x = baixas)) +
  geom_histogram(binwidth = 10, aes(fill = viveiro), color = "Black")
s

# facets
s <- ggplot(data = Mortalidade, aes(x = baixas)) +
  geom_histogram(binwidth = 10, aes(fill = viveiro), color = "Black") +
  facet_grid(viveiro~.)
s

# boxplot por ciclo
t <- ggplot(data = Mortalidade, aes(x = ciclo, y = baixa.mil, color = ciclo))

t + geom_jitter() + geom_boxplot(alpha = 0.7) +
  xlab("Ciclo") + #x axis title
  ylab("Mortalidade/milheiro") + #y axis title
  ggtitle("Mortalidade Semanal por Ciclo") + #plot title
  theme(text = element_text(family = "Chalkboard"),
        
        #Axes titles:
        axis.title.x = element_text(colour = "Blue", size = 20),
        axis.title.y = element_text(colour = "Blue", size = 20),
        
        #Axes texts:
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        
        #Plot title:
        plot.title = element_text(colour = "Black",
                                  size = 25),
        
        #Legend title:
        legend.title = element_text(size = 12),
        
        #Legend text
        legend.text = element_text(size = 10)
  )


# boxplot por viveiro
p <- ggplot(data = Mortalidade, aes(x = viveiro, y = baixa.mil, color = viveiro))

p + geom_jitter() + geom_boxplot(alpha = 0.7) +
  xlab("Viveiro") + #x axis title
  ylab("Mortalidade/milheiro") + #y axis title
  ggtitle("Mortalidade Semanal por Viveiro") #plot title
  





# Gráfico de mortalidade por Semana (Semana como numérico)
plot(Mortalidade$semana, Mortalidade$baixa.mil, type  = "n", main = "", xlab = "", ylab = "", col = rainbow(10))
abline(h = c(0.00700,0.06775), col = "red", lty = 2, lwd = 2)
segments(x0 = c(8,12), y0 = c(0.00700,0.00700), x1 = c(8,12), y1 = c(0.06775,0.06775), col = "red", lty = 3, lwd = 2)

title(main = "Azul Marinho\nMortalidade Semanal")
title(xlab = 'Semana')
title(ylab = "Ind/mil")

# Gráfico de mortalidade por Viveiro

plot(Mortalidade$viveiro,Mortalidade$baixa.mil,type = "n",main = "",xlab = "",ylab = "",col = rainbow(4))
title(main = "Azul Marinho\nMortalidade Semanal por Viveiro")
title(xlab = "Viveiro")
title(ylab = "Ind/mil")

# Gráfico de mortalidade por Ciclo

plot(Mortalidade$ciclo,Mortalidade$baixa.mil, type = "n", main = "", xlab = "", ylab = "", col = rainbow(9))
title(main = "Azul Marinho\nMortalidade Semanal por Ciclo")
title(xlab = 'Ciclo')
title(ylab = "Ind/mil")


# Calculando a mortalidade média, máxima e mínima, por semana, para todos os viveiros em todos os ciclos. E armazenando o resultado nos objetos MeanSemanal, MaxSemanal e MinSemanal

MeanSemanal = tapply(Mortalidade$baixa.mil,Mortalidade$semana,mean)

MaxSemanal = tapply(Mortalidade$baixa.mil,Mortalidade$semana,max)

MinSemanal = tapply(Mortalidade$baixa.mil,Mortalidade$semana,min)

# Em seguida, transforme este objeto em um dataframe:

MeanSemanal = data.frame(MeanSemanal)

MaxSemanal = data.frame(MaxSemanal)

MinSemanal = data.frame(MinSemanal)

# Insira uma coluna para semana:

MeanSemanal$semana = c(5:15)

MaxSemanal$semana = c(5:15)

MinSemanal$semana = c(5:15)

# Ajuste os nomes das colunas:

names(MeanSemanal) = NULL

names(MaxSemanal) = NULL

names(MinSemanal) = NULL


names(MeanSemanal) = c("Baixa.mil", "semana")

names(MaxSemanal) = c("Baixa.mil", "semana")

names(MinSemanal) = c("Baixa.mil", "semana")



# Plotando o gráfico:

plot(MaxSemanal$semana,MaxSemanal$baixa.mil, type = "n",xlab = "", ylab = "", las = 1)

points(x = MaxSemanal$semana, y  = MaxSemanal$baixa.mil,pch = 17, col = "red")

plot(MeanSemanal$semana, MeanSemanal$baixa.mil, type = "n",xlab = "", ylab = "", las = 1)

points(x = MeanSemanal$semana, y = MeanSemanal$baixa.mil, pch = 15, col = "black")



plot(MinSemanal$semana,MinSemanal$baixa.mil, type = "n",xlab = "", ylab = "", las = 1)

points(x = MinSemanal$semana, y = MinSemanal$baixa.mil, pch = 25, col = "blue")


# Adicionando as linhas horizontais

abline(h = 0.01, lty = 2, col = "black")
# abline(h=0.02, lty=2, col= "black")
# abline(h=0.03, lty=2, col= "black")
# abline(h=0.04, lty=2, col= "black")
abline(h = 0.05, lty = 2, col = "black")
abline(h = 0.25, lty = 2, col = "black")
abline(h = 0.30, lty = 2, col = "black")
abline(h = 0.35, lty = 2, col = "black")
abline(h = 0.40, lty = 2, col = "black")
abline(h = 0.10, lty = 2, col = "black")
abline(h = 0.15, lty = 2, col = "black")
abline(h = 0.20, lty = 2, col = "black")

# Colocando os títulos

title(main = "Azul Marinho\n Mortalidade Semanal", col.main = "blue")
title(xlab = "Semana", ylab = "Mortalidade\n Ind/milheiro povoado")

# Adicionando uma linha ligando os pontos médios

lines(MeanSemanal$Semana, MeanSemanal$Baixa_Mil, lty = 4, lwd = 2)
lines(MaxSemanal$Semana, MaxSemanal$Baixa_Mil, lty = 4, col = "red", lwd = 2)
lines(MinSemanal$Semana, MinSemanal$Baixa_Mil, lty = 4, col = "blue", lwd = 2)

# Adicionando a legenda

legend("topleft",legend = c("Max","Med","Min"), pch = c(17,15,25), pt.cex = c(2,2,2,2), col = c("red", "black","blue"))



# Fazendo um subset de Mortalidade para o Viveiro 1, Viveiro 2, Viveiro 3 e Viveiro 4

Mortalidade$Semana = as.integer(Mortalidade$Semana)

MortalidadeV1 = Mortalidade[Mortalidade$Viveiro == "V1",]
MortalidadeV2 = Mortalidade[Mortalidade$Viveiro == "V2",]
MortalidadeV3 = Mortalidade[Mortalidade$Viveiro == "V3",]
MortalidadeV4 = Mortalidade[Mortalidade$Viveiro == "V4",]


# Calculando a mortalidade média por semana por Viveiro em todos os ciclos.

tapply(MortalidadeV1$Baixa_Mil,MortalidadeV1$Semana,mean)
tapply(MortalidadeV2$Baixa_Mil,MortalidadeV2$Semana,mean)
tapply(MortalidadeV3$Baixa_Mil,MortalidadeV3$Semana,mean)
tapply(MortalidadeV4$Baixa_Mil,MortalidadeV4$Semana,mean)



## gráfico com a função plot ##

plot(Mortalidade$Semana,Mortalidade$Baixa_Mil, type = "n",xlab = "", ylab = "")

summary(Mortalidade$Baixa_Mil)
abline(h = 0.00700, lty = 2, col = "red") # line 1st Qu
abline(h = 0.06775, lty = 2, col = "red") # line 3rd Qu
abline(h = 0.04733, lty = 2, col = "blue") # line Mean
points(x = Mortalidade$Semana, y = Mortalidade$Baixa_Mil) # insere os pontos

# insere pontos diferentes para cada Viveiro
points(Mortalidade$Semana[Mortalidade$Viveiro == "V1"], Mortalidade$Baixa_Mil[Mortalidade$Viveiro == "V1"], pch = 4, col = "darkmagenta", cex = 2) 

# points(Mortalidade$Semana[Mortalidade$Viveiro=="V1"],Mortalidade$Baixa_Mil[Mortalidade$Viveiro=="V1"], pch=10, cex=2)

points(Mortalidade$Semana[Mortalidade$Viveiro == "V2"], Mortalidade$Baixa_Mil[Mortalidade$Viveiro == "V2"], pch = 18, col = "darkblue", cex = 2)

# points(Mortalidade$Semana[Mortalidade$Viveiro=="V2"],Mortalidade$Baixa_Mil[Mortalidade$Viveiro=="V2"], pch=5, cex=2)

points(Mortalidade$Semana[Mortalidade$Viveiro == "V3"], Mortalidade$Baixa_Mil[Mortalidade$Viveiro == "V3"], pch = 2, col = "darkgreen", cex = 2)

# points(Mortalidade$Semana[Mortalidade$Viveiro=="V3"],Mortalidade$Baixa_Mil[Mortalidade$Viveiro=="V3"], pch=12, cex=2)

points(Mortalidade$Semana[Mortalidade$Viveiro == "V4"], Mortalidade$Baixa_Mil[Mortalidade$Viveiro == "V4"], pch = 8, col = "darkred", cex = 2)

# points(Mortalidade$Semana[Mortalidade$Viveiro=="V4"],Mortalidade$Baixa_Mil[Mortalidade$Viveiro=="V4"], pch=2, cex=2)

identify(Mortalidade$Semana,Mortalidade$Baixa_Mil,labels = Mortalidade$Viveiro) # identifica os pontos como Viveiros

# Adicionar legenda

legend("topleft", legend = c("V1","V2","V3","V4"), pch = c(4,18,2,8), col = c("darkmagenta", "darkblue", "darkgreen", "darkred") ,pt.cex = c(2,2,2,2))

# legend("topleft",legend=c("V1","V2","V3","V4"),pch=c(10,5,12,2),pt.cex=c(2,2,2,2))

# legend("topleft",legend=c("V1","V2","V3","V4"),pch=c(4,18,2,8),col=c("darkmagenta","darkblue","darkgreen","darkred"),pt.cex=c(2,2,2,2))

title(main = "Azul Marinho\n Mortalidade Semanal")
title(xlab = "Semana", ylab = "Mortalidade\n ind/milheiro povoado")

## Mortalidade Semanal por Viveiro ##

MaxViveiro = tapply(Mortalidade$Baixa_Mil,Mortalidade$Viveiro,max)


MeanViveiro = tapply(Mortalidade$Baixa_Mil,Mortalidade$Viveiro,mean)


MinViveiro = tapply(Mortalidade$Baixa_Mil,Mortalidade$Viveiro,min)


MaxViveiro = data.frame(MaxViveiro)

MeanViveiro = data.frame(MeanViveiro)

MinViveiro = data.frame(MinViveiro)


MaxViveiro$Viveiro = c("V1","V2","V3","V4")

MeanViveiro$Viveiro = c("V1","V2","V3","V4")

MinViveiro$Viveiro = c("V1","V2","V3","V4")


names(MaxViveiro) = NULL

names(MeanViveiro) = NULL

names(MinViveiro) = NULL



names(MaxViveiro) = c("Baixa_Mil", "Viveiro")

names(MeanViveiro) = c("Baixa_Mil", "Viveiro")

names(MinViveiro) = c("Baixa_Mil", "Viveiro")


plot(MaxViveiro$Viveiro,MaxViveiro$Baixa_Mil, type = "n",xlab = "", ylab = "")


points(x = MaxViveiro$Viveiro, y = MaxViveiro$Baixa_Mil, pch = 17, col = "red")

points(x = MeanViveiro$Viveiro, y = MeanViveiro$Baixa_Mil, pch = 15, col = "black")


points(x = MinViveiro$Viveiro, y = MinViveiro$Baixa_Mil, pch = 25, col = "blue")

### Não deu certo ainda
