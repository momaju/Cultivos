Biomassa <- read.csv(file = "Cultivo - Biomassa.csv", header = T)
head(Biomassa)

# transformando algumas variáveis em fatores
Biomassa$viveiro = factor(Biomassa$viveiro, levels = c(1,2,3,4), labels = c("V1","V2","V3","V4"))
Biomassa$ciclo = factor(Biomassa$ciclo,levels = c(1,2,3,4,5,6,7,8,9,10,11), labels = c("C1","C2","C3","C4","C5","C6","C7","C8","C9","C10", "C11"))
Biomassa$ano = factor(Biomassa$ano)
Biomassa$bionutri = factor(Biomassa$bionutri)

library(ggplot2)
ggnames(Biomassa)

p <- ggplot(Biomassa, aes(x = biometria.1))
p
p + geom_histogram(color = "black", fill = "white", bins = 9, binwidth = 0.5) + geom_vline(aes(xintercept = mean(biometria.1)), color = "blue", linetype = "dashed", size =1)

p <- p + geom_histogram(color = "darkblue", fill = "lightblue", bins = 9, binwidth = 0.5) + geom_vline(aes(xintercept = mean(biometria.1)), color = "red", linetype = "dashed", size =1)
p

p <- p + geom_histogram(color = "darkblue", fill = "lightblue", bins = 9, binwidth = 0.5) + geom_vline(aes(xintercept = mean(biometria.1)), color = "red", linetype = "dashed", size =1) + labs(title = "Histograma da Primeira Biometria", x = "Peso (g)", y = "Quantidade", caption = "Azul Marinho") + theme_classic()
p

# Incluindo texto
sp <- p + annotate(geom="text", x=3.6, y=10, label="Média = 3.16075 g", color="red")
sp
