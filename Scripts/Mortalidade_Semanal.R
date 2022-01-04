# Plotando o gráfico:

plot(MaxSemanal$Semana,MaxSemanal$Baixa_Mil, type = "n",xlab = "", ylab = "", las = 1)

points(x=MeanSemanal$Semana, y=MeanSemanal$Baixa_Mil,pch = 15, col= "black")


points(x=MaxSemanal$Semana, y=MaxSemanal$Baixa_Mil,pch = 17, col= "red")

points(x=MinSemanal$Semana, y=MinSemanal$Baixa_Mil,pch = 25, col= "blue")


# Adicionando as linhas horizontais

abline(h=0.01, lty=2, col= "black")
# abline(h=0.02, lty=2, col= "black")
# abline(h=0.03, lty=2, col= "black")
# abline(h=0.04, lty=2, col= "black")
abline(h=0.05, lty=2, col= "black")
abline(h=0.25, lty=2, col= "black")
abline(h=0.30, lty=2, col= "black")
abline(h=0.35, lty=2, col= "black")
abline(h=0.40, lty=2, col= "black")
abline(h=0.10, lty=2, col= "black")
abline(h=0.15, lty=2, col= "black")
abline(h=0.20, lty=2, col= "black")

# Colocando os títulos

title(main = "Azul Marinho\n Mortalidade Semanal", col.main = "blue")
title(xlab = "Semana", ylab = "Mortalidade\n Ind/milheiro povoado")

# Adicionando uma linha ligando os pontos médios

lines(MeanSemanal$Semana,MeanSemanal$Baixa_Mil,lty=4, lwd = 2)
lines(MaxSemanal$Semana,MaxSemanal$Baixa_Mil,lty=4, col = "red", lwd = 2)
lines(MinSemanal$Semana,MinSemanal$Baixa_Mil,lty=4, col = "blue", lwd =2)

# Adicionando a legenda

legend("topleft",legend=c("Max","Med","Min"),pch=c(17,15,25),pt.cex=c(2,2,2,2), col = c("red", "black","blue"))

