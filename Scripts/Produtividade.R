library(tidyverse)
library(googlesheets4)
library(ggthemes)
library(lubridate)
library(ggrepel)
library(scales)


biom <- read_sheet("1KkLM7bz-Az-etHUeENou-BjX4mDUfJCccwcCIo0k0CU", 2)

##Produtividade por ciclo de cultivo--------------------------------

produtividade_ciclo <- biom %>%
    #filter(densidade >= 10 & sobrevive >= 50) %>% # neste caso, o filtro opera
    #antes de summarize e não é o desejado
    group_by(ciclo) %>% # agrupa os dados
    mutate(ciclo = factor(ciclo)) %>%
    summarize(densidade = round(mean(densidade), 2), gramatura = mean(g_final),
              produção = sum(biom_real),
              produtividade = round(mean(produtividade), 2),
              sobrevive = round(mean(sobrevive), 2)) %>%
    #filter(densidade >= 5 & sobrevive >= 50) %>% # filter após summarize,
    # sai como quqero. Omitindo  esta linha, pega totos os ciclos.
    arrange(sobrevive)

produtividade_ciclo

# Produtividade por ciclo, destacando a sobrevivência ---------------------

produtividade_ciclo %>%
  ggplot(aes(ciclo, produtividade)) + # Cria um ggplot object
  geom_bar(stat = "identity", fill = "royalblue2") +  # Defines the geometry
  geom_hline(yintercept = 1000, linetype = "dashed", color = "#000000") +
  geom_text(aes(label = sobrevive), vjust = -1, color = "black", size = 3) +
  labs(title = "Produtividade por Ciclo",
         subtitle = "Destacando a Sobrevivência Média",
         caption = "Fonte: Azul Marinho Aquicultura",
         x = "Ciclo de Cultivo",
         y = "Produtividade (kg/ha)") +
  theme_minimal() +
  theme(plot.caption = element_text(size = 8, color = "grey60", hjust = 1))
  




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








