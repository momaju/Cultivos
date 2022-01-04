# Primeira Biometria


library(tidyverse)
library(googlesheets4)
library(ggthemes)

Biom <- read_sheet("1KkLM7bz-Az-etHUeENou-BjX4mDUfJCccwcCIo0k0CU", 2)



# transformando algumas variáveis em fatores

Biom$viveiro = factor(Biom$viveiro, levels = c(1,2,3,4), labels = c("V1","V2","V3","V4"))

Biom$ciclo = factor(Biom$ciclo,levels = c(1,2,3,4,5,6,7,8,9,10,11), labels = c("C1","C2","C3","C4","C5","C6","C7","C8","C9","C10","C11"))

Biom$ano = factor(Biom$ano)
Biom$bionutri = factor(Biom$bionutri)

str(Biom)

# boxplot
theme_set(theme_bw())
u <- Biom %>% 
  ggplot(aes(x=viveiro, y=biometria_1, color = viveiro))
u + geom_boxplot() +
  theme(legend.position = "none")

u1 <- u +geom_jitter() + geom_boxplot(alpha=0.5, size=1.1)
u1

# Non data ink

u1 + labs(title = "Primeira Biometria",
           subtitle = "2015-2020",
           caption = "Fonte: Azul Marinho",
           x = "Viveiro", y = "Peso (g)")

# export the chart: width = 1280, height = 720