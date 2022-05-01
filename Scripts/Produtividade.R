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




a <- biom %>%
ggplot(aes(x = densidade, y = produtividade)) +
geom_point() +
geom_smooth(method = lm, se = FALSE) +
labs(title = "Produtividade em Função da Densidade de Povoamento",
         subtitle = "",
         caption = "Fonte: Azul Marinho Aquicultura",
         x = "Povoamento\n(Cmarões/mq.)",
         y = "Produtividade (kg/ha)") +
  theme_minimal() +
  theme(plot.caption = element_text(size = 8, color = "grey60", hjust = 1),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.title.y = element_text(size = 15, color = "black"),
        axis.title.x = element_text(size = 15, color = "black"),
        plot.margin = margin(25, 25, 25, 25)) +
scale_x_continuous(expand = c(0, 0), limits = c(0, 30))
a

# Regressão--------------------------------------------

fit <- biom %>%
lm(produtividade ~ densidade, data = .)
summary(fit)

# Inserting the equation---------------------------------

equation <- function(fit) {
  lm_coef <- list(a = round(coef(fit)[1], digits = 2),
                  b = round(coef(fit)[2], digits = 2),
                  r2 = round(summary(fit)$r.squared, digits = 2));
  lm_eq <- substitute(italic(y) == a + b %.% italic(x)
  * ","~~italic(R)^2~"="~r2, lm_coef)
  as.character(as.expression(lm_eq));
}

b <-  a + annotate("text", x = 20, y = 450.00,
label = equation(fit), parse = TRUE, color = "blue")
b
b + theme_economist()
