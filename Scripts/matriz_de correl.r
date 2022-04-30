# Matriz de correlção

library(tidyverse)
library(googlesheets4)
library(ggthemes)
library(lubridate)
library(ggrepel)
library(scales)
library(corrplot)
library(RColorBrewer)

biom <- read_sheet("1KkLM7bz-Az-etHUeENou-BjX4mDUfJCccwcCIo0k0CU", 2)

View(biom)

biom_numeric <- biom %>%
  na.omit() %>%
  select_if(is.numeric)

cor(biom_numeric[, 3:19]) %>%
  corrplot()

cor_matrix <- biom %>%
  na.omit() %>%
  select_if(is.numeric) %>%
  cor()

cor_matrix

cor.test(biom$biom_calc, biom$biom_real,
         method = "pearson", use = "complete.obs")

cor.test(biom$produtividade, biom$sobrevive,
         method = "pearson", use = "complete.obs")

 
cor(biom_numeric[, 1:9]) %>%
  corrplot(method = "color",
           type = "lower",
           order = "alphabet",
           diag = FALSE,
           addCoef.col = "black",
           tl.col = "blue",
           tl.srt = 45,
           number.cex = 0.9,
           addgrid.col = "black",
           title = "Matrix de Correlação \nde Produção de Camarão Marinho",
           mar = c(0, 0, 5, 0),
           #col = COL2('PiYG')),
           #col = COL2('PuOr', 10)),
           #col = COL1('YlGn'),
           #col = brewer.pal(n=8, name="Paired"),
           col = brewer.pal(n = 8, name = "RdYlBu"))
## Estimativa de biomassa real ---------------------------------------------
## Cálculo para toda a fazenda. Melhor usar este e
#acrecenta mais 15% ao resultado.

fit_biom <- lm(biom_real ~ biom_calc + pop + ddc +
                 densidade + baixa_mil, data = biom)
summary(fit_biom)

biom_v2 <- biom %>%
  na.omit() %>%
  filter(viveiro == 2)

## Mesmo cálculo para o viveiro 2

fit_biom_v2 <- lm(biom_real ~ biom_calc + pop + ddc +
                    densidade + baixa_mil, data = biom_v2)
summary(fit_biom_v2)
