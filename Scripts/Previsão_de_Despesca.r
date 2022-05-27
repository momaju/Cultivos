library(tidyverse)
library(googlesheets4)
library(ggthemes)
library(lubridate)
library(scales)
library(corrplot)
library(RColorBrewer)

biom <- read_sheet("1KkLM7bz-Az-etHUeENou-BjX4mDUfJCccwcCIo0k0CU", 2)

biom_numeric <- biom %>%
  na.omit() %>%
  select_if(is.numeric)

  View(biom_numeric)

cor(biom_numeric[, 3:19]) %>%
  corrplot()

# Previsão de Despesca ---------------------------------------------------

biom %>%
lm(biom_real ~ g_final + sobrevive + pop + ddc +
                 densidade + baixa_mil, data = .) %>%
summary()


## Função de previsão-----------------------------------------------------------

previsto <- function(g, sobrevive, pop, ddc, dens, baixa) {
    biom %>%
    lm(biom_real ~ g_final + sobrevive + pop + ddc +
         densidade + baixa_mil, data = .) %>%
    predict(data.frame(g_final = c(g),
                      sobrevive = c(sobrevive),
                       pop = c(pop),
                       ddc = c(ddc),
                       densidade = c(dens),
                       baixa_mil = c(baixa))) %>%
    round(2)
}
## Para um viveiro--------------------------------------------------------------

previsto(8.81, 73.34, 470000, 60, 12.05, 0.54)
## Para dois viveiros-----------------------------------------------------------

previsto(c(8.5, 8.0),
        c(3075, 2498),
        c(470000, 350000),
        c(60, 60),
        c(12.05, 11.99),
        c(0.55, 0.03))

## Regressão para um único viveiro-------------------------------------------

biom_v2 <- biom %>%
  na.omit() %>%
  filter(viveiro == 2)
biom_v2

fit_biom_v2 <- biom_v2 %>%
lm(biom_real ~ biom_calc + pop + ddc +
                    densidade + baixa_mil, data = .)
summary(fit_biom_v2)
