# Previs"ao de Despesca
# 
library(tidyverse)
library(googlesheets4)
library(ggthemes)
library(lubridate)
library(ggrepel)
library(scales)
library(corrplot)
library(RColorBrewer)

biom <- read_sheet("1KkLM7bz-Az-etHUeENou-BjX4mDUfJCccwcCIo0k0CU", 2)


# Previsão de Despesca ---------------------------------------------------
# Para dois viveiros.

biom %>%
  lm(biom_real ~ biom_calc + pop + ddc +
       densidade + baixa_mil, data = .) %>%
  predict(data.frame(biom_calc = c(3330,1850),
                     pop = c(470000,350000),
                     ddc = c(45,41),
                     densidade = c(12.05,11.99),
                     baixa_mil = c(0.03,0.02))) %>%
  round(2)
