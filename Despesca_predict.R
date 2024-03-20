# Previsão de Despesca
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
  predict(data.frame(biom_calc = c(2700,0),
                     pop = c(470000,0),
                     ddc = c(52,0),
                     densidade = c(12.05,0),
                     baixa_mil = c(0.045,0))) %>%
  round(2)
