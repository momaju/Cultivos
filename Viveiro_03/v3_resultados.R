

library(tidyverse)
library(googlesheets4)
library(ggthemes)
library(lubridate)
library(ggrepel)
library(scales)
library(corrplot)
library(kableExtra)


biom <- read_sheet("1KkLM7bz-Az-etHUeENou-BjX4mDUfJCccwcCIo0k0CU", 2)


#Resumo --------------------------------------

v3_resumo <- biom %>%
  filter(viveiro == 3) %>%
  select(data_desp, lab, biom_real, densidade, sobrevive, g_semana, g_final, ddc, baixa_mil, tca) %>% 
  filter(densidade >= 12 & densidade <= 13)

v3_resumo %>% 
arrange(desc(tca)) %>% 
kable()
  



  