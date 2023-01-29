library(tidyverse)
library(googlesheets4)
library(ggthemes)
library(lubridate)
library(ggrepel)
library(scales)

biom <- read_sheet("1KkLM7bz-Az-etHUeENou-BjX4mDUfJCccwcCIo0k0CU", 2)

desp_mensal <- biom %>%
  mutate(ano_desp = year(data_desp)) %>%
  mutate(mes_desp = month(data_desp, label = TRUE)) %>% 
  filter(mes_desp == "jan") %>% 
  group_by(mes_desp, ano_desp) %>% 
  summarise(kg = sum(biom_real, na.rm = TRUE), acumulado = cumsum(kg))
  
desp_mensal

