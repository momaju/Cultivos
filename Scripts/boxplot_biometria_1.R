library(tidyverse)
library(googlesheets4)
library(ggthemes)
library(lubridate)
library(scales)

Biom <- read_sheet("1KkLM7bz-Az-etHUeENou-BjX4mDUfJCccwcCIo0k0CU", 2)

Biom %>% 
  select(viveiro, biometria_1) %>% 
  ggplot(aes(factor(viveiro), biometria_1))+
  geom_boxplot()+
  labs(title = "Primeira Biometria",
  subtitle =  "Peso em g",
  y = "Peso na Primeira Biometria",
  x = "Viveiro")+
  theme_light()

