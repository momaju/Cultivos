library(tidyverse)
library(googlesheets4)
library(ggthemes)
library(lubridate)
library(scales)

Biom <- read_sheet("1KkLM7bz-Az-etHUeENou-BjX4mDUfJCccwcCIo0k0CU", 2)

Biom %>% 
  mutate(viveiro = factor(viveiro),
         mean_b1 = mean(biometria_1)) %>%
  select(viveiro, biometria_1) %>%
  ggplot(aes(viveiro, biometria_1))+
  geom_boxplot()+
  geom_jitter(alpha = 0.4, width = 0.25)+
  #geom_hline(yintercept = mean(Biom$biometria_1), colour = "#4C00FF") + 
  stat_summary(fun = mean, geom="point", 
               shape=20, size=4, color="blue") +
  labs(title = "Primeira Biometria",
  subtitle =  "Peso em g",
  y = "Peso na Primeira Biometria",
  x = "Viveiro")+
  theme_light()

