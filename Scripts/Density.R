
library(tidyverse)
library(googlesheets4)
library(scales)

Biom <- read_sheet("1KkLM7bz-Az-etHUeENou-BjX4mDUfJCccwcCIo0k0CU", 2)

glimpse(Biom)



# Densidade Primeira Biometria --------------------------------------------


Biom %>% 
  ggplot(aes(x = biometria_1, 
             fill = factor(viveiro))) +
  scale_x_continuous(breaks = seq(1,6,1),
                     limits = c(0,7))+
  geom_density(alpha = 0.4) +
  #facet_wrap(~ viveiro)+
  labs(fill = "Viveiros")


# Densidade Sobrevivência -------------------------------------------------


Biom %>% 
  ggplot(aes(x = sobrevive, 
             fill = factor(viveiro))) +
  scale_x_continuous(breaks = seq(20,300,20),
                     limits = c(0,300))+
 # scale_y_continuous(labels = percent_format())+
  scale_fill_manual(values = c("#0054FF", "#FFCC00", "#00FF00", "#FFDC91"))+
  geom_density(alpha = 0.4) +
  theme_classic()+
  labs(fill = "Viveiros")




