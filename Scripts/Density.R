
library(tidyverse)
library(googlesheets4)
library(ggthemes)
library(lubridate)
library(ggrepel)
library(scales)


biom <- read_sheet("1KkLM7bz-Az-etHUeENou-BjX4mDUfJCccwcCIo0k0CU", 2)

glimpse(biom)



# Densidade Primeira Biometria --------------------------------------------


biom %>%
  ggplot(aes(x = biometria_1,
             fill = factor(viveiro))) +
  scale_x_continuous(breaks = seq(1, 6, 1),
                     limits = c(0, 7)) +
  geom_density(alpha = 0.4) +
  #facet_wrap(~ viveiro) +
  labs(title = "Frequência de Peso na Primeira Biometria",
       y = "Frequência",
       x = "Primeira Biometria (g)",
       caption = "Fonte: Azul Marinho Aquicultura",
       fill = "Viveiro") +
  theme_minimal() +
  theme(plot.caption = element_text(size = 8, color = "grey60", hjust = 1))


# Densidade Sobrevivência -------------------------------------------------


biom %>%
  ggplot(aes(x = sobrevive,
             fill = factor(viveiro))) +
  scale_x_continuous(breaks = seq(20, 300, 20),
                     limits = c(0, 300)) +
 # scale_y_continuous(labels = percent_format())+
  scale_fill_manual(values = c("#0054FF", "#FFCC00", "#00FF00", "#FFDC91")) +
  geom_density(alpha = 0.4) +
  labs(title = "Frequência de Sobrevivência",
       y = "Frequência",
       x = "Sobrevivência (%)",
       caption = "Fonte: Azul Marinho Aquicultura",
       fill = "viveiro") +
  theme_minimal() +
  theme(plot.caption = element_text(size = 8, color = "grey60", hjust = 1))
