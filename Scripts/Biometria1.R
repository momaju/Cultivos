
# Boxplot Primeira biometria ----------------------------------------------

library(tidyverse)
library(googlesheets4)
library(ggthemes)

biom <- read_sheet("1KkLM7bz-Az-etHUeENou-BjX4mDUfJCccwcCIo0k0CU", 2)


biom %>%
  mutate(viveiro = factor(viveiro),
         ciclo = factor(ciclo)) %>%
  ggplot(aes(x = viveiro, y = biometria_1, color = viveiro)) +
  geom_boxplot(size = 1) +
  geom_jitter(width = 0.25, alpha = 0.5) +
  stat_summary(fun = mean, geom = "point",
               shape = 20, size = 4, color = "blue") +
  theme_set(theme_bw()) +
  labs(title = "Primeira Biometria (g)",
      subtitle = "Showing The Mean (blue dot)",
       y = "Peso (g)",
       x = "Viveiro",
       caption = "Fonte: Azul Marinho Aquicultura") +
       theme_light() +
  theme(legend.position = "none",
        plot.title = element_text(size = 20, color = "black"),
        plot.caption = element_text(size = 9, color = "grey60"),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.title.y = element_text(size = 15, color = "black"),
        axis.title.x = element_text(size = 15, color = "black")) +
  scale_color_brewer(palette = "Dark2")

## Histogram-------------------------------------

p <- biom %>%
    ggplot(aes(biometria_1)) +
    geom_histogram(color = "black", fill = "white", bins = 9,
                    binwidth = 0.5) +
    geom_vline(aes(xintercept = mean(biometria_1)),
                color = "blue", linetype = "dashed", size = 1)

p

# Incluindo texto
sp <- p + annotate(geom = "text", x = 3.6, y = 25,
                 label = "Média = 3.16075 g", color = "red")
sp



# export the chart: width = 1280, height = 720
