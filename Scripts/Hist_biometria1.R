library(tidyverse)
library(googlesheets4)
library(ggthemes)
library(lubridate)
library(ggrepel)
library(scales)
library(corrplot)

biom <- read_sheet("1KkLM7bz-Az-etHUeENou-BjX4mDUfJCccwcCIo0k0CU", 2)
View(biom)


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
