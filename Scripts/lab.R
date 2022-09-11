### Hatchery Performance

library(tidyverse)
library(googlesheets4)
library(ggthemes)
library(lubridate)
library(ggrepel)
library(scales)


biom <- read_sheet("1KkLM7bz-Az-etHUeENou-BjX4mDUfJCccwcCIo0k0CU", 2)


# General Performance -----------------------------------------------------

lab_desempenho <- biom %>% 
  group_by(lab) %>% 
  summarise(Pls_compradas = sum(pop),
            Sobrevivência = mean(sobrevive),
            Densidade = mean(densidade),
            Dias_de_cultivo = mean(ddc),
            Peso_final = mean(g_final, na.rm = TRUE),
            Crescimento = mean(g_semana),
            Produtividade = mean(produtividade),
            Conversão = mean(tca),
            Biometria_1 = mean(biometria_1),
            Id_entrada = mean(id_entrada))


# Desempenho por viveiro e ciclo de cultivo -------------------------------

biom %>% 
  group_by(lab, viveiro) %>%
  summarise(ciclos = n())


# Ciclos por lab ----------------------------------------------------------

biom %>% 
  group_by(lab) %>%
  summarise(ciclo = n())
