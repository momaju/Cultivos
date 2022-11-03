### Hatchery Performance

library(tidyverse)
library(googlesheets4)
library(ggthemes)
library(lubridate)
library(ggrepel)
library(scales)
library(flextable)


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

lab_desempenho


# Desempenho por viveiro e ciclo de cultivo -------------------------------

biom %>% 
  group_by(lab, viveiro) %>%
  summarise(ciclos = n())


# Ciclos por lab ----------------------------------------------------------

biom %>% 
  group_by(lab) %>%
  summarise(ciclo = n())


# Percentage by group -----------------------------------------------------

biom %>% 
  count(lab) %>% 
  mutate(pct = n/sum(n)*100)

# Making tables -----------------------------------------------------------

lab_table <- flextable(lab_desempenho) %>% 
  colformat_double(., j = c(3:11), digits = 2) %>% 
  bg(., i= ~ lab == "AQC", part = "body", bg = "#7CADD2") %>% 
  bg(., i= ~ lab == "TIJ", part = "body", bg = "#FFFF6D") %>% 
  bold(i = 1, bold = TRUE, part = "header")

lab_table  



# Making table wider ------------------------------------------------------

# This did not work the way I wanted

lab_wide_table <- lab_desempenho %>% 
  pivot_wider(names_from = lab, values_from = c(Pls_compradas:Id_entrada)) %>%
  flextable() %>% 
  #colformat_double(., j = c(3:11), digits = 2) %>% 
  #bg(., i= ~ lab == "AQC", part = "body", bg = "#7CADD2") %>% 
  #bg(., i= ~ lab == "TIJ", part = "body", bg = "#FFFF6D") %>% 
  bold(i = 1, bold = TRUE, part = "header")
  
  
lab_wide_table  




