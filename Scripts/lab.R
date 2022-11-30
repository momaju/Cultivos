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
# use flextable package

lab_table <- flextable(lab_desempenho) %>% 
  colformat_double(., j = c(3:11), digits = 2) %>% 
  bg(., i= ~ lab == "AQC", part = "body", bg = "#7CADD2") %>% 
  bg(., i= ~ lab == "TIJ", part = "body", bg = "#f7ce00") %>% 
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


# Resultado Tijuca --------------------------------------------------------
# 
library(kableExtra)

tij_desempenho <- biom %>% 
  filter(lab == "TIJ" & ciclo == 32) %>% 
  group_by(viveiro) %>% 
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

kbl(tij_desempenho)

tij_desempenho %>%
  kbl(caption = "Resultado dos Últimos Cultivos") %>%
  kable_styling()


tij_desempenho %>%
  kbl(caption = "Resultado dos Últimos Cultivos") %>%
  kable_classic_2(html_font = "Cambria") %>% 
  kable_styling(bootstrap_options = "condensed",
                full_width = F,
                fixed_thead = T) %>% 
  footnote(general = "Data da despesca 16/11/2022")



# Todos cultivos Tijuca ---------------------------------------------------


tij_desempenho_all <- biom %>% 
  filter(lab == "TIJ") %>% 
  group_by(viveiro) %>% 
  summarise(Pls_compradas = sum(pop),
            Sobrevivência = round(mean(sobrevive),2),
            Densidade = round(mean(densidade),2),
            Dias_de_cultivo = round(mean(ddc),2),
            Peso_final = round(mean(g_final, na.rm = TRUE),2),
            Crescimento = round(mean(g_semana),2),
            Produtividade = round(mean(produtividade),2),
            Conversão = round(mean(tca),2),
            Biometria_1 = round(mean(biometria_1),2),
            Id_entrada = round(mean(id_entrada),2)) 

tij_desempenho_all %>% 
  kbl(caption = "Todos os Cultivos com Sua PL") %>%
  kable_classic_2(html_font = "Cambria") %>% 
  kable_styling(bootstrap_options = "condensed",
                full_width = F,
                fixed_thead = T)
  

                