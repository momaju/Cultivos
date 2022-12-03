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
            Sobrevive = round(mean(sobrevive),2),
            Densidade = round(mean(densidade),2),
            Dias_de_cultivo = round(mean(ddc),2),
            Peso_final = round(mean(g_final, na.rm = TRUE),2),
            Crescimento = round(mean(g_semana),2),
            Produtivid. = round(mean(produtividade),2),
            Conversão = round(mean(tca),2),
            Biometria_1 = round(mean(biometria_1),2),
            Id_entrada = round(mean(id_entrada),2)) 

lab_desempenho


# Tabela: conjunto dos dois laboratórios ----------------------------------

# Utilizando library(gt)



lab_desempenho <- biom %>% 
  group_by(lab) %>% 
  summarise(Pls_compradas = sum(pop),
            Sobrevive = round(mean(sobrevive),2),
            Densidade = round(mean(densidade),2),
            Dias_de_cultivo = round(mean(ddc),2),
            Peso_final = round(mean(g_final, na.rm = TRUE),2),
            Crescimento = round(mean(g_semana),2),
            Produtivid. = round(mean(produtividade),2),
            Conversão = round(mean(tca),2),
            Biometria_1 = round(mean(biometria_1),2),
            Id_entrada = round(mean(id_entrada),2)) 


lab_desempenho %>% 
  gt() %>% 
  cols_label(
    #viveiro = "Viveiro",
    Pls_compradas = "Total PLs",
    Sobrevive = "Sobrevive.",
    Dias_de_cultivo = "Dias",
    Peso_final = "Peso(g)"
  ) %>%
  cols_width(
   # viveiro ~ px(40),
    Sobrevive ~ px(110),
    Peso_final ~ px(80),
    Crescimento ~ px(75),
    Produtivid. ~ px(185),
    Conversão ~ px(90),
    everything() ~ px(100)) %>% 
  cols_align(
    align = "right",
    columns = everything()
  ) %>% 
  fmt_number(columns = c(Pls_compradas:Id_entrada), dec_mark = ",",
             sep_mark = ".") %>%
  tab_header(title = md("**Aquacrusta vs Tijuca**")) %>% 
  summary_rows(
    groups = NULL,
    columns = Pls_compradas,
    fns = list(
      Total = ~sum(., na.rm = TRUE)), dec_mark = ",",sep_mark = ".") %>% 
  summary_rows(
    groups = NULL,
    columns = c(Sobrevive:Id_entrada),
    fns = list(
      Média = ~mean(., na.rm = TRUE)), dec_mark = ",",sep_mark = ".") %>% 
  tab_options(
    summary_row.background.color = "#ACEACE80",
    grand_summary_row.background.color = "#000080",
    row_group.background.color = "#FFEFDB80",
    heading.background.color = "#ffffff",
    column_labels.background.color = "#8080c0",
    stub.background.color = "#ffffff",
    table.font.color = "#000080",
    table_body.hlines.color = "#000080",
    table_body.border.top.color = "#989898",
    heading.border.bottom.color = "#989898",
    row_group.border.top.color = "#989898",
    row_group.border.bottom.style = "none",
    stub.border.style = "line",
    stub.border.color = "#000080",
    stub.border.width = "1px",
    summary_row.border.color = "#989898",
    table.width = "100%"
  )


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

library(flextable) 


lab_table <- flextable(lab_desempenho) %>% 
  colformat_double(., j = c(3:11), digits = 2) %>% 
  bg(., i= ~ lab == "AQC", part = "body", bg = "#7CADD2") %>% 
  bg(., i= ~ lab == "TIJ", part = "body", bg = "#f7ce00") %>% 
  bold(i = 1, bold = TRUE, part = "header")

lab_table  



 


# Resultado Tijuca --------------------------------------------------------
# Tabelas utilizando o pacote kableExtra



library(kableExtra)

tij_desempenho <- biom %>% 
  filter(lab == "TIJ" & ciclo == 32) %>% 
  group_by(viveiro) %>% 
  summarise(Pls_compradas = sum(pop),
            Sobrevive = mean(sobrevive),
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



# Todos cultivos Tijuca (kableExtra)  --------------------------------------


tij_desempenho_all <- biom %>% 
  filter(lab == "TIJ") %>% 
  group_by(viveiro) %>% 
  summarise(Pls_compradas = sum(pop),
            Sobrevive = round(mean(sobrevive),2),
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



# Using gt package --------------------------------------------------------

library(gt)

tij_desempenho_all <- biom %>% 
  filter(lab == "TIJ") %>% 
  group_by(viveiro) %>% 
  summarise(Pls_compradas = sum(pop),
            Sobrevive = round(mean(sobrevive),2),
            Densidade = round(mean(densidade),2),
            Dias_de_cultivo = round(mean(ddc),2),
            Peso_final = round(mean(g_final, na.rm = TRUE),2),
            Crescimento = round(mean(g_semana),2),
            Produtividade = round(mean(produtividade),2),
            Conversão = round(mean(tca),2),
            Biometria_1 = round(mean(biometria_1),2),
            Id_entrada = round(mean(id_entrada),2)) 


tij_desempenho_all %>% 
  gt() %>% 
  fmt_number(columns = c(Pls_compradas:Id_entrada), dec_mark = ",",
             sep_mark = ".") %>%
  tab_header(title = "Resultado dos Cultivos com PLs Tijuca") %>% 
  summary_rows(
    groups = NULL,
    columns = Pls_compradas,
    fns = list(
      Total = ~sum(., na.rm = TRUE)), dec_mark = ",",sep_mark = ".") %>% 
  summary_rows(
    groups = NULL,
    columns = c(Sobrevive:Id_entrada),
    fns = list(
      Média = ~mean(., na.rm = TRUE)), dec_mark = ",",sep_mark = ".")
  


# Todos os cultivos Aquacrusta --------------------------------------------

library(gt)

aqc_desempenho_all <- biom %>% 
  filter(lab == "AQC") %>% 
  group_by(viveiro) %>% 
  summarise(Pls_compradas = trunc(sum(pop)),
            Sobrevive = round(mean(sobrevive),2),
            Densidade = round(mean(densidade),2),
            Dias_de_cultivo = round(mean(ddc),2),
            Peso_final = round(mean(g_final, na.rm = TRUE),2),
            Crescimento = round(mean(g_semana),2),
            Produtividade = round(mean(produtividade),2),
            Conversão = round(mean(tca),2),
            Biometria_1 = round(mean(biometria_1),2),
            Id_entrada = round(mean(id_entrada),2)) 



aqc_desempenho_all %>% 
  gt() %>% 
  cols_label(
    viveiro = "Viveiro",
    Pls_compradas = "Total PLs",
    Sobrevive = "Sobrevive.",
    Dias_de_cultivo = "Dias",
    Peso_final = "Peso(g)"
  ) %>%
  cols_width(
    viveiro ~ px(40),
    Sobrevive ~ px(110),
    Peso_final ~ px(60),
    Crescimento ~ px(100),
    Produtividade ~ px(135),
    everything() ~ px(80)) %>% 
  cols_align(
    align = "right",
    columns = everything()
  ) %>% 
  fmt_number(columns = c(Pls_compradas:Id_entrada), dec_mark = ",",
             sep_mark = ".") %>%
  tab_header(title = md("**Resultado dos Cultivos com PLs Aquacrusta**")) %>% 
  summary_rows(
    groups = NULL,
    columns = Pls_compradas,
    fns = list(
      Total = ~sum(., na.rm = TRUE)), dec_mark = ",",sep_mark = ".") %>% 
  summary_rows(
      groups = NULL,
      columns = c(Sobrevive:Id_entrada),
      fns = list(
      Média = ~mean(., na.rm = TRUE)), dec_mark = ",",sep_mark = ".") %>% 
  tab_options(
    summary_row.background.color = "#ACEACE80",
    grand_summary_row.background.color = "#000080",
    row_group.background.color = "#FFEFDB80",
    heading.background.color = "#ffffff",
    column_labels.background.color = "#8080c0",
    stub.background.color = "#ffffff",
    table.font.color = "#000080",
    table_body.hlines.color = "#000080",
    table_body.border.top.color = "#989898",
    heading.border.bottom.color = "#989898",
    row_group.border.top.color = "#989898",
    row_group.border.bottom.style = "none",
    stub.border.style = "line",
    stub.border.color = "#000080",
    stub.border.width = "1px",
    summary_row.border.color = "#989898",
    table.width = "100%"
  )

                