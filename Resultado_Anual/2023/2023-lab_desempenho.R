# Desempenho por fornecedor de PLs.
# 
# # gtsummary ---------------------------------------------------------------
 
library(tidyverse)
library(googlesheets4)
library(ggthemes)
library(lubridate)
library(ggrepel)
library(scales)
library(kableExtra)
library(gt)
library(flextable)
library(gtsummary)


biom <- read_sheet("1KkLM7bz-Az-etHUeENou-BjX4mDUfJCccwcCIo0k0CU", 2)

## A função abaixo serve para formatar os valores da tabela para o formato
# brasileiro onde o separador de milhares é o ponto e o de decimais é a
# vírgula. Vai ser utilizada na opção digits = do função tbl_summary.

number_style <- function(x)(style_number(x,
                                         digits = 2,
                                         scale = 1,
                                         big.mark = ".",
                                         decimal.mark = ","))


biom_lab_2023 <- biom %>% 
  mutate(ano_desp = factor(year(data_desp))) %>% 
  filter(ano_desp == "2023") %>%
  select(lab, 
         pop, 
         baixa_mil, 
         tca, 
         biometria_1, 
         fallow, 
         g_semana, 
         id_entrada, 
         g_final,
         sobrevive, 
         ddc) %>% 
  tbl_summary(by = lab,
              statistic = list(pop ~ "{sum}",
                               c(all_continuous(), -pop) ~ "{mean} ({sd})"),
              type = list(c(pop, fallow, id_entrada) ~ 'continuous'),
              missing = "no", # don't list missing data separately
              digits = all_continuous() ~ list(number_style),
              label = list(lab = "Lab", 
                           pop = "PLs Compradas", 
                           baixa_mil = "Mortalida/Milheiro", 
                           tca = "Conversão Alimentar", 
                           biometria_1 = "Primeira Biometria (g)",
                           fallow = "Dias Parados",
                           g_semana = "Crescimento Semanal (g)", 
                           id_entrada = "Id. Entrada (PL)", 
                           g_final = "Peso Final (g)",
                           sobrevive = "Sobrevivência", 
                           ddc = "Dias de Cultivo")) %>%
  
  modify_header(label ~ "**Variável**") %>% # update the column header
  modify_spanning_header(c("stat_1", "stat_2", "stat_3") ~ "**Lab**, N = {n} ({style_percent(p)}%)") %>%
  #modify_caption("**Desempenho por Laboratório**") %>%
  #dd_difference() #add column for difference between two group, 
  #confidence interval, and p-value
  #add_p() %>% 
  #add_p(pvalue_fun = ~ style_pvalue(.x, 
  #                                  digits = 3,
  #                                  decimal.mark = ",")) %>% # test for a difference between groups
  add_overall() %>% 
  #add_n() %>% 
  #add_significance_stars() %>% #Add significance stars
  #bold_p() %>%  #bold significant p-values
  as_gt() %>% #the summary table must first be converted into a gt object
  gt::tab_source_note(gt::md("*Azul Marinho Aquicultura*")) %>% 
  gt::tab_options(column_labels.background.color = "#8080c0",
                  table_body.hlines.color = "#000080",
                  table.font.color = "#000080") %>% 
  #gt::fmt_number(columns =  where(~ is.numeric(.x)), #não formata de acordo

  
    #              locale = "pt",
                # use_seps = TRUE,
  #               decimals = 3,
  #               dec_mark = ",",
  #              sep_mark = ".") %>% 
  gt::tab_header(
    title = md("**Desempenho por Larvicultura**"),
    subtitle = md("**2023**"))

biom_lab_2023         




