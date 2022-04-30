

library(tidyverse)
library(googlesheets4)
library(ggthemes)
library(lubridate)
library(ggrepel)
library(scales)
library(corrplot)


biom <- read_sheet("1KkLM7bz-Az-etHUeENou-BjX4mDUfJCccwcCIo0k0CU", 2)


str(biom)

summary(biom)





d <- biom %>%
  ggplot(aes(biom_calc, biom_real)) +
  geom_point(aes(shape = factor(viveiro)), size = 2.5) +
  geom_smooth(method = lm, se = FALSE) +
  labs(title = "Total Despescado (kg): Calculado vs Real",
       y = "Biomassa Real",
       x = "Biomassa Calculada",
       caption = "Fonte: Azul Marinho Aquicultura") +
  theme_minimal() +
  theme(plot.caption = element_text(size = 8, color = "grey60", hjust = 1)) +
  scale_shape_discrete(name = "Viveiro") # ajusta o t´tulo da legenda
  
d



# Regressão-----

fit_biom <- biom %>%
  lm(biom_real ~ biom_calc, data = .)

fit_biom

summary(fit_biom)

attributes(fit_biom)
fit_biom$residuals
hist(fit_biom$residuals)


# Regression Prediction ---------------------------------------------------

## Predicting for 2000 kg, 3000 kg and 5000 kg


biom %>%
  lm(biom_real ~ biom_calc, data = .) %>%
  predict(data.frame(biom_calc = c(2000, 3000, 5000))) %>%
  round(2)
  



lm_eqn <- function(fit_biom) {
  fit <- lm(biom_real ~ biom_calc, data = biom) # nolint
  eq <- substitute(italic(y) == a + b %.% italic(x) * ","~~italic(r)^2~"="~r2,
                   list(a = format(unname(coef(fit_biom)[1]), digits = 3),
                        b = format(unname(coef(fit_biom)[2]), digits = 3),
                        r2 = format(summary(fit_biom)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

d + geom_text(x = 1500.00, y = 4500.00, label = lm_eqn(fit_biom), parse = TRUE)

biom_mod <- lm(biom_real ~ biom_calc, data = biom)

summary(biom_mod)

# O mesmo cálculo para um único viveiro.

V4 <- biom %>%
  filter(viveiro == 4)

V4

p <- V4 %>%
  ggplot(aes(biom_calc, biom_real)) +
  geom_point(size = 2.5) +
  geom_smooth(method = lm, se = FALSE) +
  labs(title = "V04 -Total Despescado (kg) por ciclo: Calculado vs Real",
       y = "Biomassa Real",
       x = "Biomassa Calculada",
       caption = "Fonte: Azul Marinho Aquicultura") +
  theme_minimal() +
  theme(plot.caption = element_text(size = 8, color = "grey60")) +
  geom_text(
    aes(label = ciclo), nudge_y = 100)
  
p


# Escrevendo a equação da regressão ---------------------------------------



fit4 <- lm(biom_real ~ biom_calc, data = V4)
summary(fit4)

lm_eqn <- function(fit4) {
  fit <- lm(biom_real ~ biom_calc, data = V4) # nolint
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,  # nolint # nolint
                   list(a = format(unname(coef(fit4)[1]), digits = 3),
                        b = format(unname(coef(fit4)[2]), digits = 3),
                        r2 = format(summary(fit4)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

p + annotate("text", x = 1500.00,
             y = 4500.00, 
             label = lm_eqn(fit4), 
             parse = TRUE, color = "Blue", size=5)
  

# Escrevendo de outra maneira

p + geom_text(x = 1500.00, y = 4500.00, label = lm_eqn(fit4), parse = TRUE)



# Theme
library(ggthemes)
s + theme_economist()

# Gáfico de barras de densidade média por ano:-----

biom %>% 
  mutate(ano_desp = factor(year(data_desp))) %>% #extrai o ano da data 
  #de despesca
  group_by(ano_desp) %>%
  summarize(densidade_media = round(mean(densidade),2)) %>% 
  ggplot(aes(x=ano_desp, y=densidade_media, fill = ano_desp)) +
  geom_bar(stat="identity", width = 0.5,show.legend = FALSE) +
  labs(title = "Densidade no Povoamento",
       y = "Densidade Média",
       x = "Ano",
       caption = "Fonte: Azul Marinho Aquicultura")+
  theme_minimal() +
  theme(plot.caption = element_text(size = 7, color = "grey60")) +
  geom_text(aes(label = format(densidade_media, big.mark = ".", 
                               decimal.mark = ",")), 
            vjust=1.6, color="white", size=4.0)

# Gáfico de barras de produtividade média por ano:----

biom %>%
  mutate(ano_desp = factor(year(data_desp))) %>%
  group_by(ano_desp) %>%
  summarize(produtividade_media = round(mean(produtividade),2)) %>% 
  ggplot(aes(x=ano_desp, y=produtividade_media, fill = ano_desp)) +
  geom_bar(stat="identity",width = 0.5, show.legend = FALSE) + 
  labs(title = "Produtivide Média (kg/ha/ano)",
       y = "kg/Ha",
       x = "Ano",
       caption = "Fonte: Azul Marinho Aquicultura")+
  theme_minimal() +
  theme(plot.caption = element_text(size = 7, color = "grey60")) +
  geom_text(aes(label = format(produtividade_media, big.mark = ".", 
                               decimal.mark = ",")), 
            vjust=1.6, color="white", size=4.0)

  


# Boxplot Mortalidade Viveiro 1. Entenda-se como mortalidade, 
# a tottalidae de indivíduos encontrados mortos, nas bandejas de alimentação 
# durante todo o cultivo

V1 <- biom %>% 
  filter(viveiro == 1) %>% 
  mutate(ano_desp = factor(year(data_desp)))
V1




m1 <- V1 %>% 
  ggplot(aes(x = ano_desp, y = baixas_ac)) +
  geom_boxplot(aes(color = ano_desp)) + 
  geom_point() +
  labs(title = "V1: Mortalidade Acumulada",
       y = "Mortalidade\nAcumulada",
       x = "Ano",
       caption = "Fonte: Azul Marinho Aquicultura")+
  theme_minimal() +
  theme(plot.caption = element_text(size = 8, color = "grey60"),
        legend.position = "none") +
  coord_flip()
  
m1



# utilizando o tema economist:
m1 + theme_economist()

# utilizando tema personalozado:
m1 + theme(
           axis.title.x = element_text(color = "Orangered", size= 20),
           axis.title.y = element_text(color = "Orangered", size =20),
           axis.text.x = element_text(size =12),
           axis.text.y = element_text(size =12),
          
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 12),
          plot.title = element_text(color = "Blue3",
                          size = 20,
                          family = "Courier"))

# Código para várias correlações. Basta substituir as variáveis x e y para 
# obter-se a correlação desejada.

biom %>% 
ggplot(aes(x = g_final, y = sobrevive, 
           color = factor(viveiro),label =factor(ciclo))) + 
  geom_text_repel(max.overlaps = 100)+ #ficou meio feio. Ajustei com ggrepel
  geom_point(shape =1, size = 2) + 
  geom_smooth(method = lm, se = F, aes(group = 1))+
  scale_color_discrete(name = "Viveiro")+
  labs(title = "Peso Médio (g) vs. Sobrevivência (%)",
       subtitle = "Por Ciclo de Cultivo",
       caption = "Fonte:Azul Marinho Aquicultura",
       y = "Sobrevivência",
       x = "Peso Médio (g)") +
theme(plot.caption = element_text(size = 8, color = "grey60"))
      

biom %>% 
  mutate(viveiro = factor(viveiro)) %>% 
  ggplot(aes(x = densidade, y = produtividade, 
             color = viveiro, label = viveiro)) +
  geom_text_repel(max.overlaps = 100)+
  geom_point(size = 2) + 
  geom_smooth(method = lm, se = F, aes(group =1)) +
  expand_limits(y = 0, x = 0) +
  labs(title = "Produtividade vs. Densidade ",
       subtitle = "Por Viveiro",
       caption = "Fonte: Azul Marinho Aquicultura",
       y = "Produtividade (kg/ha)",
       x = "Densidade (camarões/mq)") +
  theme(plot.caption = element_text(size = 8, color = "grey60"))


# Regressões

fit <- lm(produtividade ~ densidade, data=biom)
summary(fit) # show results

fit <- lm(g_semana ~ densidade, data=biom)
summary(fit) # show results

# Gráfico sem agrupar por viveiro:

sp <- biom %>% 
  ggplot(aes(x = densidade, y = g_semana)) + 
  geom_point(shape =1, size = 2) + 
  geom_smooth(method = lm, se = F)

sp

# Inserindo a equação da regressão:

# Método 1
coeff = coefficients(fit)
eq = paste0("y =  ", round(coeff[2],3), "x +  ", round(coeff[1],3))


# Método 2
equation = function(lr) {
  lm_coef <- list(a = round(coef(lm)[1], digits = 2),
                  b = round(coef(lm)[2], digits = 2),
                  r2 = round(summary(lr)$r.squared, digits = 2));
  lm_eq <- substitute(
    italic(y) == a + b %.% italic(x)*","~~italic(R)^2~"="~r2,lm_coef)
  as.character(as.expression(lm_eq));                 
}


# Escrevendo no gráfico
sp + annotate("text", x = 20.00, y = 1.20, 
              label = equation(lm), parse = TRUE, color = "red", size = 7)


# Dias parados entre ciclos -----------------------------------------------


# Gráfico na horizontal ---------------------------------------------------



V4_fallow <- biom %>% 
  filter(viveiro == 4) %>% 
  mutate(ciclo = factor(ciclo))

V4_fallow %>% ggplot(aes(fallow, fct_reorder(ciclo, fallow),fill = ciclo)) +
  geom_col() +
  theme(legend.position = "none") +
  labs(title = "V4-Número de Dias Parados Entre Cultivos ",
       subtitle = "Por Ciclo",
       caption = "Fonte: Azul Marinho Aquicultura",
       y = "Ciclo de Cultivo",
       x = "Dias Parados") +
  theme(plot.caption = element_text(size = 8, color = "grey60")) +
  geom_text(aes(label = fallow, ), hjust = -0.5, color="#996035", size=4.0) +
  scale_fill_viridis_d(option = "viridis")


# Gráfico na vertical -----------------------------------------------------

  

V4_fallow <- biom %>% 
  filter(viveiro == 4) %>% 
  mutate(ciclo = factor(ciclo))

V4_fallow %>% ggplot(aes(ciclo, fallow, fill = ciclo)) + 
  geom_col() +
  theme(legend.position = "none") +
  labs(title = "V4-Número de Dias Parados Entre Cultivos ",
       subtitle = "Por Ciclo",
       caption = "Fonte: Azul Marinho Aquicultura",
       y = "Dias Parados",
       x = "Ciclo de Cultivo") +
  theme(plot.caption = element_text(size = 8, color = "grey60")) +
  geom_text(aes(label = fallow, ), vjust = -0.5, color="#996035", size = 4.0) +
  scale_fill_viridis_d(option = "viridis")

V3_fallow <- biom %>% 
  filter(viveiro == 3) %>% 
  mutate(ciclo = factor(ciclo))

V3_fallow %>% ggplot(aes(ciclo, fallow, fill = ciclo)) + 
  geom_col() +
  theme(legend.position = "none") +
  labs(title = "V3-Número de Dias Parados Entre Cultivos ",
       subtitle = "Por Ciclo",
       caption = "Fonte: Azul Marinho Aquicultura",
       y = "Dias Parados",
       x = "Ciclo de Cultivo") +
  theme(plot.caption = element_text(size = 8, color = "grey60")) +
  geom_text(aes(label = fallow, ), vjust = -0.5, color="#996035", size = 4.0) +
  scale_fill_viridis_d(option = "viridis")

V2_fallow <- biom %>% 
  filter(viveiro == 2) %>% 
  mutate(ciclo = factor(ciclo))

V2_fallow %>% ggplot(aes(ciclo, fallow, fill = ciclo)) + 
  geom_col() +
  theme(legend.position = "none") +
  labs(title = "V2-Número de Dias Parados Entre Cultivos ",
       subtitle = "Por Ciclo",
       caption = "Fonte: Azul Marinho Aquicultura",
       y = "Dias Parados",
       x = "Ciclo de Cultivo") +
  theme(plot.caption = element_text(size = 8, color = "grey60")) +
  geom_text(aes(label = fallow, ), vjust = -0.5, color="#996035", size = 4.0) +
  scale_fill_viridis_d(option = "viridis")


V1_fallow <- biom %>% 
  filter(viveiro == 1) %>% 
  mutate(ciclo = factor(ciclo))

V1_fallow %>% ggplot(aes(ciclo, fallow, fill = ciclo)) + 
  geom_col() +
  theme(legend.position = "none") +
  labs(title = "V1-Número de Dias Parados Entre Cultivos ",
       subtitle = "Por Ciclo",
       caption = "Fonte: Azul Marinho Aquicultura",
       y = "Dias Parados",
       x = "Ciclo de Cultivo") +
  theme(plot.caption = element_text(size = 8, color = "grey60")) +
  geom_text(aes(label = fallow, ), vjust = -0.5, color="#996035", size = 4.0) +
  scale_fill_viridis_d(option = "viridis")




# Dias parados todos os viveiros ------------------------------------------



biom %>% 
  mutate(ciclo = factor(ciclo)) %>% 
  group_by(viveiro) %>% 
  ggplot(aes(ciclo, fallow, fill = ciclo)) + 
  geom_col() +
  theme(legend.position = "none") +
  labs(title = "Número de Dias Parados Entre Cultivos ",
       subtitle = "Por Ciclo",
       caption = "Fonte: Azul Marinho Aquicultura",
       y = "Dias Parados",
       x = "Ciclo de Cultivo") +
  theme(plot.caption = element_text(size = 8, color = "grey60")) +
  geom_text(aes(label = fallow, ), vjust = -0.5, color="#996035", size = 4.0) +
  scale_fill_viridis_d(option = "viridis") +
  facet_wrap(~viveiro, scales = "free")

biom %>% 
  group_by(viveiro) %>% 
  summarize(med_dias_parados = mean(fallow, na.rm = TRUE)) 



# Problemas Ciclo 26 V1 e V2 ----------------------------------------------



V1 <- biom %>%
  filter(viveiro == 1) %>% 
  mutate(ciclo = factor(ciclo)) %>% 
  select(ciclo, densidade, biom_real, sobrevive, 
         produtividade, g_semana, g_final, ddc)
  


v <- ggplot(V1,aes(ddc, g_final)) +
  #geom_point(aes(shape = factor(viveiro)), size = 2.5) +
  geom_smooth(method = lm, se = FALSE) +
  geom_point(aes(color = ciclo), size = 3)+
  labs(title = "V1-Peso Médio Final (g)\nPor Dias de Cultivo",
       y = "Peso final (g)",
       x = "Dias de Cultivo",
       caption = "Fonte: Azul Marinho Aquicultura") +
  theme_minimal() +
  theme(plot.caption = element_text(size = 7, color = "grey60"))

v

fit_V1 <- lm(g_final ~ ddc, data = V1)
summary(fit_V1)

lm_eqn <- function(fit_V1) {
  fit_V1 <- lm(g_final ~ ddc, data = V1)
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(fit_V1)[1]), digits = 3),
                        b = format(unname(coef(fit_V1)[2]), digits = 3),
                        r2 = format(summary(fit_V1)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

v + geom_label(x = 60, y = 14, label = lm_eqn(fit_V1), 
               color="#654CFF", size = 6, parse = TRUE) +
  
  geom_text(aes(label = ciclo ), vjust = -0.5, color="#654CFF", size = 4.0) +
  theme(legend.position = "none")



V2 <- biom %>%
  filter(viveiro == 2) %>% 
  mutate(ciclo = factor(ciclo)) %>% 
  select(ciclo, densidade, biom_real, sobrevive, 
         produtividade, g_semana, g_final, ddc)



v <- ggplot(V2 ,aes(ddc, g_final)) +
  #geom_point(aes(shape = factor(viveiro)), size = 2.5) +
  geom_smooth(method = lm, se = FALSE) +
  geom_point(aes(color = ciclo), size = 3)+
  labs(title = "V2-Peso Médio Final (g)\nPor Dias de Cultivo",
       y = "Peso final (g)",
       x = "Dias de Cultivo",
       caption = "Fonte: Azul Marinho Aquicultura") +
  theme_minimal() +
  theme(plot.caption = element_text(size = 7, color = "grey60"),
        legend.position = "none") +
  geom_text(aes(label = ciclo ), vjust = -0.5, color="#654CFF", size = 4.0) 

v

fit_V2 <- lm(g_final ~ ddc, data = V2)
summary(fit_V2)

lm_eqn <- function(fit_V2) {
  fit_V2 <- lm(g_final ~ ddc, data = V2)
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,  # nolint
                   list(a = format(unname(coef(fit_V2)[1]), digits = 3),
                        b = format(unname(coef(fit_V2)[2]), digits = 3),
                        r2 = format(summary(fit_V2)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

v + geom_label(x = 60, y = 12, label = lm_eqn(fit_V2), 
               color="#654CFF", size = 6, parse = TRUE) 
  

# Crescimento Semanal -----------------------------------------------------

 
V1_V2_crescimento <- biom %>%
  filter(viveiro %in% c(1,2)) %>% 
  mutate(ciclo = factor(ciclo)) %>% 
  select(viveiro,ciclo, densidade, biom_real, sobrevive, 
         produtividade, g_semana, g_final, ddc)

V1_V2_crescimento %>%
  group_by(viveiro) %>% 
  summarize(crescimento = mean(g_semana, na.rm = TRUE))


# Viveios despescados em agosto & setembro -----------------------------------------

despesca_ago_set <- biom %>% 
  mutate(mes_despesca = month(data_desp)) %>% 
  filter(mes_despesca %in% c(8, 9))


despesca_ago_set %>% 
  filter(viveiro == 1) %>% 
  summarise(crescimento = mean(g_semana, na.rm = TRUE), 
            dias = mean(ddc, na.rm = TRUE),
            peso = mean(g_final, na.rm =TRUE))

despesca_ago_set %>% 
  filter(viveiro == 2) %>% 
  summarise(crescimento = mean(g_semana, na.rm = TRUE), 
            dias = mean(ddc, na.rm = TRUE),
            peso = mean(g_final, na.rm =TRUE))


despesca_ago_set %>% 
  filter(viveiro %in% c(1, 2)) %>% 
  group_by(viveiro, mes_despesca) %>% 
  summarise(cultivos = n(),
            crescimento = mean(g_semana, na.rm = TRUE), 
            dias = mean(ddc, na.rm = TRUE),
            peso_g = mean(g_final, na.rm =TRUE),
            densidade = mean(densidade, na.rm = TRUE),
            sobreviv = mean(sobrevive, na.rm = TRUE))

despesca_mes <- biom %>% 
  mutate(mes_despesca = factor(month(data_desp, label = TRUE)))

despesca_mes %>% 
  ggplot(aes(mes_despesca, fill = mes_despesca)) +
  geom_bar()+
  geom_text(aes(label = ..count..), stat = "count", 
            vjust = -0.5, colour = "#004586") +
  scale_fill_viridis_d(option = "turbo") +
  labs(title = "Número de Despescas por Meses do Ano",
       y = "Despescas",
       x = "Meses",
       caption = "Azul Marinho Aquicultura") +
  theme(legend.position = "none",
        plot.caption = element_text(size = 8, color = "grey60"))

despesca_mes %>% 
  count(mes_despesca, sort = TRUE)

biom %>% 
  filter(densidade >= 11.99 & densidade <= 12.01) %>% 
  select(viveiro, ciclo, g_semana, sobrevive, ddc, g_final) %>% 
  summarize(cultivos = n(), peso_medio = mean(g_final), 
            ddc = mean(ddc), sobrevive = mean(sobrevive))

## Produção acumulada em kg para cada viveiro.

biom %>% 
  mutate(viveiro = factor(viveiro)) %>% 
  count(viveiro, wt = biom_real) %>% 
  ggplot(aes(viveiro,n, fill = viveiro)) +
  geom_col()+
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ","))
 


biom %>%
  mutate(ano_despesca = factor(year(data_desp))) %>% 
  #filter(!is.na(densidade)) %>% 
  group_by(ano_despesca) %>% 
  summarize(densidade_media = mean(densidade)) %>%
  ggplot(aes(ano_despesca, densidade_media)) +
  geom_point() +
  geom_line(aes(group = 1), color = "red") +
  labs(title = "Densidade Média ao Ano",
       y = "densidade média",
       x = "Anos",
       caption = "Fonte: Azul Marinho") +
  expand_limits(y = 0) +
  scale_y_continuous(labels = comma_format())

V1 <- biom %>%
  filter(viveiro == 1) %>% 
  mutate(ciclo = factor(ciclo)) %>% 
  select(ciclo, densidade, biometria_1,dbiometria_1, 
         produtividade, g_semana, g_final, ddc)
V1 %>% summarise(median_b1 = median(biometria_1), 
                 median_db1 = median(dbiometria_1))

#V1



V1 <- biom %>%
  filter(viveiro == 1) %>% 
  mutate(ciclo = factor(ciclo)) %>% 
  select(ciclo, densidade, biometria_1,dbiometria_1, 
         produtividade, g_semana, g_final, ddc)
V1 %>% summarise(min_b1 = min(biometria_1), min_db1 = min(dbiometria_1))


summary(V1$biometria_1)


V1 <- biom %>%
  filter(viveiro == 1) %>% 
  mutate(ciclo = factor(ciclo)) %>% 
  select(ciclo, densidade, biometria_1,dbiometria_1, 
         produtividade, g_semana, g_final, ddc)
V1 %>% summarise(max_b1 = max(biometria_1), 
                 min_db1 = min(dbiometria_1))



V1 <- biom %>%
  filter(viveiro == 1) %>% 
  mutate(ciclo = factor(ciclo)) %>% 
  select(ciclo, densidade, biometria_1,dbiometria_1, 
         produtividade, g_semana, g_final, ddc)
V1 %>% summarise(median_b1 = median(biometria_1), 
                 median_db1 = median(dbiometria_1))


# Sobrev. por ano ---------------------------------------------------------

sobrevive_ano <- biom %>%  
  mutate(ano_desp = factor(year(data_desp))) %>% 
  group_by(ano_desp) %>% 
  summarize(sobrevive = median(sobrevive))

sobrevive_ano %>% 
  ggplot(aes(ano_desp, sobrevive)) +
  geom_point() +
  geom_line(group =1)+
  expand_limits(y = 60)+
  labs(title = "Taxa Anual de Sobrevivência",
       x = "Ano",
       y = "Percentual",
       caption = "Azul Marinho Aquicultura") +
  geom_text(aes(label = sobrevive), vjust=1.6, color="blue", size=4.0)+
  theme(plot.caption = element_text(size = 8, color = "grey60"))


# tables ------------------------------------------------------------------

biom %>% gt() %>% 
tab_options(
  table.font.color = "#81B1D6",
  column_labels.background.color = "#4B974F",
  table_body.hlines.color = "#006DDB"
)






tab_options(
  summary_row.background.color = "#ACEACE80",
  grand_summary_row.background.color = "#990000",
  row_group.background.color = "#FFEFDB80",
  heading.background.color = "#EFFBFC",
  column_labels.background.color = "#EFFBFC",
  stub.background.color = "#EFFBFC",
  table.font.color = "#323232",
  table_body.hlines.color = "#989898",
  table_body.border.top.color = "#989898",
  heading.border.bottom.color = "#989898",
  row_group.border.top.color = "#989898",
  row_group.border.bottom.style = "none",
  stub.border.style = "dashed",
  stub.border.color = "#989898",
  stub.border.width = "1px",
  summary_row.border.color = "#989898",
  table.width = "60%"
)




# Correlation -------------------------------------------------------------

library(RColorBrewer)

biom_numeric <- biom %>% 
  na.omit() %>% 
  select_if(is.numeric)

cor(biom_numeric[, 3:19]) %>%
  corrplot()

cor_matrix <- biom %>% 
  na.omit() %>% 
  select_if(is.numeric) %>% 
  cor()

cor_matrix

cor.test(biom$biom_calc, biom$biom_real, 
         method = "pearson", use = "complete.obs")

cor.test(biom$produtividade, biom$sobrevive, 
         method = "pearson", use = "complete.obs")

 
cor(biom_numeric[, 1:9]) %>%
  corrplot(method = "color",
           type = "lower",
           order = "alphabet",
           diag = FALSE,
           addCoef.col = 'black',
           tl.col = "blue",
           tl.srt = 45,
           number.cex = 0.9,
           addgrid.col = "black",
           title = "Matrix de Correlação \nde Produção de Camarão Marinho",
           mar=c(0,0,5,0),
           #col = COL2('PiYG')),
           #col = COL2('PuOr', 10)),
           #col = COL1('YlGn'),
           #col = brewer.pal(n=8, name="Paired"),
           col = brewer.pal(n=8, name="RdYlBu"))





# Estimativa de biomassa real ---------------------------------------------

## Cálculo para toda a fazenda. Melhor usar este e 
#acrecenta mais 15% ao resultado.

fit_biom <- lm(biom_real ~ biom_calc + pop + ddc + 
                 densidade + baixa_mil, data = biom)
summary(fit_biom)

biom_V2 <- biom %>% 
  na.omit() %>% 
  filter(viveiro == 2)

## Mesmo cálculo para o viveiro 2

fit_biom_V2 <- lm(biom_real ~ biom_calc + pop + ddc + 
                    densidade + baixa_mil, data = biom_V2)
summary(fit_biom_V2)



