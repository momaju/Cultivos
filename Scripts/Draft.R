
## Primeiro passo: Abrir o arquivo 2019_Biomassa.csv em C:\Users\momaj\Google Drive\RWork\Azul Marinho, 
## Passar a informação para um data frame Biomassa.

Biomassa <- read.csv(file.choose(), header = T)
head(Biomassa)  # confirma que os dados foram corretamente carregados.


## Outra alternativa para ler a aba Biomassa da planilha cultivos:
## 
##Neste caso, já é lida como tibble, e não é necessário utilizar 
## Biom <- as_tibble(Biomassa) # transforma o data frame na tibble Biom


# Carregando as bibliotecas -----------------------------------------------


library(tidyverse)
library(googlesheets4)
library(ggthemes)





# Lendo a planinha Biomassa utilizando  o pacote googlesheets4 ------------


Biom <- read_sheet("1KkLM7bz-Az-etHUeENou-BjX4mDUfJCccwcCIo0k0CU", 2)




  
## Cria um barplot mostrando a produtividade por cilo, usando o objeto Biom que foi criado
## com o código anterior.


Produtividade_ciclo <- Biom %>% 
    #filter(densidade >= 10 & sobrevive >= 50) %>% # neste caso, o filtro opera
    #antes de summarize e não é o desejado
    group_by(ciclo) %>%# agrupa os dados
    mutate(ciclo = factor(ciclo)) %>%
    summarize(densidade = round(mean(densidade), 2), gramatura = mean(g_final), 
              produção = sum(biom_real), produtividade = round(mean(produtividade),2), 
              sobrevive = round(mean(sobrevive),2)) %>%
    #filter(densidade >= 5 & sobrevive >= 50) %>% # filter após summarize,
    # sai como quqero. Omitindo  esta linha, pega totos os ciclos.
    arrange(sobrevive)

Produtividade_ciclo



# Produtividade por ciclo, destacando a sobrevivência ---------------------


Produtividade_ciclo %>% 
  ggplot(aes(ciclo, produtividade)) + # Cria um ggplot object
  geom_bar(stat = "identity", fill = "royalblue2") +  # Defines the geometry
  geom_hline(yintercept = 1000, linetype="dashed", color = "black") +  
  geom_text(aes(label = sobrevive), vjust = -1, color = "black", size = 3) +
  labs(title = "Produtividade por Ciclo",
         subtitle = "Destacando a Sobrevivência Média",
         caption = "Fonte: Azul Marinho Aquicultura",
         x = "Ciclo de Cultivo",
         y = "Produtividade (kg/ha)")




# Produtividade por viveiiro por ciclo ------------------------------------



Produtividade_viveiro_ciclo <- Biom %>% 
  #filter(densidade >= 10 & sobrevive >= 50) %>% # neste caso, o filtro opera
  #antes de summarize e não é o desejado
  group_by(viveiro, ciclo) %>%# agrupa os dados
  mutate(ciclo = factor(ciclo), viveiro = factor(viveiro)) %>%
  summarize(densidade = round(mean(densidade), 2), gramatura = mean(g_final), 
            produção = sum(biom_real), produtividade = round(mean(produtividade),2), 
            sobrevive = round(mean(sobrevive),2), ddc = mean(ddc)) %>%
  #filter(densidade >= 5 & sobrevive >= 50) %>% # filter após summarize,
  # sai como quqero. Omitindo  esta linha, pega totos os ciclos.
  arrange(sobrevive)

Produtividade_viveiro_ciclo




# Produtividade média por viveiro por ciclo -------------------------------


Produtividade_viveiro_ciclo %>% 
  ggplot(aes(ciclo, produtividade, fill = viveiro)) + # Cria um ggplot object
  geom_col() +  # Defines the geometry
  geom_hline(yintercept = 1000, linetype="dashed", color = "black") +  
  #geom_text(aes(label = sobrevive),
   #         vjust = -0.25,
   #         color = "black", size = 3,
   #          position = position_dodge(width = 0.9),
   #          check_overlap = TRUE) +
  facet_wrap(~ viveiro, scales = "free") +
  labs(title = "Produtividade por Ciclo",
       #subtitle = "Destacando a Sobrevivência Média",
       caption = "Fonte: Azul Marinho Aquicultura",
       x = "Ciclo de Cultivo",
       y = "Produtividade (kg/ha)")




# Produtividade com destaque para  sobevivência -----------------------------------------------



Produtividade_viveiro_ciclo %>% 
  ggplot(aes(ciclo, produtividade, fill = viveiro)) + # Cria um ggplot object
  geom_col() +  # Defines the geometry
  geom_hline(yintercept = 1000, linetype="dashed", color = "black") +  
  geom_text(aes(label = sobrevive),
            vjust = -0.25,
            color = "black", size = 3,
            position = position_dodge(width = 0.9),
            check_overlap = TRUE) +
  facet_wrap(~ viveiro, scales = "free") +
  labs(title = "Produtividade por Ciclo",
       subtitle = "Destacando a Sobrevivência Média",
       caption = "Fonte: Azul Marinho Aquicultura",
       x = "Ciclo de Cultivo",
       y = "Produtividade (kg/ha)")





# Densidade Média por Ciclo --------------------------------------------------


Produtividade_viveiro_ciclo %>% 
  ggplot(aes(ciclo, densidade, fill = viveiro)) + # Cria um ggplot object
  geom_col() +  # Defines the geometry
  #geom_hline(yintercept = 1000, linetype="dashed", color = "black") +  
  geom_text(aes(label = densidade),
            vjust = -0.25,
            color = "black", size = 3,
            #position = position_dodge(width = 0.9),
            check_overlap = TRUE) +
  facet_wrap(~ viveiro, scales = "free") +
  theme(legend.position = "none") + # suprime a legenda, já que usamos facet_wrap
  labs(title = "Densidade Média por Ciclo",
       #subtitle = "Destacando a Densidade",
       caption = "Fonte: Azul Marinho Aquicultura",
       x = "Ciclo de Cultivo",
       y = "Densidade (ind/mq)")





# Produtividade com destaque para densidade -------------------------------


Produtividade_viveiro_ciclo %>%
  ggplot(aes(ciclo, produtividade, fill = viveiro)) + # Cria um ggplot object
  geom_col() +  # Defines the geometry
  geom_hline(yintercept = 1000, linetype="dashed", color = "black") +  
  geom_text(aes(label = densidade),
            vjust = -0.25,
            color = "black", size = 3,
            position = position_dodge(width = 0.9),
            check_overlap = TRUE) +
  facet_wrap(~ viveiro, scales = "free") +
  labs(title = "Produtividade por Ciclo",
       subtitle = "Destacando a Densidade de Estocagem",
       caption = "Fonte: Azul Marinho Aquicultura",
       x = "Ciclo de Cultivo",
       y = "Produtividade (kg/ha)")




# Produtividade destacando os dias de cultivo -----------------------------


Produtividade_viveiro_ciclo %>% 
  ggplot(aes(ciclo, produtividade, fill = viveiro)) + # Cria um ggplot object
  geom_col() +  # Defines the geometry
  geom_hline(yintercept = 1000, linetype="dashed", color = "black") +  
  geom_text(aes(label = ddc),
            vjust = -0.25,
            color = "black", size = 3,
            position = position_dodge(width = 0.9),
            check_overlap = TRUE) +
  facet_wrap(~ viveiro, scales = "free") + # scales = "free" coloca labels em todos os eixos
  labs(title = "Produtividade por Ciclo",
       subtitle = "Destacando Dias de Cultivo",
       caption = "Fonte: Azul Marinho Aquicultura",
       x = "Ciclo de Cultivo",
       y = "Produtividade (kg/ha)")




# Boxplot de produtividade por viveiro ------------------------------------


Produtividade_viveiro_ciclo %>% 
  ggplot(aes(viveiro, produtividade, fill = viveiro)) + # Cria um ggplot object
  geom_boxplot() +  # Defines the geometry
  geom_hline(yintercept = 1000, linetype="dashed", color = "black") +  
  #geom_text(aes(label = ddc),
            #vjust = -0.25,
            #color = "black", size = 3,
            #position = position_dodge(width = 0.9),
            #check_overlap = TRUE) +
  #facet_wrap(~ viveiro, scales = "free") + # scales = "free" coloca labels em todos os eixos
  theme(legend.position = "none") +
  labs(title = "Produtividade por Ciclo",
       #subtitle = "Destacando Dias de Cultivo",
       caption = "Fonte: Azul Marinho Aquicultura",
       x = "Viveiro",
       y = "Produtividade (kg/ha)")







# Produtividade >= 1000 ---------------------------------------------------


Produtividade_mil <- Biom %>% 
  #filter(densidade >= 10 & sobrevive >= 50) %>% # neste caso, o filtro opera
  #antes de summarize e não é o desejado
  filter(produtividade >= 1000) %>% 
group_by(viveiro, ciclo) %>%# agrupa os dados
  mutate(ciclo = factor(ciclo), viveiro = factor(viveiro)) %>%
  summarize(densidade = round(mean(densidade), 2), gramatura = mean(g_final), 
            produção = sum(biom_real), produtividade = round(mean(produtividade),2), 
            sobrevive = round(mean(sobrevive),2), ddc = mean(ddc)) %>%
  #filter(densidade >= 5 & sobrevive >= 50) %>% # filter após summarize,
  # sai como quqero. Omitindo  esta linha, pega totos os ciclos.
  arrange(sobrevive)

Produtividade_mil %>% 
  select(produtividade, densidade) %>% 
  summarise(densidade) %>% 
  View()

summary(Produtividade_mil)





Biom_1000 <- Biom %>% 
  filter(produtividade >= 1000) %>%
  mutate(year = lubridate ::year(data_desp)) %>% 
  group_by(year, ciclo) %>% # agrupa os dados 
  summarize(densidade = round(mean(densidade), 2), gramatura = round(mean(g_final),2), 
            produção = sum(biom_real), produtividade = round(mean(produtividade),2), 
            sobrev = round(mean(sobrevive), 2)) %>%
  arrange(desc(produtividade))

Biom_1000


## Mudando a orientação do gráfico
## 
Biom_ciclo %>%   
  ggplot(aes(y = fct_reorder(ciclo, produtividade),x = produtividade)) + # Cria um ggplot object
  geom_bar(stat = "identity",fill = "lightblue4") +  # Defines the geometry
  #coord_flip() +
  geom_vline(xintercept = 1000, linetype="dashed", color = "blue", size = 1) +  
  geom_text(aes(label = produtividade), hjust = 1, color = "bisque2", size = 4, fontface = "bold") +
  labs(title = "Produtividade por Ciclo",
       subtitle = "Destacando a Produtividade Média",
       caption = "Fonte: Azul Marinho Aquicultura",
       x = "Produtividade (kg/ha)",
       y = "Ciclo de Cultivo")

Biom_ciclo


# Unico Viveiro -----------------------------------------------------------


## Produtividae 
## 
## 

biom_v3 <- Biom %>%
  filter(viveiro == 3) %>% 
  mutate(ciclo = factor(ciclo))
  
 biom_v3 %>% 
  ggplot(aes(x = produtividade,y = fct_reorder(ciclo, produtividade))) + # Cria um ggplot object
  geom_bar(stat = "identity",fill = "lightblue4") +  # Defines the geometry
  #coord_flip() +
  geom_vline(xintercept = 1000, linetype="dashed", color = "blue", size = 1) +  
  geom_text(aes(label = produtividade), hjust = 1, color = "bisque2", size = 4, fontface = "bold") +
  labs(title = "Produtividade por Ciclo - V03",
       subtitle = "Destacando a Produtividade (kg/ha)",
       caption = "Fonte: Azul Marinho Aquicultura",
       x = "Produtividade (kg/ha)",
       y = "Ciclo de Cultivo")
 
 
  ## Produção
  
 biom_v3 <- Biom %>%
   filter(viveiro == 3) %>% 
   mutate(ciclo = factor(ciclo))
 
 biom_v3 %>% 
   ggplot(aes(x = biom_calc,y = fct_reorder(ciclo, biom_calc))) + # Cria um ggplot object
   geom_bar(stat = "identity",fill = "lightblue4") +  # Defines the geometry
   #coord_flip() +
   #geom_vline(xintercept = 1000, linetype="dashed", color = "blue", size = 1) +  
   geom_text(aes(label = biom_calc), hjust = 1, color = "bisque2", size = 4, fontface = "bold") +
   labs(title = "Produção por Ciclo - V03",
        subtitle = "Total em kg.",
        caption = "Fonte: Azul Marinho Aquicultura",
        x = "Produção em kg",
        y = "Ciclo de Cultivo")
 

# Linhas e Pontos ---------------------------------------------------------

 
 
 ## Gráfico de linhas e pontos.
  

  
  Biom_sep_ano <- Biom %>%
    separate(data_desp, c("aaaa", "mm", "dd"), sep = "-") %>%
    mutate(ano = aaaa) %>%
    group_by(ano,ciclo) %>% # agrupa os dados 
    summarize(densidade = round(mean(densidade), 2), gramatura = mean(g_final), 
              produção = sum(biom_real), produtividade = mean(produtividade), 
              sobrev = mean(sobrevive))
    ggplot(Biom_sep_ano,aes(ciclo, produtividade)) +
    geom_point(aes(color = ano),size = 4) + # a legenda não ficou boa.
    geom_line() + # adiciona uma linha ao gráfico
    scale_color_discrete(name = "Year") + # Esta linha corrige a legenda.
    xlab("Grow out Cycle") + 
    ylab("Productiviy in kg/ka") +
    ggtitle("Productivity for Semi-intensive Shrimp Farming") +
    labs(caption =  "Source: Mozart Marinho-Jr, 2020") +
    theme_economist()
Biom
    
  # A mesma análise acima, agora, agrupada por viveiros.
  
  Biom_viveiro <- Biom %>% 
    #filter(densidade >= 10 & sobrevive >= 50) %>% # neste caso, o filtro opera
    #antes de summarize e não é o desejado
    group_by(viveiro) %>% # agrupa os dados 
    summarize(densidade = round(mean(densidade), 2), gramatura = mean(g_final), 
              produção = sum(biom_real), produtividade = mean(produtividade), 
              sobrevive = round(mean(sobrevive),2))
    #filter(densidade >= 5 & sobrevive >= 50) %>% # filter após summarize,
    # sai como quqero. Emitindo esta linha, pega totos os ciclos.
    #arrange(sobrevive)
  Biom_viveiro
  
  Biom_viveiro %>% ggplot(aes(viveiro, produtividade)) + # Cria um ggplot object
    geom_bar(stat = "identity", fill = "steelblue") +  # Defines the geometry
    geom_hline(yintercept = 850, linetype="dashed", color = "red") +  
    geom_text(aes(label = sobrevive), vjust = -1, color = "tomato", size = 4)+
    xlab("Viveiro") + 
    ylab("Produtivide em kg/ha") +
    ggtitle("Produtividade Média Por Viveiro") +
    labs(subtitle = "2015 a 2020\nSobrevivência") +
    labs(caption =  "Fonte: Mozart Marinho-Jr, 2020")
    
  Biom_viveiro
  
  
  
  ## Grafico de Pontos entre duas variáveis.
  ## 
  
  Biomassa <- read.csv(file.choose(), header = T)
  
  head(Biomassa)
  
  library(dplyr)
  library(ggplot2)
  
  
  
  
  
  
  
  despesca <- Biom %>%
    mutate(year = lubridate ::year(data_desp)) %>% 
    select(viveiro,ciclo, biom_calc, biom_real, year)
    #filter(biom.real >= biom.calc)
    
  # Regressão
  
  
  ## O código a seguir é para gerar o texto da equação a ser inserido no 
  ## gráfico
  
  
  
  
  lm_eqn <- function(fit){
    fit <- lm(biom_real ~ biom_calc, despesca);
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                     list(a = format(unname(coef(fit)[1]), digits = 3),
                          b = format(unname(coef(fit)[2]), digits = 3),
                          r2 = format(summary(fit)$r.squared, digits = 3)))
    as.character(as.expression(eq));
  }
  
 
  
    
  despesca
  
   despesca %>% ggplot (aes(biom_calc, biom_real, label = viveiro)) +
    geom_point(aes(color = factor(year)),size = 3) +
    geom_text(nudge_x = 50) +
    xlab("Biomassa Calculada (kg)") + 
    ylab("Biomassa Despescada (kg)") +
    ggtitle("Biomassa Despescada (kg)\nPor Viveiro") +
    scale_color_discrete(name = "Ano") + # acerta a legenda
    geom_smooth(method = "lm") +
    annotate("text", x = 2000.0, y = 4800.00, label = lm_eqn(fit), 
             parse = TRUE, color = "Blue", size=4) + # insere a equação
    theme_economist() +
    labs(caption = "Fonte: Mozart Marinho-Jr., 2020")
 
  
  
  ## Boxplot
  
    ggplot(data = Biom, mapping = aes(x = factor(viveiro), y = sobrevive)) +
     geom_boxplot()
   
  #ggplot(data = biom, mapping = aes(x = factor(viveiro), y = biom.real)) +
  # geom_boxplot()
  # 

    
## Maior produtividade (kg/ha) durante o ano 
  

maior_produtividade_ano <- Biom %>%
  separate(data_desp, c("aaaa", "mm", "dd"), sep = "-") %>%
  mutate(ano = aaaa) %>%
  group_by(ano) %>%
  summarize(maior = round(max(produtividade),2)) %>% 
  arrange(maior)
  
maior_produtividade_ano


## Maior produtividade por viveiro -----------

maior_produtividade_viveiro <- Biom %>%
  separate(data_desp, c("aaaa", "mm", "dd"), sep = "-") %>%
  mutate(ano = aaaa) %>%
  group_by(viveiro) %>%
  summarize(maior = round(max(produtividade),2)) %>% 
  arrange(maior)

maior_produtividade_viveiro



## Viveiro mais produtivo --------

viveiro_mais_produtivo <- maior_produtividade_ano %>%
  inner_join(maior_produtividade_viveiro, by = "maior")

viveiro_mais_produtivo



viveiro_mais_produtivo %>% ggplot(mapping = aes(x = maior)) +
  geom_histogram(color = "white", binwidth= 100)


## Peso Médio Despescado ----------------

Biom %>%
  ggplot(aes(g_final)) +
  geom_histogram(binwidth = 1, color = "white", fill = rainbow(21)) +
  scale_x_continuous(limits = c(0, 20)) +
  scale_y_continuous(limits = c(0, 20)) +
  labs(title = "Peso Médio Despescado",
       x = "peso médio (g)")

## Sovrevivência vs. Densidade por viveiro ------------

Biom %>%
  #group_by(viveiro) %>%
  ggplot(aes(densidade, sobrevive)) +
  geom_point(aes(color = as.factor(viveiro)), size = 3) +
  geom_smooth(se = FALSE) +
  labs(color = "viveiro") +
  facet_wrap(~ viveiro, scales = "free")


## Histograma de dias parados (entre despescas)---------  

dias_parados <- Biom %>%
  filter(!is.na(fallow))%>%
  select(viveiro, ciclo,fallow)
dias_parados 



ggplot(dias_parados,aes(fallow)) +
  geom_histogram(binwidth = 5, color = "white", fill = rainbow(13)) +
  labs(title = "Dias Parados",
       x = "Dias")




 

 ## Gráfico de Dias Parados Entre Ciclos-----------------
  
   dias_parados %>% 
    group_by(viveiro) %>%
    ggplot(aes(viveiro, fallow, fill = factor(viveiro))) +
    geom_boxplot()+
    geom_jitter(alpha = 0.2) +
    scale_fill_discrete(name = "Viveiro") +
      labs(title = "Dias Parados entre Ciclos", 
            x = "Viveiro",
           y = "Dias Parados")



## Dias Parados por Ciclo de Cultivo--------------------
   
  dias_parados %>%
  group_by(ciclo) %>%
  summarize(avg.fallow = mean(fallow))

d_cultivo <- Biom %>%
  filter(!is.na(fallow))%>%
  select(viveiro, ciclo,ddc)
d_cultivo

ggplot(d_cultivo,aes(ddc)) +
  geom_histogram(binwidth = 30, boundary = 60, color = "white", fill = "tan3") +
  labs(title = "Dias de Ciltivo",
       x = "Dias")


d_cultivo %>%
  group_by(ciclo) %>%
  summarize(avg.ddc = mean(ddc), sd.ddc = sd(ddc))




# Média ddc por viveiro -- ------------------------------------------------



d_cultivo %>%
  group_by(viveiro) %>%
  summarize(avg.ddc = mean(ddc))




# Máximo ddc por viveiro - ------------------------------------------------



d_cultivo %>%
  group_by(viveiro) %>%
  summarize(max.ddc = max(ddc))





Biom %>%
  filter(ciclo == 12)
Biom %>%
  filter(ciclo == 12)

####



# Em média, qual o melhor dia para despescar? 
# 
dia_de_despesca <- Biom %>%
  mutate(dow = lubridate ::wday(data.desp, label = TRUE)) %>%
  select(dow, biom.real) %>%
  #filter(biom.real >0) %>% #no caso, este filtro não é necessário
  group_by(dow) %>%
  summarize(avg_biom = mean(biom.real))

dia_de_despesca %>%
  ggplot(aes(dow, avg_biom, fill = dow)) +
  geom_bar(stat = "identity") +
  #theme_bw() + #em caso de usar o tema, não usar scale abaixo, nem fill.
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Melhor Dia Para Despescar?",
    y = "kg Despescados (média)",
    x = "",
    fill = "Dia da\nSemana"
  )


####
# Gráfico para o Linkedin


dia_de_despesca <- Biom %>%
  mutate(dow = lubridate ::wday(data.desp, label = TRUE, locale = "English_United States.1252")) %>%
  select(dow, biom.real) %>%
  #filter(biom.real >0) %>% #no caso, este filtro não é necessário
  group_by(dow) %>%
  summarize(avg_biom = mean(biom.real))

dia_de_despesca %>%
  ggplot(aes(dow, avg_biom)) +
  geom_bar(stat = "identity") +
  theme_bw() + #em caso de usar o tema, não usar scale abaixo, nem fill.
  #scale_color_brewer(palette = "Set1") +
  labs(
    title = "Best Day to Harvest Your Shrimp?",
    y = "Mean Harverst Wight",
    x = ""
  )


####

# Quantidade de despescas por dia da semana

despesca_semana <- Biom %>%
  mutate(dow = lubridate ::wday(data.desp, label = TRUE, locale = "English_United States.1252")) %>%
  # locale coloca os dias da semana em inglês
  select(dow, biom.real) %>%
  group_by(dow) %>%
  summarize(harvests = n())

despesca_semana %>%
  ggplot(aes(dow, harvests)) +
  geom_bar(stat = "identity",fill = "antiquewhite4", color = "darkgoldenrod4") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() + #em caso de usar o tema, não usar scale abaixo, nem fill.
  #scale_fill_brewer(palette = "Set1") +
  #scale_color_uchicago() +
  theme(plot.caption = element_text(color = "darkgoldenrod4", face = "italic", hjust = 0),
        plot.title = element_text(color = "darkgoldenrod4", size = 18),
        axis.text =  element_text(colour = "darkgoldenrod4", size = 10),
        axis.ticks = element_line(size = 2),
        axis.title.y=element_text(colour="darkgoldenrod4", size = 15),
        panel.grid.minor = element_line(colour = "darkgoldenrod3", 
                                        size = 0.25, linetype = 'dotdash'),
        panel.background = element_rect(fill = "antiquewhite")) + 
  labs(
    title = "Shrimp Friday",
    y = "# of Harvests",
    x = "",
    caption = "Data: Azul Marinho Aquicultura (2020)"
  ) 
  

# A linha abaixo fornece o total de despescas por dia da semana.

pct_despescas <- despesca_semana %>%
  count(dow,harvests) %>%
  mutate(percent = (harvests /sum(harvests) * 100))
pct_despescas



knitr::kable(
  pct_despescas[1:7, c(1,2,4) ], 
  caption = "Percentual por Dia.",
  table.attr = "style='width:30%'")

#####

# Gráfico Pinguins --------------------------------------------------------

library(ggtext)

g <- ggplot(Biom, aes(ddc, g_final, group = viveiro)) +
  geom_point(aes(colour = as_factor(viveiro), shape = as_factor(viveiro)), alpha = 1, size =3) +
  scale_color_manual(values = c("orange","blue","red2", "green4")) +
  labs(title = "Peso Médio por Viveiro",
       subtitle = "Viveiros <span style = 'color:orange;'>V01</span>, <span style = 
       'color:blue;'>V02</span>, <span style = 'color:red2;'>V03</span>, <span style = 'color:green4;'>V04</span>",
       caption = "Azul Marinho Aquicultura",
       x = "Dias de Cultivo",
       y = "Peso Final (g)") +
    geom_smooth(aes(color = as_factor(viveiro)), se = FALSE) +
  theme_minimal() +  
  theme(legend.position = 'none',
        #text = element_text(family = "Consolas"), 
        # (I only have 'Light' )
        # plot.title = element_text(family = "Lucida Console", size = 16), 
        plot.subtitle = element_markdown(), # element_markdown from `ggtext` to parse the css in the subtitle
        plot.title.position = "plot",
        plot.caption = element_text(size = 8, colour = "grey50"),
        plot.caption.position = "plot")
g



 p <- ggplot(Biom, aes(ddc, sobrevive, group = viveiro)) +
  geom_point(aes(colour = as_factor(viveiro), shape = as_factor(viveiro)), alpha = 1, size =3) +
  scale_color_manual(values = c("orange","blue","red2", "green4")) +
  labs(title = "Sobrevivência por Viveiro",
       subtitle = "Viveiros <span style = 'color:orange;'>V01</span>, <span style = 
       'color:blue;'>V02</span>, <span style = 'color:red2;'>V03</span>, <span style = 'color:green4;'>V04</span>",
       caption = "Azul Marinho Aquicultura",
       x = "Dias de Cultivo",
       y = "Sobrevivência(%)") +
  geom_smooth(aes(color = as_factor(viveiro)), se = FALSE) +
  theme_minimal() +  
  theme(legend.position = 'none',
        #text = element_text(family = "Consolas"), 
        # (I only have 'Light' )
        # plot.title = element_text(family = "Lucida Console", size = 16), 
        plot.subtitle = element_markdown(), # element_markdown from `ggtext` to parse the css in the subtitle
        plot.title.position = "plot",
        plot.caption = element_text(size = 8, colour = "grey50"),
        plot.caption.position = "plot")

p




# dplyr 1.0.0 -------------------------------------------------------------



Means <- Biom %>%
  group_by(viveiro) %>%
  summarize(across(where(is.numeric), mean, na.rm =TRUE, .names = "mean_{col}"))
Means


kg_produzidos_ano <- Biom %>%
  separate(data_desp, c("aaaa", "mm", "dd"), sep = "-") %>%
  mutate(ano = aaaa) %>%
  group_by(ano) %>%
  summarize(total = round(sum(biom_real),2))

kg_produzidos_ano



# Produtivida vs. Produção-----------------




produtividade_por_ciclo <- Biom %>%
                            group_by(ciclo) %>%
                            summarise(Value = mean(produtividade), 
                                      Total = sum(biom_real, sort = TRUE))
produtividade_por_ciclo

Biom %>% 
  summarize(mean_densidade =mean(densidade), sd_densidade = sd(densidade))






