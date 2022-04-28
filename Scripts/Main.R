

# Carregando os pacotes ---------------------------------------------------

library(tidyverse)
library(googlesheets4)
library(ggthemes)
library(googledrive)
library(scales)


# Lendo a planilha de dados -----------------------------------------------

#A planilha está no formato google sheets, 
#denomina-se Biomassa e a leitura é feita utilizando-se o pacote 
#googlesheest4, fornecendo o ID da planilha. 
#Este pode ser obtido diretamente do URL da planilha. 
#Veja instuções no Onenote. A "sheet"Biomassa, é a segunda da planilha. 
#O sistema pede autenticação do usuário.

Biom <- read_sheet("1KkLM7bz-Az-etHUeENou-BjX4mDUfJCccwcCIo0k0CU", 2)

# or by its name, which requires an assist from the googledrive package (googledrive.tidyverse.org):

drive_get("Cultivo") %>% 
  read_sheet("Biomassa")

# or

Biom <- drive_get("Cultivo") %>% 
        read_sheet(2)


# Produtividade -----------------------------------------------------------

Produtividade_ciclo <- Biom %>% 
  group_by(ciclo) %>%# agrupa os dados por ciclo
  mutate(ciclo = factor(ciclo)) %>%
  summarize(densidade = round(mean(densidade), 2), gramatura = mean(g_final), 
            produção = sum(biom_real), produtividade = round(mean(produtividade),2), 
            sobrevive = round(mean(sobrevive),2)) %>%
  #filter(densidade >= 5 & sobrevive >= 50) %>% # filter após summarize,
  # sai como quqero. Omitindo  esta linha, pega totos os ciclos.
  arrange(sobrevive)

Produtividade_ciclo
Produtividade_ciclo %>% 
  ggplot(aes(ciclo, produtividade)) + # Cria um ggplot object
  geom_bar(stat = "identity", fill = "royalblue2") +  # Defines the geometry
  geom_hline(yintercept = 1000, linetype="dashed", color = "black") +  
  geom_text(aes(label = produtividade), vjust = -1, color = "black", size = 3) +
  labs(title = "Produtividade por Ciclo",
       #subtitle = "Destacando a Sobrevivência Média",
       caption = "Fonte: Azul Marinho Aquicultura",
       x = "Ciclo de Cultivo",
       y = "Produtividade (kg/ha)")


