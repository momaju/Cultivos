

# Viveors povoados no mes de outubro --------------------------------------

library(tidyverse)
library(googlesheets4)
library(ggthemes)
library(lubridate)
library(ggrepel)


biom <- read_sheet("1KkLM7bz-Az-etHUeENou-BjX4mDUfJCccwcCIo0k0CU", 2)


# Todos os viveiros por mes de povoamento -------------------------------------------------------


# agosto ------------------------------------------------------------------


povoa_ago <- biom %>% 
  mutate(mes_povoa = month(data_pov)) %>% 
  filter(mes_povoa == 8) %>% 
  group_by(viveiro) %>% 
  summarise(survival = mean(sobrevive, na.rm = TRUE),
            kg = mean(biom_real, na.rm = TRUE),
            densidade = mean(densidade, na.rm = TRUE))


# Setembro ----------------------------------------------------------------

povoa_set <- biom %>% 
  mutate(mes_povoa = month(data_pov)) %>% 
  filter(mes_povoa == 9) %>% 
  group_by(viveiro) %>% 
  summarise(survival = mean(sobrevive, na.rm = TRUE),
            kg = mean(biom_real, na.rm = TRUE),
            densidade = mean(densidade, na.rm = TRUE))



# Outubro -----------------------------------------------------------------



povoa_out <- biom %>% 
  mutate(mes_povoa = month(data_pov)) %>% 
  filter(mes_povoa == 10) %>% 
  group_by(viveiro) %>% 
  summarise(survival = mean(sobrevive, na.rm = TRUE),
            kg = mean(biom_real, na.rm = TRUE),
            densidade = mean(densidade, na.rm = TRUE))





# Apenas viveiros 3 e 4 ---------------------------------------------------

povoa_out_34 <- biom %>% 
  mutate(mes_povoa = month(data_pov)) %>% 
  filter(mes_povoa == 10 & viveiro %in% c(3,4))




# Povoamento Mensal -------------------------------------------------------



povoa_mensal <- biom %>% 
  mutate(mes_povoa = factor(month(data_pov, label = TRUE)))

povoa_mensal %>% 
  ggplot(aes(mes_povoa, fill = mes_povoa)) +
  geom_bar()+
  geom_text(aes(label = ..count..), stat = "count", 
            vjust = -0.5, colour = "#004586") +
  scale_fill_viridis_d(option = "turbo") +
  labs(title = "Número de Povoamentos por Meses do Ano",
       y = "Povoamentos",
       x = "Meses",
       caption = "Azul Marinho Aquicultura") +
  theme(legend.position = "none",
        plot.caption = element_text(size = 8, color = "grey60"))
