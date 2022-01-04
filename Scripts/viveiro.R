Biom %>% 
  #na.omit() %>% 
  filter(viveiro == 4,ddc > 70) %>% 
  #select(viveiro, ciclo, ddc, sobrevive, g_semana, produtividade, g_final) %>% 
  summarise(mean_g_semana = mean(g_semana, na.rm = TRUE), 
            mean_produtividade = mean(produtividade, na.rm = TRUE), 
            sobrevive = mean(sobrevive, na.rm = TRUE), 
            gramatura = max (g_final),n = n(), max_ddc =max(ddc))

Produtividade_ciclo_V2 <- Biom %>% 
  filter(viveiro == 2) %>%
  group_by(ciclo) %>%# agrupa os dados por ciclo
  mutate(ciclo = factor(ciclo)) %>%
  summarize(densidade = round(mean(densidade), 2), gramatura = mean(g_final), 
            produção = sum(biom_real), produtividade = round(mean(produtividade),2), 
            sobrevive = round(mean(sobrevive),2)) %>%
  #filter(densidade >= 5 & sobrevive >= 50) %>% # filter após summarize,
  # sai como quqero. Omitindo  esta linha, pega totos os ciclos.
  arrange(produtividade)

Produtividade_ciclo_V2 %>% 
  ggplot(aes( sobrevive,produtividade, color = ciclo, label = ciclo))+
  geom_point(size = 3) +
  geom_text(nudge_x = 2, nudge_y = 3) +
  labs(title = "Produtividade vs. Sobrevivência - V02",
       x = "Sobrevivência",
       y = "Produtividade",
       caption = "Azul Marinho Aquicultura") +
  theme(legend.position = "none")


Ciclo_V2 <- Biom %>% 
  filter(viveiro == 2) %>% 
  summarise(median_g_final = median(g_final), 
            median_ddc = median(ddc),
            median_sobrevive = median(sobrevive),
            median_pop = median(pop),
            median_densidade = median(densidade))

Ciclo_V2


Ciclo_V1 <- Biom %>% 
  filter(viveiro == 1) %>% 
  summarise(median_g_final = median(g_final), 
            median_ddc = median(ddc),
            median_sobrevive = median(sobrevive),
            median_pop = median(pop),
            median_densidade = median(densidade)
            )

Ciclo_V1


Ciclo_V3 <- Biom %>% 
  filter(viveiro == 3) %>% 
  summarise(median_g_final = median(g_final), 
            median_ddc = median(ddc),
            median_sobrevive = median(sobrevive),
            median_pop = median(pop),
            median_densidade = median(densidade)
  )

Ciclo_V3


Ciclo_V4 <- Biom %>% 
  filter(viveiro == 4) %>% 
  summarise(median_g_final = median(g_final), 
            median_ddc = median(ddc),
            median_sobrevive = median(sobrevive),
            median_pop = median(pop),
            median_densidade = median(densidade)
  )

Ciclo_V4

summary_v1 <- Biom %>% 
  filter(viveiro == 1) %>%
  summary()

summary_v1


summary_v2 <- Biom %>% 
  filter(viveiro == 2) %>%
  summary()

summary_v2

summary_v3 <- Biom %>% 
  filter(viveiro == 3) %>%
  summary()

summary_v3

summary_v4 <- Biom %>% 
  filter(viveiro == 4) %>%
  summary()

summary_v4












