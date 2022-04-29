

# Quais os viveiros povoados com 12 por mq com os melhores resultados?



Produtividade_12_ciclo <- biom %>% 
  group_by(ciclo,viveiro) %>%# agrupa os dados por ciclo
  mutate(ciclo = factor(ciclo), viveiro = factor(viveiro)) %>%
  summarize(densidade = round(mean(densidade), 2),
            gramatura = mean(g_final), 
            produção = sum(biom_real), 
            produtividade = round(mean(produtividade),2), 
            sobrevive = round(mean(sobrevive),2),
            dias = round(sum(ddc),2)) %>%
  filter(densidade >= 12, densidade <= 13, produtividade >= 1000) %>% 
  #filter(densidade >= 5 & sobrevive >= 50) %>% # filter após summarize,
  # sai como quqero. Omitindo  esta linha, pega totos os ciclos.
  arrange(densidade)

Produtividade_12_ciclo



# Com 10 camarões/mq ------------------------------------------------------



Produtividade_10_ciclo <- biom %>% 
  group_by(ciclo,viveiro) %>%# agrupa os dados por ciclo
  mutate(ciclo = factor(ciclo), viveiro = factor(viveiro)) %>%
  summarize(densidade = round(mean(densidade), 2),
            gramatura = mean(g_final), 
            produção = sum(biom_real), 
            produtividade = round(mean(produtividade),2), 
            sobrevive = round(mean(sobrevive),2),
            dias = round(sum(ddc),2)) %>%
  filter(densidade >= 10, densidade <= 13, produtividade >= 1000) %>% 
  #filter(densidade >= 5 & sobrevive >= 50) %>% # filter após summarize,
  # sai como quqero. Omitindo  esta linha, pega totos os ciclos.
  arrange(densidade)

Produtividade_10_ciclo
