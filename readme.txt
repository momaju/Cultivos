Despesca Mensal
Estou tentando agrupar as despesca mes a mes. Para tanto é necessário extrair o mes de despesca da data de despesca. O que pode ser feito com o seguinte código:
biom_mes <- biom %>% mutate(mes = month(data_desp, label = TRUE, abb = FALSE))

For `month()` and `wday()` you can set `label = TRUE` to return the abbreviated name of the month or day of the week. Set `abbr = FALSE` to return the full name.

summarise( ano = ano_desp,total_kg = sum(biom_real), .groups = "drop") %>% 
  # .groups = "drop" elimina a soma cumulativa por grupo e soma todo o ano,
  # caso contrário acumula por grupo de viveiro.