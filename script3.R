
# PLINILHAS COM OS FILTROS PARA VMP > 0 E TOTAL DETECTADOS >= 10

### TABELA ACRILAMIDA FILTRADA -----

tabela_acrilamida_filtrada <- tabela_acrilamida |> 
  filter(Total_Detectados >= 10,
         `Total de Consistentes detectados Acima do VMP` > 0)
View(tabela_acrilamida_filtrada)

write.csv(tabela_acrilamida_filtrada, "tabelas_filtradas/dados_acrilamida_filtrados.csv", row.names = FALSE)
### TABELA ANTIMONIO FILTRADO -----

tabela_antimonio_filtrada <- tabela_antimonio |> 
  filter(Total_Detectados >= 10,
         `Total de Consistentes detectados Acima do VMP` > 0)
View(tabela_antimonio_filtrada)

write.csv(tabela_antimonio_filtrada, "tabelas_filtradas/dados_antimonio_filtrados.csv", row.names = FALSE)
### TABELA ARSENIO FILTRADO -----

tabela_arsenio_filtrada <- tabela_arsenio |> 
  filter(Total_Detectados >= 10,
         `Total de Consistentes detectados Acima do VMP` > 0)
View(tabela_arsenio_filtrada)

write.csv(tabela_arsenio_filtrada, "tabelas_filtradas/dados_arsenio_filtrados.csv", row.names = FALSE)
# TABELA BARIO FILTRADA ----

tabela_bario_filtrada <- tabela_bario |> 
  filter(Total_Detectados >= 10,
         `Total de Consistentes detectados Acima do VMP` > 0)
View(tabela_bario_filtrada)

write.csv(tabela_bario_filtrada, "tabelas_filtradas/dados_bario_filtrados.csv", row.names = FALSE)
# TABELA CADMIO FILTRADA ----

tabela_cadmio_filtrada <- tabela_cadmio |> 
  filter(Total_Detectados >= 10,
         `Total de Consistentes detectados Acima do VMP` > 0)
View(tabela_cadmio_filtrada)

write.csv(tabela_cadmio_filtrada, "tabelas_filtradas/dados_cadmio_filtrados.csv", row.names = FALSE)
# TABELA CHUMBO FILTRADA ----

tabela_chumbo_filtrada <- tabela_chumbo |> 
  filter(Total_Detectados >= 10,
         `Total de Consistentes detectados Acima do VMP` > 0)
View(tabela_chumbo_filtrada)

write.csv(tabela_chumbo_filtrada, "tabelas_filtradas/dados_chumbo_filtrados.csv", row.names = FALSE)

# TABELA COBRE FILTRADA ----

tabela_cobre_filtrada <- tabela_cobre |> 
  filter(Total_Detectados >= 10,
         `Total de Consistentes detectados Acima do VMP` > 0)
View(tabela_cobre_filtrada)
write.csv(tabela_chumbo_filtrada, "tabelas_filtradas/dados_chumbo_filtrados.csv", row.names = FALSE)
# TABELA CROMO FILTRADA ----

tabela_cromo_filtrada <- tabela_cromo |> 
  filter(Total_Detectados >= 10,
         `Total de Consistentes detectados Acima do VMP` > 0)
View(tabela_cromo_filtrada)

write.csv(tabela_cromo_filtrada, "tabelas_filtradas/dados_cromo_filtrados.csv", row.names = FALSE)


# TABELA NIQUEL FILTRADA ----

tabela_niquel_filtrada <- tabela_niquel |> 
  filter(Total_Detectados >= 10,
         `Total de Consistentes detectados Acima do VMP` > 0)
View(tabela_niquel_filtrada)

write.csv(tabela_niquel_filtrada, "tabelas_filtradas/dados_niquel_filtrados.csv", row.names = FALSE)

# TABELA NITRATO FILTRADA ----

tabela_nitrato_filtrada <- tabela_nitrato |> 
  filter(Total_Detectados >= 10,
         `Total de Consistentes detectados Acima do VMP` > 0)
View(tabela_nitrato_filtrada)

write.csv(tabela_nitrato_filtrada, "tabelas_filtradas/dados_nitrato_filtrados.csv", row.names = FALSE)


# TABELA SELENIO FILTRADA ----

tabela_selenio_filtrada <- tabela_selenio |> 
  filter(Total_Detectados >= 10,
         `Total de Consistentes detectados Acima do VMP` > 0)
View(tabela_selenio_filtrada)

write.csv(tabela_selenio_filtrada, "tabelas_filtradas/dados_selenio_filtrados.csv", row.names = FALSE)
