library(readr)




dados_sisagua_p7 <- read_csv("planilha7_pivotresult_sisagua_25_out.csv") # planilha 8s
View(dados_sisagua_p7)

dim(table(dados_sisagua_p7$municipio))

colnames(dados_sisagua_p7)[colnames(dados_sisagua_p7)=='município']<-'municipio'
colnames(quant_cnaes)[colnames(quant_cnaes)=='municipio_nome']<-'municipio'

View(dados_sisagua_p7)

############## TESTES ###########
teste<-dados_sisagua_p7 |> 
  group_by(municipio, parâmetro) |> 
  summarise(quantidade = n(), .groups = "drop")

teste2 <- teste |> 
  left_join(quant_cnaes, by='municipio') |> 
  filter(quantidade > 0)

dim(table(teste2$municipio))
View(teste2)


#################################################



dados_combinados <- dados_sisagua_p7 |> 
  left_join(quant_cnaes, by='municipio')
attach(dados_combinados)



# Paramentros - Acrilamida, Antimônio, Arsênio, Bário, Cádmio, Chumbo, Cromo, Cobre, Níquel, Nitrato (como N), Selênio




View(dados_combinados)
### 


contagem_substancias<-dados_combinados |> 
  count(parâmetro)



dados_combinados$`Porcentagem de Consistentes detectados Abaixo do VMP`<-dados_combinados$`Porcentagem de Consistentes detectados Abaixo do VMP`/100

dados_combinados$`Porcentagem de Consistentes detectados Acima do VMP`<-dados_combinados$`Porcentagem de Consistentes detectados Acima do VMP`/100




contagem_cnaes_mun <- dados_combinados |> 
  select(municipio, uf, parâmetro,`Porcentagem de Consistentes detectados Abaixo do VMP`,  # pegar a quantidade
         `Porcentagem de Consistentes detectados Acima do VMP`,
         "161001", "1071600", "1072402", "2022300", 
         "2029100", "2031200", "2072000", "2091600", "2093200", "729404", 
         "2019399", "2073800", "2092401", "2092403", "2093200", "2094100", 
         "2110600", "2121101", "2122000", "2449199", "2452100", "2539001", 
         "3831999", "724301", "724302", "729403", "729404", "1510600", "1610205",
         "2019399", "2312500", "2449199", "2610800", "600001", "891600", "1313800",
         "1340502", "2019399", "2031200", "2061400", "2033900", "2093200", "2211100",
         "2212900", "2219600", "2311700", "2342701", "2342702", "2349401", "2349499", 
         "2399199", "729404", "729405", "2019399", "2093200", "2522500", "2539002", 
         "2721000", "729404", "729405", "2019399", "2029100", "2033900", "2071100", 
         "2422901", "2449199", "2539001", "2539002", "2599399", "2660400", "2722801", 
         "2722802", "2910701", "2920401", "2945000", "3313902", "3831999", "111301", 
         "111302", "111303", "111399", "112101", "112102", "112199", "113000", "114800",
         "115600", "116401", "116402", "116403", "116499", "119901", "119902", "119903", 
         "119904", "119905", "119906", "119907", "119908", "119909", "119999", "121101", 
         "121102", "122900", "131800", "132600", "133401", "133402", "133403", "133404", 
         "133405", "133406", "133407", "133408", "133409", "133410", "133411", "133499", 
         "134200", "135100", "139301", "139302", "139303", "139304", "139305", "139306", 
         "139399", "141501", "141502", "142300", "161001", "161003", "210101", "210102", 
         "210103", "210104", "210105", "210106", "729404", "729405", "1340502", "1510600",
         "1610205", "2019399", "2029100", "2051700", "2093200", "2443100", "2449199", 
         "2452100", "2532201", "2539002", "2592601", "2592602", "2733300", "3831999", 
         "729404", "729405", "1510600", "1610205", "2019399", "2071100", "2093200",
         "2412100", "2423702", "2449199", "2539002", "729403", "1043100", "2019399", 
         "2093200", "2094100", "2423702", "2449199", "2452100", "2539001", "2539002", 
         "2721000", "2722801", "2722802", "3211603", "3250706", "3313902", "111301", 
         "111302", "111303", "111399", "112101", "112102", "112199", "113000", "114800",
         "115600", "116401", "116402", "116403", "116499", "119901", "119902", "119903", 
         "119904", "119905", "119906", "119907", "119908", "119909", "119999", "121101", 
         "121102", "122900", "131800", "132600", "133401", "133402", "133403", "133404", 
         "133405", "133406", "133407", "133408", "133409", "133410", "133411", "133499", 
         "134200", "135100", "139301", "139302", "139303", "139304", "139305", "139306", 
         "139399", "141501", "141502", "142300", "151201", "151202", "151203", "152101", 
         "152102", "152103", "153901", "153902", "154700", "155501", "155502", "155503",
         "155504", "155505", "159899", "161001", "161003", "210101", "210102", "210103",
         "210104", "210105", "210106", "210108", "220902", "1013901", "1052000", "2012600",
         "2013401", "2013402", "2019399", "2092401", "2092402", "2110600", "2121101", 
         "2122000", "3839401", "729404", "1066000", "2013401", "2013402", "2071100", 
         "2099101", "2121101", "2311700", "2312500", "2610800", "2829199")




contagem_cnaes_mun <- contagem_cnaes_mun |> 
  pivot_longer(
    cols = -c(parâmetro, municipio, uf, `Porcentagem de Consistentes detectados Abaixo do VMP`, 
              `Porcentagem de Consistentes detectados Acima do VMP`),
    names_to = "CNAE",
    values_to = "quantidade",
    names_transform = list(CNAE=as.character)
  ) |> 
  filter(quantidade > 0)
  


View(contagem_cnaes_mun)

# cor(contagem_cnaes_mun$`Porcentagem de Consistentes detectados Abaixo do VMP`, 
#     contagem_cnaes_mun$quantidade, method = "pearson")


cor(contagem_cnaes_mun$`Porcentagem de Consistentes detectados Abaixo do VMP`, 
    contagem_cnaes_mun$quantidade, method = "spearman")



cor(contagem_cnaes_mun$`Porcentagem de Consistentes detectados Acima do VMP`, 
    contagem_cnaes_mun$quantidade, method = "spearman")


# dados_combinados_caracter <- dados_combinados %>%
#   mutate(across(where(is.numeric), as.character))


# contagem_cnaes_mun<-dados_combinados_caracter |> 
#   pivot_longer(
#     cols = -c(nome_da_instituição,sigla_da_instituição, cnpj_da_instituição, `nome_do_escritório_regional/local`,
#               `cnpj_do_escritório_regional/local`, nome_da_forma_de_abastecimento, `nome_da_eta_/_uta`, código_ibge,
#               grupo_de_parâmetros, `Total de testes substâncias em geral para cada linha - incluindo MENOR_LQ`, `Total de inconsistentes`,
#               `Porcentagem de inconsistentes`, `Total de Consistentes não detectados`, `Porcentagem de Consistentes não detectados`,
#               `Total de parâmetros com MENOR_LQ`, `Porcentagem de parâmetros com MENOR_LQ`, `Total de Consistentes não detectados + MENOR_LQ`,
#               `Porcentagem Consistentes não detectados + MENOR_LQ`, `Total de Consistentes detectados Abaixo do VMP`, `Porcentagem de Consistentes detectados Abaixo do VMP`,
#               `Total de Consistentes detectados Acima do VMP`, `Porcentagem de Consistentes detectados Acima do VMP`),
#     names_to = "CNAE",
#     values_to = "quantidade",
#     names_transform = list(CNAE = as.character)
#   ) |> 
#   filter(quantidade > 0)




