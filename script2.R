library(readr)

dados_sisagua_p7 <- read_csv("planilha7_pivotresult_sisagua_25_out (1).csv")
View(dados_sisagua_p7)

colnames(dados_sisagua_p7)[colnames(dados_sisagua_p7)=='município']<-'municipio'
colnames(quant_cnaes)[colnames(quant_cnaes)=='municipio_nome']<-'municipio'


dados_combinados <- dados_sisagua_p7 |> 
  left_join(quant_cnaes, by='municipio')
attach(dados_combinados)

View(dados_combinados)
### 


contagem_substancias<-dados_combinados |> 
  count(parâmetro)



dados_combinados_caracter <- dados_combinados %>%
  mutate(across(where(is.numeric), as.character))


contagem_cnaes_mun<-dados_combinados_caracter |> 
  pivot_longer(
    cols = -c(nome_da_instituição,sigla_da_instituição, cnpj_da_instituição, `nome_do_escritório_regional/local`,
              `cnpj_do_escritório_regional/local`, nome_da_forma_de_abastecimento, `nome_da_eta_/_uta`, código_ibge,
              grupo_de_parâmetros, `Total de testes substâncias em geral para cada linha - incluindo MENOR_LQ`, `Total de inconsistentes`,
              `Porcentagem de inconsistentes`, `Total de Consistentes não detectados`, `Porcentagem de Consistentes não detectados`,
              `Total de parâmetros com MENOR_LQ`, `Porcentagem de parâmetros com MENOR_LQ`, `Total de Consistentes não detectados + MENOR_LQ`,
              `Porcentagem Consistentes não detectados + MENOR_LQ`, `Total de Consistentes detectados Abaixo do VMP`, `Porcentagem de Consistentes detectados Abaixo do VMP`,
              `Total de Consistentes detectados Acima do VMP`, `Porcentagem de Consistentes detectados Acima do VMP`),
    names_to = "CNAE",
    values_to = "quantidade",
    names_transform = list(CNAE = as.character)
  ) |> 
  filter(quantidade > 0)
