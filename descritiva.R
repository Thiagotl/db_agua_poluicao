
# ANALISE DESCRITIVA

library(tidyverse)
library(readxl)
library(flextable)


dados_combinado <- read_excel("dados_combinados_planilha10.xlsx")
View(dados_combinado)

attach(dados_combinado)


x<-dados_combinado |> 
  group_by(municipio) |> 
  summarise(n=n()) |> 
  arrange(n)

View(x)
dim(x)

round(prop.table(table(dados_combinado$grupo_de_parâmetros))*100,2)

dados_combinado_arsenio<-dados_combinado |> 
  filter(parâmetro == "Arsênio")


# dados<-dados_combinado |> 
#   filter(Total_Detectados>=1)

dados<-dados_combinado |> 
  filter(`Total de Consistentes detectados Acima do VMP`>=1)

View(dados)

dados<-dados |> 
  group_by(municipio, parâmetro) |> 
  summarise(quant_total = n(), .groups = 'drop') |> 
  arrange(municipio, parâmetro)

tabela<-as.data.frame(table(dados$parâmetro)) |> 
  rename(Parâmetro = Var1, Quantidade = Freq) |> 
  arrange(desc(Quantidade))


tabela_final <- tabela %>%
  flextable() %>%
  set_header_labels(
    Parâmetro = "Parâmetro",
    Quantidade = "Quantidade Total"
  ) %>%
  theme_zebra() %>%  # Corrigido: theme_zebra() com "e", não theme_zebra()
  autofit()

library(officer)

doc <- read_docx() %>%  
  body_add_flextable(tabela_final)

print(doc, target = "contagem_parametros.docx")


##filtro para 3 ou mais susbtâncias----


dados2<-dados_combinado |> 
  filter(`Total de Consistentes detectados Acima do VMP`>=3)


dados2<-dados2 |> 
  group_by(municipio, parâmetro) |> 
  summarise(quant_total = n(), .groups = 'drop') |> arrange(municipio, parâmetro)


tabela2<-as.data.frame(table(dados2$parâmetro)) |> 
  rename(Parâmetro = Var1, Quantidade = Freq) |> 
  arrange(desc(Quantidade))


tabela_final2 <- tabela2 %>%
  flextable() %>%
  set_header_labels(
    Parâmetro = "Parâmetro",
    Quantidade = "Quantidade Total"
  ) %>%
  theme_zebra() %>%  # Corrigido: theme_zebra() com "e", não theme_zebra()
  autofit()


doc2 <- read_docx() %>%  
  body_add_flextable(tabela_final2)

print(doc2, target = "contagem2_parametros.docx")
#########

dados3<-dados_combinado |> 
  filter(`Total de Consistentes detectados Acima do VMP`>=8)


dados3<-dados3 |> 
  group_by(municipio, parâmetro) |> 
  summarise(quant_total = n(), .groups = 'drop') |> arrange(municipio, parâmetro)


tabela3<-as.data.frame(table(dados3$parâmetro)) |> 
  rename(Parâmetro = Var1, Quantidade = Freq) |> 
  arrange(desc(Quantidade))


tabela_final3 <- tabela3 %>%
  flextable() %>%
  set_header_labels(
    Parâmetro = "Parâmetro",
    Quantidade = "Quantidade Total"
  ) %>%
  theme_zebra() %>%  # Corrigido: theme_zebra() com "e", não theme_zebra()
  autofit()


doc3 <- read_docx() %>%  
  body_add_flextable(tabela_final3)

print(doc3, target = "contagem3_parametros.docx")


###  Planilhas 1 e 6 e casos graves-----

casos_graves<-dados_combinado |> 
  filter(Total_Detectados>=10,
         `Total de Consistentes detectados Acima do VMP`>=1) |> 
  group_by(municipio, parâmetro)


#planilha 1 

planilha1<-dados_combinado |> 
  filter(`Total de Consistentes detectados Abaixo do VMP`>=11,
         `Total de Consistentes detectados Acima do VMP`>=1) |> 
  group_by(municipio, parâmetro)
        
#planilha 6
planilha6<-dados_combinado |> 
  filter(`Total de Consistentes detectados Acima do VMP`>0) |> 
  group_by(municipio, parâmetro)

# ## planilha6 teste
# planilha6_teste<-planilha6 |> 
#   group_by(municipio, parâmetro) |> 
#   summarise(quant_total = n(), .groups = 'drop') |> 
#   arrange(municipio, parâmetro)
# tabela_final6<-as.data.frame(table(planilha6_teste$parâmetro)) |> 
#   rename(Parâmetro = Var1, Quantidade = Freq) |> 
#   arrange(desc(Quantidade))
# 
# 
# tabela_final <- tabela %>%
#   flextable() %>%
#   set_header_labels(
#     Parâmetro = "Parâmetro",
#     Quantidade = "Quantidade Total"
#   ) %>%
#   theme_zebra() %>%  # Corrigido: theme_zebra() com "e", não theme_zebra()
#   autofit()

##
library(writexl)

write_xlsx(lista_municipios, "municipios_join_cnpj_arsenio_completos_teste4.xlsx")


library(openxlsx)

# Criar uma lista com os nomes dos seus objetos e os próprios objetos
lista_objetos <- list(
  "caso_graves" = casos_graves,
  "planilha1" = planilha1,
  "planilha6" = planilha6
)

# Salvar em um arquivo Excel com várias abas
write.xlsx(
  lista_objetos,
  file = "planilhas_01_06_casos_graves.xlsx",
  asTable = TRUE)  # Opcional: TRUE se quiser formatar como tabela Excel



# planilha6_pivotresult <- read_excel("planilha6_pivotresult_sisagua_9_nov.xlsx")
# 
# planilha6_pivotresult<-planilha6_pivotresult |> 
#   group_by(município, parâmetro) |> 
#   summarise(quant_total = n(), .groups = 'drop') |> 
#   arrange(município, parâmetro)
# 
# tabelapla6<-as.data.frame(table(planilha6_pivotresult$parâmetro)) |> 
#   rename(Parâmetro = Var1, Quantidade = Freq) |> 
#   arrange(desc(Quantidade))
# 
# 
# tabela_final6 <- tabelapla6  |> 
#   flextable() |> 
#   set_header_labels(
#     Parâmetro = "Parâmetro",
#     Quantidade = "Quantidade Total"
#   ) |> 
#   theme_zebra()  |>   # Corrigido: theme_zebra() com "e", não theme_zebra()
#   autofit()



#Tabelas solicitadas --------

### ARSENIO ----
planilha_arsenio1 <- read_excel("planilhas_parametros/planilha_arsenio.xlsx")


# Aqui é filtrado dos consistentes 
planilha_arsenio<-planilha_arsenio1 |>
  mutate(num_empresa=ifelse(total_cnaes > 0, 1, 0),
         deteccao=ifelse(`Total de Consistentes detectados Acima do VMP` >= 1, 1, 0)) |>
  filter((`Total de inconsistentes`==`Total de testes substâncias em geral para cada linha - incluindo MENOR_LQ`) == 0)


chisq.test(table(num_empresa=planilha_arsenio$num_empresa, deteccao=planilha_arsenio$deteccao), correct = F)

tabela<-table(num_empresa=planilha_arsenio$num_empresa, deteccao=planilha_arsenio$deteccao)
prop.table(tabela,1)*100


# planilha_arsenio_teste<-planilha_arsenio |> 
#   mutate(num_empresa=ifelse(total_cnaes>0,1,0),
#          deteccao=ifelse(`Total de inconsistentes`==`Total de testes substâncias em geral para cada linha - incluindo MENOR_LQ`,1,0))
# deteccao=planilha_arsenio_teste$deteccao
# num_empresa=planilha_arsenio_teste$num_empresa
# 
# table(deteccao,num_empresa)
# table(deteccao)



# NUMERO DE MUNICIPIOS SOMENTE TESTES INCONSISTENTES (NUMERO DE LINHAS)
planilha_arsenio_teste3<-planilha_arsenio1 |> 
  filter((`Total de inconsistentes`==`Total de testes substâncias em geral para cada linha - incluindo MENOR_LQ`) != 0) |> 
  filter(`Total de Consistentes detectados Acima do VMP` == 0) 

# NUMERO DE MUNICIPIOS COM UM MAIS CNAE RELACINADO E POSSUEM SOMENTE TESTES INCONSISTENTES
planilha_arsenio_teste4<-planilha_arsenio_teste3 |> 
  filter(`total_cnaes`>0) 
# NÚMERO TOTAL DE MUNICÍPIOS COM DETECÇÃO
planilha_arsenio_teste2<-planilha_arsenio1 |> 
  filter((`Total de inconsistentes`==`Total de testes substâncias em geral para cada linha - incluindo MENOR_LQ`) == 0) |> 
  filter(`Total de Consistentes detectados Acima do VMP` == 0) |> 
  mutate(num_empresa=ifelse(total_cnaes > 0, 1, 0),
         deteccao=`Total de Consistentes detectados Abaixo do VMP` > 0)
num_empresa2<-planilha_arsenio_teste2$num_empresa
deteccao2<-planilha_arsenio_teste2$deteccao

# NÚMERO DE MUNICIPIOS COM UM OU MAIS CNAES RELACIONADOS E NÃO TEM DETECÇÃO 
# NÚMERO DE MUNICIPIOS COM UM OU MAIS CNAES RELACIONADOS E TEM DETECÇÃO ABAIXO DO VMP MAS NÃO TEM ACIMA DO VMP
table(deteccao2,num_empresa2)

# NÚMERO DE MUNICIPIOS SEM DETECÇÃO // NUMERO DE MUNICIPIOS COM UM OU MAIS CNAES RELACIONADOS SEM DETECÇÃO
table(deteccao2)

#tabela_qui<-table(planilha_arsenio$deteccao,planilha_arsenio$num_empresa)
#round(prop.table(table(planilha_arsenio$deteccao,planilha_arsenio$num_empresa))*100,2)

funcao <- function(df){
  resultado1<-df |> 
    filter((`Total de inconsistentes`==`Total de testes substâncias em geral para cada linha - incluindo MENOR_LQ`) == 0) |> 
    filter(`Total de Consistentes detectados Acima do VMP`>1) 
  
  resultado2 <- resultado1 |> 
    filter(`total_cnaes` > 0) 
  
  resultado3 <- df |> 
    filter((`Total de inconsistentes`==`Total de testes substâncias em geral para cada linha - incluindo MENOR_LQ`) == 0) |> 
    filter(`Total de Consistentes detectados Acima do VMP` == 0) |> 
    mutate(num_empresa=ifelse(total_cnaes > 0, 1, 0),
           deteccao=`Total de Consistentes detectados Abaixo do VMP` > 0) 
  tabela_resultado3<-table(resultado3$deteccao,resultado3$num_empresa)
    
  list(
    resultado1 = resultado1,
    resultado2 = resultado2,
    resultado3 = resultado3,
    tabela_resultado3 = tabela_resultado3
  )
  
}

teste_funcao<-funcao(planilha_arsenio1)

teste_funcao$resultado1

teste_funcao$resultado2

teste_funcao$resultado3

teste_funcao$tabela_resultado3


# MUNICIPIOS QUE NÃO TEM DETECÇÃO E NÃO POSSUEM CNAE RELACIONADO - USAR O SCRIPT ABAIXO (746)
# resultado1<-df |> 
#   filter((`Total de inconsistentes`==`Total de testes substâncias em geral para cada linha - incluindo MENOR_LQ`) != 1) |> 
#   filter(`Total de Consistentes detectados Acima do VMP`== 0) 
# 
# resultado2 <- resultado1 |> 
#   filter(`total_cnaes` > 0)

# TEM DETECÇÃO ACIMA DO VMP E NÃO POSSUI CNAE RELACIONADO (38)
# resultado1<-df |> 
#   filter((`Total de inconsistentes`==`Total de testes substâncias em geral para cada linha - incluindo MENOR_LQ`) != 1) |> 
#   filter(`Total de Consistentes detectados Acima do VMP`== 1) 
# 
# resultado2 <- resultado1 |> 
#   filter(`total_cnaes` > 0) 

# PARA ACHAR O VALOR 1259 DEVE-SE SOMAR OS VALORES DA PRIMEIRA COLUNA DA TABELA RESULTADO 3
# PARA ACHAR O VALOR 746 DEVE-SE SUBTRAIR OS VALORES DA SEGUNDA COLUNA DA TABELA RESULTADO 3



### CHUMBO----


planilha_chumbo <- read_excel("planilhas_parametros/planilha_chumbo.xlsx")

planilha_chumbo1<-planilha_chumbo |>
  mutate(num_empresa=ifelse(total_cnaes>0,1,0),
         deteccao=ifelse(`Total de Consistentes detectados Acima do VMP`>=1,1,0)) |>
  filter((`Total de inconsistentes`==`Total de testes substâncias em geral para cada linha - incluindo MENOR_LQ`)== 0)



tabela<-table(num_empresa=planilha_chumbo1$num_empresa, deteccao=planilha_chumbo1$deteccao)
prop.table(tabela,1)*100






