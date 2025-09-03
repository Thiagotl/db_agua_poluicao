
# ANALISE DESCRITIVA

library(tidyverse)
library(readxl)
library(flextable)
library(kableExtra)

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

# para verificar quais municipios fizeram um único teste pra a subtância durante todos os anos.
apenas_1teste <- dados_combinado |> 
  filter(`Total de testes substâncias em geral para cada linha - incluindo MENOR_LQ`==1) |> 
  group_by(parâmetro)

table(as.factor(apenas_1teste$municipio)) |> 
  length()

#prop.table(table(as.factor(apenas_1teste$parâmetro))) * 100
  

# dados<-dados_combinado |> 
#   filter(Total_Detectados>=1)

dados<-dados_combinado |> 
  filter(`Total de Consistentes detectados Acima do VMP`>=1) |> 
  group_by(municipio)

View(dados)


# Ao todo são 987 municípios 
dados <- dados |> 
  group_by(municipio, parâmetro) |> 
  summarise(quant_total = n(), .groups = 'drop') |> 
  arrange(municipio, parâmetro)

tabela <- as.data.frame(table(dados$parâmetro)) |> 
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

# TABELAS SOLICITADAS --------

# NESTA PARTE ESTÃO AS TABELAS SOLICITADAS PARA O RELATÓRIO FINAL


### ARSENIO ----
planilha_arsenio1 <- read_excel("planilhas_parametros/planilha_arsenio.xlsx")


# Aqui é filtrado dos consistentes - Tabela 2
planilha_arsenio<-planilha_arsenio1 |>
  mutate(num_empresa=ifelse(total_cnaes > 0, 1, 0),
         deteccao=ifelse(`Total de Consistentes detectados Acima do VMP` >= 1, 1, 0)) |>
  filter((`Total de inconsistentes`==`Total de testes substâncias em geral para cada linha - incluindo MENOR_LQ`) == 0)

chisq.test(table(num_empresa=planilha_arsenio$num_empresa, deteccao=planilha_arsenio$deteccao), correct = F)

tabela<-table(num_empresa=planilha_arsenio$num_empresa, deteccao=planilha_arsenio$deteccao)
prop.table(tabela,1) * 100



####### sem utlidade ####
# planilha_arsenio_teste<-planilha_arsenio |>
#   mutate(num_empresa=ifelse(total_cnaes>0,1,0),
#          deteccao=ifelse(`Total de inconsistentes`==`Total de testes substâncias em geral para cada linha - incluindo MENOR_LQ`,1,0))
# deteccao=planilha_arsenio_teste$deteccao
# num_empresa=planilha_arsenio_teste$num_empresa
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
a <- table(deteccao2,num_empresa2)
a[1,2]

# NÚMERO DE MUNICIPIOS SEM DETECÇÃO // NUMERO DE MUNICIPIOS COM UM OU MAIS CNAES RELACIONADOS SEM DETECÇÃO
table(deteccao2)[2]

#tabela_qui<-table(planilha_arsenio$deteccao,planilha_arsenio$num_empresa)
#round(prop.table(table(planilha_arsenio$deteccao,planilha_arsenio$num_empresa))*100,2)


# FUNCAO ULTRA MEGA POWER QUE RESOLVE TUDO E MAIS UM POUCO 

funcao_descritiva <- function(df){
  
  # RESULTADOS TABELA 2
  tabela2 <- df |>
    mutate(num_empresa=ifelse(total_cnaes > 0, 1, 0),
           deteccao=ifelse(`Total de Consistentes detectados Acima do VMP` >= 1, 1, 0)) |>
    filter((`Total de inconsistentes`==`Total de testes substâncias em geral para cada linha - incluindo MENOR_LQ`) == 0) 
    
    tabela2 <- table(num_empresa=tabela2$num_empresa, deteccao=tabela2$deteccao)
    rownames(tabela2) <- c("Não possui CNAE Relacionado","Possui CNAE Relacionado")
    colnames(tabela2) <- c("Não tem detecção", "Tem detecção Acima do VMP")
    
    tabela_html2 <- tabela2 |>
      as.data.frame.matrix() |>
      kbl(align = "c", caption = "Relação entre CNAE e Detecção") |>
      kable_styling(
        bootstrap_options = c("striped", "hover", "condensed"),
        full_width = FALSE,
        position = "center"
      ) |>
      add_header_above(c(" " = 1, "Status de Detecção" = 2)) |>
      row_spec(0, bold = TRUE, background = "#f8f9fa")

    resultado1 <-tabela2[1,2] + tabela2[2,2]
    resultado2 <- tabela2[2,2]
    # RESULTADOS TABELA 1
    
    planilha1 <- df |> 
      filter((`Total de inconsistentes`==`Total de testes substâncias em geral para cada linha - incluindo MENOR_LQ`) != 0) |> 
      filter(`Total de Consistentes detectados Acima do VMP` == 0)
    res_p1 <- dim(planilha1)[1]

    planilha2 <- planilha1 |> 
      filter(`total_cnaes` > 0) 
    res_p2 <- dim(planilha2)[1]
    
    planilha3<-df |> 
      filter((`Total de inconsistentes`==`Total de testes substâncias em geral para cada linha - incluindo MENOR_LQ`) == 0) |> 
      filter(`Total de Consistentes detectados Acima do VMP` == 0) |> 
      mutate(num_empresa=ifelse(total_cnaes > 0, 1, 0),
             deteccao=`Total de Consistentes detectados Abaixo do VMP` > 0)
    
    tabela_p3 <- table(planilha3$deteccao, planilha3$num_empresa)
    res_tab_p3 <- tabela_p3[2,2]
    res2_tab_p3 <- tabela_p3[1,2]
    
    tabela2_p3 <-table(planilha3$deteccao)
    res_tab2_p3 <- tabela2_p3[1]
    res2_tab2_p3 <- tabela2_p3[2]
    
    df_tabela <- data.frame(
      ELEMENTO = c(
        "Tem detecção Acima do VMP",
        "Detecção abaixo VMP sem acima VMP",
        "Não tem detecções",
        "Somente testes inconsistentes"
      ),
      "N mun" = c(
        resultado1,
        res2_tab2_p3,
        res_tab2_p3,
        res_p1
      ),
      "N mun com 1/+ CNAES rela" = c(
        resultado2,
        res_tab_p3,
        res2_tab_p3,
        res_p2
      ),
      stringsAsFactors = FALSE,
      check.names = FALSE  
    )
    
    tabela_html1 <- df_tabela 
    
    tabela_html1 <- df_tabela |>
      kbl(
        align = "c",
        col.names = c("ELEMENTO", "N mun", "N mun com 1/+ CNAES rela"),
        caption = "Relação entre CNAE e Detecção"
      ) |>
      kable_styling(
        bootstrap_options = c("striped", "hover", "condensed"),
        full_width = FALSE,
        position = "center"
      ) |>
      add_header_above(c(" " = 1, "Status de Detecção" = 2)) |>
      row_spec(0, bold = TRUE, background = "#f8f9fa")
    
    return(list(tabela_html1 = tabela_html1, tabela_html2 = tabela_html2))
    
}

teste_funcao<-funcao_descritiva(planilha_arsenio1)
teste_funcao$tabela_html1
teste_funcao$tabela_html2
library(tidyverse)

### CHUMBO----

planilha_chumbo <- read_excel("planilhas_parametros/planilha_chumbo.xlsx")

resultados_chumbo <- funcao_descritiva(planilha_chumbo)
resultados_chumbo$tabela_html1
resultados_chumbo$tabela_html2



planilha_chumbo<-planilha_chumbo |>
  mutate(num_empresa=ifelse(total_cnaes > 0, 1, 0),
         deteccao=ifelse(`Total de Consistentes detectados Acima do VMP` >= 1, 1, 0)) |>
  filter((`Total de inconsistentes`==`Total de testes substâncias em geral para cada linha - incluindo MENOR_LQ`) == 0)


tabela<-table(num_empresa=planilha_chumbo$num_empresa, deteccao=planilha_chumbo$deteccao)


planilha_nitrato <- read_excel("planilhas_parametros/planilha_nitrato_como_N.xlsx")
resultados_nitrato <- funcao_descritiva(planilha_nitrato)

resultados_nitrato$tabela_html1
resultados_nitrato$tabela_html2



planilha_nitrato<-planilha_nitrato |>
  mutate(num_empresa=ifelse(total_cnaes > 0, 1, 0),
         deteccao=ifelse(`Total de Consistentes detectados Acima do VMP` >= 1, 1, 0)) |>
  filter((`Total de inconsistentes`==`Total de testes substâncias em geral para cada linha - incluindo MENOR_LQ`) == 0)


tabela<-table(num_empresa=planilha_nitrato$num_empresa, deteccao=planilha_nitrato$deteccao)




# # Detecção acima do VMP e numero de muncípios
# resultado1<-df |> 
#   filter((`Total de inconsistentes`==`Total de testes substâncias em geral para cada linha - incluindo MENOR_LQ`) == 0) |> 
#   filter(`Total de Consistentes detectados Acima do VMP` > 0) 
# 
# # Detecção acima do VMP e num de municipios com um ou mais CNAES relacionados
# resultado2 <- resultado1 |> 
#   filter(`total_cnaes` > 0) 
# 
# # # # # # # #
# 
# resultado1_1<-df |> 
#   filter((`Total de inconsistentes`==`Total de testes substâncias em geral para cada linha - incluindo MENOR_LQ`) != 1) |> 
#   filter(`Total de Consistentes detectados Acima do VMP` == 0) 
# 
# # MUNICIPIOS QUE NÃO TEM DETECÇÃO E NÃO POSSUEM CNAE RELACIONADO
# resultado2_2 <- resultado1_1 |> 
#   filter(`total_cnaes` > 0) 
#   
# 
# # # # # # 
# resultado1_3<-df |>
#   filter((`Total de inconsistentes`==`Total de testes substâncias em geral para cada linha - incluindo MENOR_LQ`) != 1) |>
#   filter(`Total de Consistentes detectados Acima do VMP`== 1)
# 
# # Não possui CNAE relacionado e tem deteção acima do VMP
# resultado2_3 <- resultado1_3 |>
#   filter(`total_cnaes` > 0)
# 
# resultado3 <- df |> 
#   filter((`Total de inconsistentes`==`Total de testes substâncias em geral para cada linha - incluindo MENOR_LQ`) == 0) |> 
#   filter(`Total de Consistentes detectados Acima do VMP` == 0) |> 
#   mutate(num_empresa=ifelse(total_cnaes > 0, 1, 0),
#          deteccao=`Total de Consistentes detectados Abaixo do VMP` > 0) 
# tabela_resultado3 <- table(resultado3$deteccao,resultado3$num_empresa)
# tabela_resultado3_1 <- table(resultado3$deteccao)


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

# PARA ENCONTAR O VALOR 61 USE O SEGUINTE SCRIPT

# resultado1<-df |> 
#   filter((`Total de inconsistentes`==`Total de testes substâncias em geral para cada linha - incluindo MENOR_LQ`) == 0) |> 
#   filter(`Total de Consistentes detectados Acima do VMP`> 0) 
# 
# resultado2 <- resultado1 |> 
#   filter(`total_cnaes` > 0)


