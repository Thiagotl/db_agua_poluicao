
# ANALISE DESCRITIVA

library(tidyverse)
library(readxl)
library(flextable)


dados_combinado <- read_excel("dados_combinado.xlsx")
View(dados_combinado)

attach(dados_combinado)

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

#Tabelas solicitadas --------



planilha_arsenio <- read_excel("planilhas_parametros/planilha_arsenio.xlsx")

planilha_arsenio<-planilha_arsenio |> 
  mutate(num_empresa=ifelse(total_cnaes>0,1,0),
         deteccao=ifelse(`Total de Consistentes detectados Acima do VMP`>=1,1,0)) |> 
  filter((`Total de inconsistentes`==`Total de testes substâncias em geral para cada linha - incluindo MENOR_LQ`)== 0)


chisq.test(table(num_empresa=planilha_arsenio$num_empresa, deteccao=planilha_arsenio$deteccao), correct = F)

tabela<-table(num_empresa=planilha_arsenio$num_empresa, deteccao=planilha_arsenio$deteccao)
prop.table(tabela,1)*100


planilha_arsenio_teste<-planilha_arsenio |> 
  mutate(num_empresa=ifelse(total_cnaes>0,1,0),
         deteccao=ifelse(`Total de inconsistentes`==`Total de testes substâncias em geral para cada linha - incluindo MENOR_LQ`,1,0))
deteccao=planilha_arsenio_teste$deteccao
num_empresa=planilha_arsenio_teste$num_empresa

table(deteccao,num_empresa)
table(deteccao)

planilha_arsenio_teste2<-planilha_arsenio |> 
  filter((`Total de inconsistentes`==`Total de testes substâncias em geral para cada linha - incluindo MENOR_LQ`)== 0) |> 
  filter(`Total de Consistentes detectados Acima do VMP`==0) |> 
  mutate(num_empresa=ifelse(total_cnaes>0,1,0),
         deteccao=`Total de Consistentes detectados Abaixo do VMP`>0)

num_empresa2<-planilha_arsenio_teste2$num_empresa
deteccao2<-planilha_arsenio_teste2$deteccao
table(deteccao2, num_empresa2)

table(deteccao2)
dim(planilha_arsenio_teste2)



tabela_qui<-table(planilha_arsenio$deteccao,planilha_arsenio$num_empresa)
  
  
round(prop.table(table(planilha_arsenio$deteccao,planilha_arsenio$num_empresa))*100,2)


planilha_chumbo <- read_excel("planilhas_parametros/planilha_chumbo.xlsx")


planilha_chumbo<-planilha_chumbo |> 
  mutate(num_empresa=ifelse(total_cnaes>0,1,0),
         deteccao=ifelse(`Total de Consistentes detectados Acima do VMP`>=1,1,0))

tabela_qui2<-prop.table(table(planilha_chumbo$deteccao,planilha_chumbo$num_empresa))*100

chisq.test(tabela_qui2)
###chumbo teste
planilha_chumbo_teste<-planilha_chumbo |> 
  mutate(testes_totais= (`Total de testes substâncias em geral para cada linha - incluindo MENOR_LQ`- total))




###





planilha6_pivotresult <- read_excel("planilha6_pivotresult_sisagua_9_nov.xlsx")

planilha6_pivotresult<-planilha6_pivotresult |> 
  group_by(município, parâmetro) |> 
  summarise(quant_total = n(), .groups = 'drop') |> 
  arrange(município, parâmetro)

tabelapla6<-as.data.frame(table(planilha6_pivotresult$parâmetro)) |> 
  rename(Parâmetro = Var1, Quantidade = Freq) |> 
  arrange(desc(Quantidade))


tabela_final6 <- tabelapla6  |> 
  flextable() |> 
  set_header_labels(
    Parâmetro = "Parâmetro",
    Quantidade = "Quantidade Total"
  ) |> 
  theme_zebra()  |>   # Corrigido: theme_zebra() com "e", não theme_zebra()
  autofit()

