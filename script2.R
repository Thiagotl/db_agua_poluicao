library(readr)
library(stringi)
library(openxlsx)

# link importante - https://www.gov.br/receitafederal/dados/municipios.csv
# link importante - https://www.ibge.gov.br/explica/codigos-dos-municipios.php
# link importante - https://cbm.nfiss.com.br/Lista_SB.php?Cadastro=116&Campo=ID7_IBGE_CTBT

dados_sisagua_p7 <- read_csv("planilha7_pivotresult_sisagua_25_out.csv") # planilha 8s
#View(dados_sisagua_p7)

colnames(dados_sisagua_p7)[colnames(dados_sisagua_p7)=="código_ibge"] <-'codigo_ibge'

colnames(dados_sisagua_p7)[colnames(dados_sisagua_p7)=="município"] <-'municipio'

dim(table(dados_sisagua_p7$municipio)) # sao 2782


attach(dados_sisagua_p7)

colnames(dados_sisagua_p7)

dados_sisagua_p7<-as.data.frame(dados_sisagua_p7)
##########################

dados_sisagua_p7_agrupados<- dados_sisagua_p7 |> 
  group_by(codigo_ibge,parâmetro,municipio,uf) |> 
  summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop")
View(dados_sisagua_p7_agrupados)

dados_sisagua_p7_agrupados <- dados_sisagua_p7_agrupados |>
  select(municipio, parâmetro, uf, codigo_ibge, 
         `Total de testes substâncias em geral para cada linha - incluindo MENOR_LQ`,
         `Total de inconsistentes`, 
         `Total de Consistentes não detectados`,
         `Total de parâmetros com MENOR_LQ`, 
         `Total de Consistentes detectados Abaixo do VMP`,
         `Total de Consistentes detectados Acima do VMP`)

# GRUPAR O DATASET DOS DADOS DO SISAGUA POR MUNICIPIO E PARAMETRO

# PROPORCAO DE (ABAIXO DO VMP+ACIMA DO VMP) / CONSISTENTE

# PROPORCAO DE  ABAIXO DO VMP / CONSISTENTE


dados_sisagua_p7_agrupados <- dados_sisagua_p7_agrupados |>
  mutate(Total_Detectados = `Total de Consistentes detectados Abaixo do VMP` + 
           `Total de Consistentes detectados Acima do VMP`,
         prop1 = (dados_sisagua_p7_agrupados$`Total de Consistentes detectados Abaixo do VMP` +
                    dados_sisagua_p7_agrupados$`Total de Consistentes detectados Acima do VMP`)/(dados_sisagua_p7_agrupados$`Total de Consistentes não detectados`+
                                                                                                   dados_sisagua_p7_agrupados$`Total de parâmetros com MENOR_LQ`+
                                                                                                   dados_sisagua_p7_agrupados$`Total de Consistentes detectados Abaixo do VMP`+
                                                                                                   dados_sisagua_p7_agrupados$`Total de Consistentes detectados Acima do VMP`),
         prop2 = dados_sisagua_p7_agrupados$`Total de Consistentes detectados Acima do VMP`/(dados_sisagua_p7_agrupados$`Total de Consistentes não detectados`+
                                                                                               dados_sisagua_p7_agrupados$`Total de parâmetros com MENOR_LQ`+
                                                                                               dados_sisagua_p7_agrupados$`Total de Consistentes detectados Abaixo do VMP`+
                                                                                               dados_sisagua_p7_agrupados$`Total de Consistentes detectados Acima do VMP`))  






dados_sisagua_p7_agrupados <- dados_sisagua_p7_agrupados |> 
  mutate(consistente = ifelse(`Total de testes substâncias em geral para cada linha - incluindo MENOR_LQ` == `Total de inconsistentes`,0,1))



dim(table(dados_sisagua_p7_agrupados$municipio)) #2782

dados_sisagua_p7_agrupados <- dados_sisagua_p7_agrupados |> 
  mutate(codigo_ibge = ifelse(uf == "DF", 530010, codigo_ibge))




# View(dados_sisagua_p7_agrupados)
# write.csv(dados_sisagua_p7_agrupados, "dados_filtrados.csv", row.names = FALSE)


# library(arrow)
# write_feather(dados_sisagua_p7_agrupados, "dados_filtrados.feather")
# objeto_filtrado <- read_feather("dados_filtrados.feather")
# View(objeto_filtrado)

# AJUSTE PARA MUNICIPIOS - TODOS EM CAIXA ALTA E SEM ACENTO

df_cnaes_primarios$municipio <- toupper(stri_trans_general(df_cnaes_primarios$municipio,
                                                           "Latin-ASCII"))


##### AJUSTE DO CÓDIGO IBGE NA TABELA df_cnaes_primarios ####

# Remover o último dígito dos códigos IBGE
df_cnaes_primarios <- df_cnaes_primarios |> 
  mutate(codigo_ibge = substr(codigo_ibge, 1, nchar(codigo_ibge) - 1))

# Exibir a tabela modificada
view(df_cnaes_primarios)

# COMBINANDO AS DUAS TABELAS - SISAGUA E CNAE PRIMARIO
# AQUI TEMOS A PLANILHA 10 !!!!!

dados_sisagua_p7_agrupados <- dados_sisagua_p7_agrupados |> 
  mutate(codigo_ibge = as.character(codigo_ibge))

df_cnaes_primarios <- df_cnaes_primarios  |> 
  mutate(codigo_ibge = as.character(codigo_ibge))

#nchar(dados_sisagua_p7_agrupados$codigo_ibge[])
#nchar(df_cnaes_primarios$codigo_ibge[])


str(dados_sisagua_p7_agrupados$codigo_ibge)
str(df_cnaes_primarios$codigo_ibge)


# DADOS SISAGUA AGRUPADOS SEM  

# REMOVER AS TRÊS CIDADES SEM CÓDIGO IBGE. - PROBLEMA FOI RESOLVIDO
# municipios_para_remover <- c("GRANJEIRO", "SANTA FILOMENA", "SOLIDAO")
# 
# dados_sisagua_p7_agrupados <- dados_sisagua_p7_agrupados %>%
#   filter(!municipio %in% municipios_para_remover)
# 
# dim(table(dados_sisagua_p7_agrupados$municipio)) # 2779


df_cnaes_primarios <- df_cnaes_primarios |> 
  select(-c(municipio))

####### PLANILHA 10 AQUIIIII #######

dados_combinados <- dados_sisagua_p7_agrupados |> 
  left_join(df_cnaes_primarios, by='codigo_ibge') 

attach(dados_combinados)

View(dados_combinados)

dim(table(dados_combinados$municipio)) # 2782


# write_csv(dados_combinados, "dados_combinado.csv")
# write_feather(dados_combinados, "dados_combinado.feather")
# 
# write_parquet(dados_combinados, "dados_combinado.parquet")

#sum(dados_combinados$prop2 == 0, na.rm = T)

write.xlsx(dados_combinados, file = "dados_combinado.xlsx")

# uma alteracao para os dados combinados

colnames(dados_combinados) <- as.character(colnames(dados_combinados))


#### PLANILHA DADOS COMBINADOS SEM TESTES INCONSISTENTES ---- 

dados_sisagua_p7_agrupados_s_inconsistentes <- dados_sisagua_p7_agrupados |> 
  filter(dados_sisagua_p7_agrupados$consistente == 1)

view(dados_sisagua_p7_agrupados_s_inconsistentes)
####### TESTANDO ALGUNS FILTROS #####

# Paramentros - Acrilamida, Antimônio, Arsênio, Bário, Cádmio, Chumbo, Cromo, 
#               Cobre, Níquel, Nitrato (como N), Selênio

filtros_cnaes<-read_csv("Poluentes_Ref._Planilha_6 e 1_Samara_04-12-2024.csv")

#### FUNÇÃO PARA EXTRAIR CNAES ----

extrair_cnaes <- function(df, parametro){
  df |> 
    filter(Parâmetro == parametro) |> 
    mutate(CNAE = as.character(CNAE)) |> 
    pull(CNAE) 
}
#cnaes_nitrato <- sub("^0+", "", cnaes_nitrato) # colocar na funcao


###### FILTRO PARA NITRATO (COMO N) ----

cnaes_nitrato<-extrair_cnaes(filtros_cnaes, "Nitrato (como N)")

cnaes_nitrato <- sub("^0+", "", cnaes_nitrato)

tabela_nitrato <- dados_combinados |> 
  filter(parâmetro == "Nitrato (como N)") |> 
  select(municipio,codigo_ibge,parâmetro,uf,
         `Total de testes substâncias em geral para cada linha - incluindo MENOR_LQ`,
         `Total de inconsistentes`, `Total de Consistentes não detectados`, 
         `Total de parâmetros com MENOR_LQ`,
         `Total de Consistentes detectados Abaixo do VMP`,
         `Total de Consistentes detectados Acima do VMP`,Total_Detectados,prop1, prop2,
         all_of(cnaes_nitrato)
         ) |> 
  arrange(municipio)

View(tabela_nitrato)


tabela_nitrato <-as.data.frame(tabela_nitrato) |> 
  rename_with(~ ifelse(grepl("^[0-9]+$", .), paste0("cnae_", .), .))


write_csv(tabela_nitrato, "planilha_nitrato_como_N.csv")

###### FILTRO PARA ACRILAMIDA ----

cnaes_acrilamida <- extrair_cnaes(filtros_cnaes, "Acrilamida")

cnaes_acrilamida <- sub("^0+","", cnaes_acrilamida)

View(cnaes_acrilamida)

tabela_acrilamida <- dados_combinados |> 
  filter(parâmetro == "Acrilamida") |> 
  select(municipio,codigo_ibge,parâmetro,uf,
         `Total de testes substâncias em geral para cada linha - incluindo MENOR_LQ`,
         `Total de inconsistentes`, `Total de Consistentes não detectados`, 
         `Total de parâmetros com MENOR_LQ`,
         `Total de Consistentes detectados Abaixo do VMP`,
         `Total de Consistentes detectados Acima do VMP`,Total_Detectados,consistente ,prop1, prop2,
         all_of(cnaes_acrilamida)) |> arrange(municipio)


tabela_acrilamida <-as.data.frame(tabela_acrilamida) |> 
  rename_with(~ ifelse(grepl("^[0-9]+$", .), paste0("cnae_", .), .))

write_csv(tabela_acrilamida, "planilha_acrilamida.csv")

tabela_acrilamida<-tabela_acrilamida |> 
  mutate(num_empresa = ifelse(cnae_2029100 > 0,1,0), # x
         deteccao= ifelse(Total_Detectados > 0,1,0))  # y

chisq.test(table(tabela_acrilamida$num_empresa,tabela_acrilamida$deteccao))
prop.table(table(tabela_acrilamida$deteccao,tabela_acrilamida$num_empresa),1)

table(tabela_acrilamida$deteccao,tabela_acrilamida$num_empresa)

table(tabela_acrilamida$deteccao, tabela_acrilamida$num_empresa)
plot(tabela_acrilamida$`Total de Consistentes detectados Acima do VMP`, tabela_acrilamida$cnae_2029100)

plot(tabela_acrilamida$cnae_2029100,tabela_acrilamida$Total_Detectados)

table(sum(tabela_acrilamida$consistente == 0))

library(vcd)  
tabela_contingencia <- table(tabela_acrilamida$num_empresa, tabela_acrilamida$deteccao)

# tamanho do efeito
v_cramer <- assocstats(tabela_contingencia)$cramer

print(v_cramer)

# # Ajustar a regressão logística
# modelo_logit <- glm(deteccao ~ num_empresa, data = tabela_acrilamida, family = binomial)
# 
# # Resumo do modelo
# summary(modelo_logit)


###### FILTRO PARA ANTIMÔNIO ----

cnaes_antimonio <- extrair_cnaes(filtros_cnaes, "Antimônio")

cnaes_antimonio <- sub("^0+","", cnaes_antimonio)

View(cnaes_antimonio)

tabela_antimonio <- dados_combinados |> 
  filter(parâmetro == "Antimônio") |> 
  select(municipio,codigo_ibge,parâmetro,uf,
         `Total de testes substâncias em geral para cada linha - incluindo MENOR_LQ`,
         `Total de inconsistentes`, `Total de Consistentes não detectados`, 
         `Total de parâmetros com MENOR_LQ`,
         `Total de Consistentes detectados Abaixo do VMP`,
         `Total de Consistentes detectados Acima do VMP`,Total_Detectados, prop1, prop2,
         all_of(cnaes_antimonio)) |> arrange(municipio)


tabela_antimonio  <-as.data.frame(tabela_antimonio ) |> 
  rename_with(~ ifelse(grepl("^[0-9]+$", .), paste0("cnae_", .), .))

write_csv(tabela_antimonio , "planilha_antimonio.csv")


###### FILTRO PARA ARSÊNI0 ----

cnaes_arsenio <- extrair_cnaes(filtros_cnaes, "Arsênio")

cnaes_arsenio <- sub("^0+","", cnaes_arsenio)

View(cnaes_arsenio)

tabela_arsenio <- dados_combinados |> 
  filter(parâmetro == "Arsênio") |> 
  select(municipio,codigo_ibge,parâmetro,uf,
         `Total de testes substâncias em geral para cada linha - incluindo MENOR_LQ`,
         `Total de inconsistentes`, `Total de Consistentes não detectados`, 
         `Total de parâmetros com MENOR_LQ`,
         `Total de Consistentes detectados Abaixo do VMP`,
         `Total de Consistentes detectados Acima do VMP`,Total_Detectados, prop1, prop2,
         all_of(cnaes_arsenio)) |> arrange(municipio)


tabela_arsenio  <-as.data.frame(tabela_arsenio ) |> 
  rename_with(~ ifelse(grepl("^[0-9]+$", .), paste0("cnae_", .), .))

write_csv(tabela_arsenio , "planilha_arsenio.csv")


###### FILTRO PARA BÁRIO ----

cnaes_bario <- extrair_cnaes(filtros_cnaes, "Bário")

cnaes_bario <- sub("^0+","", cnaes_bario)

View(cnaes_bario)

tabela_bario <- dados_combinados |> 
  filter(parâmetro == "Bário") |> 
  select(municipio,codigo_ibge,parâmetro,uf,
         `Total de testes substâncias em geral para cada linha - incluindo MENOR_LQ`,
         `Total de inconsistentes`, `Total de Consistentes não detectados`, 
         `Total de parâmetros com MENOR_LQ`,
         `Total de Consistentes detectados Abaixo do VMP`,
         `Total de Consistentes detectados Acima do VMP`,Total_Detectados, prop1, prop2,
         all_of(cnaes_bario)) |> arrange(municipio)


tabela_bario  <-as.data.frame(tabela_bario) |> 
  rename_with(~ ifelse(grepl("^[0-9]+$", .), paste0("cnae_", .), .))
View(tabela_bario)
write_csv(tabela_bario , "planilha_bario.csv")


###### FILTRO PARA CÁDMIO ----

cnaes_cadmio <- extrair_cnaes(filtros_cnaes, "Cádmio")

cnaes_cadmio <- sub("^0+","", cnaes_cadmio)

View(cnaes_cadmio)

tabela_cadmio <- dados_combinados |> 
  filter(parâmetro == "Cádmio") |> 
  select(municipio,codigo_ibge,parâmetro,uf,
         `Total de testes substâncias em geral para cada linha - incluindo MENOR_LQ`,
         `Total de inconsistentes`, `Total de Consistentes não detectados`, 
         `Total de parâmetros com MENOR_LQ`,
         `Total de Consistentes detectados Abaixo do VMP`,
         `Total de Consistentes detectados Acima do VMP`,Total_Detectados, prop1, prop2,
         all_of(cnaes_cadmio)) |> arrange(municipio)


tabela_cadmio  <-as.data.frame(tabela_cadmio) |> 
  rename_with(~ ifelse(grepl("^[0-9]+$", .), paste0("cnae_", .), .))

write_csv(tabela_cadmio , "planilha_cadmio.csv")


###### FILTRO PARA CHUMBO ----

cnaes_chumbo <- extrair_cnaes(filtros_cnaes, "Chumbo")

cnaes_chumbo <- sub("^0+","", cnaes_chumbo)

View(cnaes_chumbo)

tabela_chumbo <- dados_combinados |> 
  filter(parâmetro == "Chumbo") |> 
  select(municipio,codigo_ibge,parâmetro,uf,
         `Total de testes substâncias em geral para cada linha - incluindo MENOR_LQ`,
         `Total de inconsistentes`, `Total de Consistentes não detectados`, 
         `Total de parâmetros com MENOR_LQ`,
         `Total de Consistentes detectados Abaixo do VMP`,
         `Total de Consistentes detectados Acima do VMP`,Total_Detectados, prop1, prop2,
         all_of(cnaes_chumbo)) |> arrange(municipio)


tabela_chumbo  <-as.data.frame(tabela_chumbo) |> 
  rename_with(~ ifelse(grepl("^[0-9]+$", .), paste0("cnae_", .), .))

write_csv(tabela_chumbo , "planilha_chumbo.csv")



###### FILTRO PARA CROMO ----

cnaes_cromo <- extrair_cnaes(filtros_cnaes, "Cromo")

cnaes_cromo <- sub("^0+","", cnaes_cromo)

View(cnaes_cromo)

tabela_cromo <- dados_combinados |> 
  filter(parâmetro == "Cromo") |> 
  select(municipio,codigo_ibge,parâmetro,uf,
         `Total de testes substâncias em geral para cada linha - incluindo MENOR_LQ`,
         `Total de inconsistentes`, `Total de Consistentes não detectados`, 
         `Total de parâmetros com MENOR_LQ`,
         `Total de Consistentes detectados Abaixo do VMP`,
         `Total de Consistentes detectados Acima do VMP`,Total_Detectados, prop1, prop2,
         all_of(cnaes_cromo)) |> arrange(municipio)


tabela_cromo  <-as.data.frame(tabela_cromo) |> 
  rename_with(~ ifelse(grepl("^[0-9]+$", .), paste0("cnae_", .), .))

write_csv(tabela_cromo , "planilha_cromo.csv")




###### FILTRO PARA COBRE ----

cnaes_cobre <- extrair_cnaes(filtros_cnaes, "Cobre")

cnaes_cobre <- sub("^0+","", cnaes_cobre)

View(cnaes_cobre)

tabela_cobre <- dados_combinados |> 
  filter(parâmetro == "Cobre") |> 
  select(municipio,codigo_ibge,parâmetro,uf,
         `Total de testes substâncias em geral para cada linha - incluindo MENOR_LQ`,
         `Total de inconsistentes`, `Total de Consistentes não detectados`, 
         `Total de parâmetros com MENOR_LQ`,
         `Total de Consistentes detectados Abaixo do VMP`,
         `Total de Consistentes detectados Acima do VMP`,Total_Detectados, prop1, prop2,
         all_of(cnaes_cobre)) |> arrange(municipio)


tabela_cobre  <-as.data.frame(tabela_cobre) |> 
  rename_with(~ ifelse(grepl("^[0-9]+$", .), paste0("cnae_", .), .))

View(tabela_cobre)
write_csv(tabela_cobre , "planilha_cobre.csv")





###### FILTRO PARA NÍQUEL ----

cnaes_niquel <- extrair_cnaes(filtros_cnaes, "Níquel")

cnaes_niquel <- sub("^0+","", cnaes_niquel)

View(cnaes_niquel)

tabela_niquel <- dados_combinados |> 
  filter(parâmetro == "Níquel") |> 
  select(municipio,codigo_ibge,parâmetro,uf,
         `Total de testes substâncias em geral para cada linha - incluindo MENOR_LQ`,
         `Total de inconsistentes`, `Total de Consistentes não detectados`, 
         `Total de parâmetros com MENOR_LQ`,
         `Total de Consistentes detectados Abaixo do VMP`,
         `Total de Consistentes detectados Acima do VMP`,Total_Detectados, prop1, prop2,
         all_of(cnaes_niquel)) |> arrange(municipio)


tabela_niquel  <-as.data.frame(tabela_niquel) |> 
  rename_with(~ ifelse(grepl("^[0-9]+$", .), paste0("cnae_", .), .))

write_csv(tabela_niquel , "planilha_niquel.csv")



###### FILTRO PARA SELÊNIO ----

cnaes_selenio <- extrair_cnaes(filtros_cnaes, "Selênio")

cnaes_selenio <- sub("^0+","", cnaes_selenio)

View(cnaes_selenio)

tabela_selenio <- dados_combinados |> 
  filter(parâmetro == "Selênio") |> 
  select(municipio,codigo_ibge,parâmetro,uf,
         `Total de testes substâncias em geral para cada linha - incluindo MENOR_LQ`,
         `Total de inconsistentes`, `Total de Consistentes não detectados`, 
         `Total de parâmetros com MENOR_LQ`,
         `Total de Consistentes detectados Abaixo do VMP`,
         `Total de Consistentes detectados Acima do VMP`,Total_Detectados, prop1, prop2,
         all_of(cnaes_selenio)) |> arrange(municipio)


tabela_selenio  <-as.data.frame(tabela_selenio) |> 
  rename_with(~ ifelse(grepl("^[0-9]+$", .), paste0("cnae_", .), .))

write_csv(tabela_selenio , "planilha_selenio.csv")
