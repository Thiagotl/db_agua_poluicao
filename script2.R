library(readr)
library(stringi)

# link importante - https://www.gov.br/receitafederal/dados/municipios.csv
# link importante - https://www.ibge.gov.br/explica/codigos-dos-municipios.php
# link importante - https://cbm.nfiss.com.br/Lista_SB.php?Cadastro=116&Campo=ID7_IBGE_CTBT

dados_sisagua_p7 <- read_csv("planilha7_pivotresult_sisagua_25_out.csv") # planilha 8s
View(dados_sisagua_p7)

colnames(dados_sisagua_p7)[colnames(dados_sisagua_p7)=="código_ibge"] <-'codigo_ibge'

colnames(dados_sisagua_p7)[colnames(dados_sisagua_p7)=="município"] <-'municipio'

dim(table(dados_sisagua_p7$município)) # sao 2782
dim(table(df_pivot$municipio)) # 5246

attach(dados_sisagua_p7)

# GRUPAR O DATASET DOS DADOS DO SISAGUA POR MUNICIPIO E PARAMETRO

# PROPORCAO DE (ABAIXO DO VMP+ACIMA DO VMP) / CONSISTENTE

prop1 = (`Total de Consistentes detectados Abaixo do VMP` + 
           `Total de Consistentes detectados Acima do VMP`)/(`Total de Consistentes não detectados`+
                                                               `Total de parâmetros com MENOR_LQ`+
                                                               `Total de Consistentes detectados Abaixo do VMP`+
                                                               `Total de Consistentes detectados Acima do VMP`)
# PROPORCAO DE  ABAIXO DO VMP / CONSISTENTE
prop2= `Total de Consistentes detectados Abaixo do VMP`/(`Total de Consistentes não detectados`+
           `Total de parâmetros com MENOR_LQ`+
           `Total de Consistentes detectados Abaixo do VMP`+
           `Total de Consistentes detectados Acima do VMP`)

dados_sisagua_p7_agrupados <- dados_sisagua_p7 |>
  select(municipio, parâmetro, uf, codigo_ibge, 
         `Total de testes substâncias em geral para cada linha - incluindo MENOR_LQ`,
         `Total de inconsistentes`, 
         `Total de Consistentes não detectados`,
         `Total de parâmetros com MENOR_LQ`, 
         `Total de Consistentes detectados Abaixo do VMP`,
         `Total de Consistentes detectados Acima do VMP`) |> 
  mutate(Total_Detectados = `Total de Consistentes detectados Abaixo do VMP` + 
           `Total de Consistentes detectados Acima do VMP`,
         prop1 = prop1,
         prop2 = prop2) 


dados_sisagua_p7_agrupados<- dados_sisagua_p7_agrupados |> 
    group_by(codigo_ibge, parâmetro, municipio, uf) |> 
    summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop")

dim(table(dados_sisagua_p7_agrupados$municipio)) #2782

dados_sisagua_p7_agrupados <- dados_sisagua_p7_agrupados |> 
  mutate(codigo_ibge = ifelse(uf == "DF", 530010, codigo_ibge))




View(dados_sisagua_p7_agrupados)
write.csv(dados_sisagua_p7_agrupados, "dados_filtrados.csv", row.names = FALSE)


#library(arrow)
#write_feather(dados_sisagua_p7_agrupados, "dados_filtrados.feather")
# objeto_filtrado <- read_feather("dados_sisagua_p7_agrupados.feather")


# AJUSTE PARA CNAES - TODOS EM CAIXA ALTA E SEM ACENTO



df_cnaes_primarios$municipio <- toupper(stri_trans_general(df_cnaes_primarios$municipio,
                                                           "Latin-ASCII"))


view(df_cnaes_primarios)

#############################
### AJUSTE DO CÓDIGO IBGE NA TABELA df_cnaes_primarios ###



# Remover o último dígito dos códigos IBGE
df_cnaes_primarios <- df_cnaes_primarios |> 
  mutate(codigo_ibge = substr(codigo_ibge, 1, nchar(codigo_ibge) - 1))

# Exibir a tabela modificada
view(df_cnaes_primarios)


# AJUSTE PARA TABELA dados_sisagua_p7_agrupados

# tabela_A_corrigida <- dados_sisagua_p7_agrupados  |> 
#   mutate(
#     codigo_completo = sapply(codigo_ibge, function(cod) {
#       match_code <- df_cnaes_primarios$codigo_ibge[str_detect(df_cnaes_primarios$codigo_ibge, paste0("^", cod))]
#       if (length(match_code) > 0) {
#         return(match_code)  # Retorna o código completo encontrado
#       } else {
#         return(NA)  # Caso não encontre um correspondente
#       }
#     }),
#     digito_faltante = ifelse(!is.na(codigo_completo), substr(codigo_completo, 7, 7), NA)
#   )
# 
# View(tabela_A_corrigida)
# 
# dim(table(tabela_A_corrigida$municipio)) #2782
# 
# any(is.na(tabela_A_corrigida))
# 
# sum(is.na(tabela_A_corrigida))
# 
# names(tabela_A_corrigida)[colSums(is.na(tabela_A_corrigida)) > 0]
# 
# tabela_NA<-tabela_A_corrigida[!complete.cases(tabela_A_corrigida), ]
# 
# View(tabela_NA)
# 
# table(tabela_NA$municipio, tabela_NA$uf)

#########################################


# COMBINANDO AS DUAS TABELAS - SISAGUA E CNAE PRIMARIO
# AQUI TEMOS A PLANILHA 10 !!!!!

dados_sisagua_p7_agrupados <- dados_sisagua_p7_agrupados |> 
  mutate(codigo_ibge = as.character(codigo_ibge))

df_cnaes_primarios <- df_cnaes_primarios  |> 
  mutate(codigo_ibge = as.character(codigo_ibge))

nchar(dados_sisagua_p7_agrupados$codigo_ibge[])
nchar(df_cnaes_primarios$codigo_ibge[])


str(dados_sisagua_p7_agrupados$codigo_ibge)
str(df_cnaes_primarios$codigo_ibge)


# REMOVER AS TRÊS CIDADES SEM CÓDIGO IBGE. 
municipios_para_remover <- c("GRANJEIRO", "SANTA FILOMENA", "SOLIDAO")

dados_sisagua_p7_agrupados <- dados_sisagua_p7_agrupados %>%
  filter(!municipio %in% municipios_para_remover)

dim(table(dados_sisagua_p7_agrupados$municipio)) # 2779


df_cnaes_primarios <- df_cnaes_primarios |> 



dados_combinados <- dados_sisagua_p7_agrupados |> 
  left_join(df_cnaes_primarios, by='codigo_ibge') 

attach(dados_combinados)



View(dados_combinados)



dim(table(dados_combinados$municipio.x)) # 

tabela_b<-dados_combinados[!complete.cases(dados_combinados), ]
View(tabela_b)







### TESTANDO ALGUNS FILTROS 

# Paramentros - Acrilamida, Antimônio, Arsênio, Bário, Cádmio, Chumbo, Cromo, Cobre, Níquel, Nitrato (como N), Selênio


filtros_cnaes<-read_csv("Poluentes_Ref._Planilha_6 e 1_Samara_04-12-2024.csv")

# filtro cnaes - NITRATO 
cnaes_nitrato<-filtros_cnaes |> 
  filter(Parâmetro == "Nitrato (como N)") |> 
  select(CNAE)

# filtor cnaes - Acrilamida

cnaes_acrilamida<-filtros_cnaes |> 
  filter(Parâmetro == "Acrilamida") |> 
  select(CNAE)


# filtor cnaes - Acrilamida

cnaes_acrilamida<-filtros_cnaes |> 
  filter(Parâmetro == "Acrilamida") |> 
  select(CNAE)




















tabela_nitrato <- dados_combinados |> 
  filter(parâmetro == "Nitrato (como N)") |> 
  select(municipio,parâmetro,`Total de testes substâncias em geral para cada linha - incluindo MENOR_LQ` ,
         `Total de inconsistentes`, `Total de Consistentes não detectados`, `Total de parâmetros com MENOR_LQ`,
         `Total de Consistentes detectados Abaixo do VMP`,`Total de Consistentes detectados Acima do VMP`,Total_Detectados ,
         all_of(cnaes_nitrato))

  

View(tabela_nitrato)

colSums(is.na(tabela_nitrato))
sum(is.na(tabela_nitrato))
tabela_nitrato[is.na(tabela_nitrato)] <- 0

tabela_nitrato<-as.data.frame(tabela_nitrato)


t1<-cor(tabela_nitrato$`Total de Consistentes detectados Acima do VMP`, tabela_nitrato[,-c(1,2,)])

t1

cor.test(tabela_nitrato$`Total de Consistentes detectados Acima do VMP`, tabela_nitrato$`111302`)

t2<-cor(tabela_nitrato$`Total de Consistentes detectados Abaixo do VMP`, tabela_nitrato[,-c(1,2)])

plot(t2)
View(t2)

zero_sd_cols <- sapply(tabela_nitrato[, -c(1:2)], function(x) sd(x, na.rm = TRUE) == 0)
zero_sd_cols  # Retorna TRUE para colunas com desvio padrão zero

#tabela_filtrada <- tabela_nitrato[, -c(1:2)][, sapply(tabela_nitrato[, -c(1:2)], sd, na.rm = TRUE) > 0]






#### TESTE PARA MATRIZ DE CORRELAÇÃO


cor_matrix <- cor(tabela_nitrato[,-c(1,2,15)], use = "pairwise.complete.obs")
View(cor_matrix)
library(heatmaply)

# Criando um heatmap interativo
heatmaply(cor_matrix, colors = colorRampPalette(c("blue", "white", "red"))(200), 
          dendrogram = "row", k_row = 5, k_col = 5)
