library(readr)
# link importante - https://www.gov.br/receitafederal/dados/municipios.csv
dados_sisagua_p7 <- read_csv("planilha7_pivotresult_sisagua_25_out.csv") # planilha 8s
View(dados_sisagua_p7)

dim(table(dados_sisagua_p7$município)) # sao 2782
dim(table(df_pivot$municipio)) # 5246

colnames(dados_sisagua_p7)[colnames(dados_sisagua_p7)=="código_ibge"] <-'codigo_ibge'



# GRUPAR O DATASET DOS DADOS DO SISAGUA POR MUNICIPIO E PARAMETRO
# dados_sisagua_p7_agrupados<-dados_sisagua_p7 |> 
#   group_by(municipio, uf, parâmetro) |> 
#   summarise(contagem_parametro = n(), .groups = "drop")
# 
# view(dados_sisagua_p7_agrupados)

dados_sisagua_p7_agrupados <- dados_sisagua_p7 |>
  select(município, parâmetro, uf, codigo_ibge, `Total de testes substâncias em geral para cada linha - incluindo MENOR_LQ`,
         `Total de inconsistentes`, `Total de Consistentes não detectados`,`Total de parâmetros com MENOR_LQ`, `Total de Consistentes detectados Abaixo do VMP`,
         `Total de Consistentes detectados Acima do VMP`) |> 
  mutate(Total_Detectados = `Total de Consistentes detectados Abaixo do VMP` + 
           `Total de Consistentes detectados Acima do VMP`) 
  
dados_sisagua_p7_agrupados<- dados_sisagua_p7_agrupados |> 
    group_by(codigo_ibge, parâmetro, município) |> 
    summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop")

dim(table(dados_sisagua_p7_agrupados$município)) #2782

view(dados_sisagua_p7_agrupados)

# COMBINANDO AS DUAS TABELAS - SISAGUA E CNAE PRIMARIO
# AQUI TEMOS A PLANILHA 10 !!!!!

dados_combinados <- dados_sisagua_p7_agrupados |> 
  left_join(df_pivot, by='codigo_ibge') 

attach(dados_combinados)

View(dados_combinados)

View(df_pivot)

dim(table(dados_combinados$municipio)) # 2782






colnames(dados_sisagua_p7_agrupados) # Nomes das colunas da tabelaA
colnames(df_pivot) # Nomes das colunas da tabelaB
str(dados_sisagua_p7_agrupados$codigo_ibge)
str(df_pivot$codigo_ibge)

# Converter codigo_ibge para character em ambas as tabelas
dados_sisagua_p7_agrupados <- dados_sisagua_p7_agrupados %>%
  mutate(codigo_ibge = as.character(codigo_ibge))

df_pivot <- df_pivot %>%
  mutate(codigo_ibge = as.character(codigo_ibge))


dados_sisagua_p7_agrupados <- dados_sisagua_p7_agrupados %>%
  mutate(codigo_ibge = str_pad(codigo_ibge, width = 7, pad = "0"))

df_pivot <- df_pivot %>%
  mutate(codigo_ibge = str_pad(codigo_ibge, width = 7, pad = "0"))




dados_sisagua_p7_agrupados <- dados_sisagua_p7_agrupados %>%
  mutate(codigo_ibge = trimws(codigo_ibge))

df_pivot <- df_pivot %>%
  mutate(codigo_ibge = trimws(codigo_ibge))




codigo_teste <- "0110012"


# Verificar se ele existe na tabela A
codigo_teste %in% dados_sisagua_p7_agrupados$codigo_ibge

# Verificar se ele existe na tabela B
codigo_teste %in% df_pivot$codigo_ibge



head(dados_sisagua_p7_agrupados$codigo_ibge, 20)
head(df_pivot$codigo_ibge, 20)

df_pivot_filtrado <- df_pivot %>%
  filter(codigo_ibge %in% dados_sisagua_p7_agrupados$codigo_ibge)

nrow(df_pivot_filtrado)

nchar(dados_sisagua_p7_agrupados$codigo_ibge[1])
nchar(df_pivot$codigo_ibge[1])




Encoding(dados_sisagua_p7_agrupados$codigo_ibge)
Encoding(df_pivot$codigo_ibge)

diferenca_A <- setdiff(dados_sisagua_p7_agrupados$codigo_ibge, df_pivot$codigo_ibge)

# Encontrar códigos IBGE da tabela B que não estão na tabela A
diferenca_B <- setdiff(df_pivot$codigo_ibge, dados_sisagua_p7_agrupados$codigo_ibge)

# Mostrar os primeiros valores diferentes
head(diferenca_A, 10)
head(diferenca_B, 10)



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
