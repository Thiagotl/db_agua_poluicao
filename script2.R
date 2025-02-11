library(readr)
library(stringi)
library(heatmaply)

# link importante - https://www.gov.br/receitafederal/dados/municipios.csv
# link importante - https://www.ibge.gov.br/explica/codigos-dos-municipios.php
# link importante - https://cbm.nfiss.com.br/Lista_SB.php?Cadastro=116&Campo=ID7_IBGE_CTBT

dados_sisagua_p7 <- read_csv("planilha7_pivotresult_sisagua_25_out.csv") # planilha 8s
View(dados_sisagua_p7)

colnames(dados_sisagua_p7)[colnames(dados_sisagua_p7)=="código_ibge"] <-'codigo_ibge'

colnames(dados_sisagua_p7)[colnames(dados_sisagua_p7)=="município"] <-'municipio'

dim(table(dados_sisagua_p7$municipio)) # sao 2782


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


# REMOVER AS TRÊS CIDADES SEM CÓDIGO IBGE. 
municipios_para_remover <- c("GRANJEIRO", "SANTA FILOMENA", "SOLIDAO")

dados_sisagua_p7_agrupados <- dados_sisagua_p7_agrupados %>%
  filter(!municipio %in% municipios_para_remover)

dim(table(dados_sisagua_p7_agrupados$municipio)) # 2779


df_cnaes_primarios <- df_cnaes_primarios |> 
  select(-c(municipio))

####### PLANILHA 10 AQUIIIII #######

dados_combinados <- dados_sisagua_p7_agrupados |> 
  left_join(df_cnaes_primarios, by='codigo_ibge') 

attach(dados_combinados)

View(dados_combinados)

dim(table(dados_combinados$municipio)) # 2779


write_csv(dados_combinados, "dados_combinado.csv")


# uma alteracao para os dados combinados

colnames(dados_combinados) <- as.character(colnames(dados_combinados))

####### TESTANDO ALGUNS FILTROS #####

# Paramentros - Acrilamida, Antimônio, Arsênio, Bário, Cádmio, Chumbo, Cromo, 
#               Cobre, Níquel, Nitrato (como N), Selênio


filtros_cnaes<-read_csv("Poluentes_Ref._Planilha_6 e 1_Samara_04-12-2024.csv")

table(filtros_cnaes$Parâmetro)
# filtor cnaes - Antimônio

cnaes_Antimônio<-filtros_cnaes |> 
  filter(Parâmetro == "Antimônio") |> 
  select(CNAE)


# filtor cnaes - Arsênio

cnaes_arsenio<-filtros_cnaes |> 
  filter(Parâmetro == "Arsênio") |> 
  select(CNAE)


# filtor cnaes - Bário

cnaes_bario<-filtros_cnaes |> 
  filter(Parâmetro == "Bário") |> 
  select(CNAE)

# filtor cnaes - Cádmio

cnaes_cadmio<-filtros_cnaes |> 
  filter(Parâmetro == "Cádmio") |> 
  select(CNAE)

# filtor cnaes - Chumbo

cnaes_chumbo<-filtros_cnaes |> 
  filter(Parâmetro == "Chumbo") |> 
  select(CNAE)

# filtor cnaes - Cromo

cnaes_cromo<-filtros_cnaes |> 
  filter(Parâmetro == "Cromo") |> 
  select(CNAE)


# filtor cnaes - Cobre

cnaes_cobre<-filtros_cnaes |> 
  filter(Parâmetro == "Cobre") |> 
  select(CNAE)

# filtor cnaes - Níquel

cnaes_niquel<-filtros_cnaes |> 
  filter(Parâmetro == "Níquel") |> 
  select(CNAE)

# filtro cnaes - Selênio 
cnaes_selenio<-filtros_cnaes |> 
  filter(Parâmetro == "Selênio") |> 
  select(CNAE)

#### FUNÇÃO PARA EXTRAIR CNAES ----

extrair_cnaes <- function(df, parametro){
  df |> 
    filter(Parâmetro == parametro) |> 
    mutate(CNAE = as.character(CNAE)) |> 
    pull(CNAE) 
}
cnaes_nitrato <- sub("^0+", "", cnaes_nitrato) # colocar na funcao
# testas depois


###### FILTRO PARA NITRATO (COMO N) ----

cnaes_nitrato<-extrair_cnaes(filtros_cnaes, "Nitrato (como N)")

cnaes_nitrato <- sub("^0+", "", cnaes_nitrato)

tabela_nitrato <- dados_combinados |> 
  filter(parâmetro == "Nitrato (como N)") |> 
  select(municipio,parâmetro,uf,
         `Total de testes substâncias em geral para cada linha - incluindo MENOR_LQ`,
         `Total de inconsistentes`, `Total de Consistentes não detectados`, 
         `Total de parâmetros com MENOR_LQ`,
         `Total de Consistentes detectados Abaixo do VMP`,
         `Total de Consistentes detectados Acima do VMP`,Total_Detectados ,
         all_of(cnaes_nitrato)
         ) |> 
  arrange(municipio)

View(tabela_nitrato)


tabela_nitrato <-as.data.frame(tabela_nitrato)

write_csv(tabela_nitrato, "planilha_nitrato_como_N.csv")



# PARA CORRELAÇÃO Nitrato como N

tabela_nitrato<-tabela_nitrato |> 
  mutate(across(11:100, as.double))



zero_sd_cols <- sapply(tabela_nitrato[, -c(1:3)], function(x) sd(x, na.rm = TRUE) == 0)
zero_sd_cols  # Retorna TRUE para colunas com desvio padrão zero


t1<-round(cor(tabela_nitrato[,-c(1:3,16)]), 5)
View(t1)

colnames(t1)[colnames(t1)=="Total de testes substâncias em geral para cada linha - incluindo MENOR_LQ"] <-'Total de Testes'

colnames(t1)[colnames(t1)=="Total de inconsistentes"] <-'Tot inconst'
colnames(t1)[colnames(t1)=="Total de Consistentes não detectados"] <-'Tot cons n dec'
colnames(t1)[colnames(t1)=="Total de parâmetros com MENOR_LQ"] <-'Tot par menor LQ'
colnames(t1)[colnames(t1)=="Total de Consistentes detectados Abaixo do VMP"] <-'Tot cons dec AB VMP'
colnames(t1)[colnames(t1)=="Total de Consistentes detectados Acima do VMP"] <-'Tot cons dec AC VMP'


heatmaply(t1)



###### FILTRO PARA ACRILAMIDA ----

cnaes_acrilamida <- extrair_cnaes(filtros_cnaes, "Acrilamida")

cnaes_acrilamida <- sub("^0+","", cnaes_acrilamida)

View(cnaes_acrilamida)

tabela_acrilamida <- dados_combinados |> 
  filter(parâmetro == "Acrilamida") |> 
  select(municipio,parâmetro,uf,
         `Total de testes substâncias em geral para cada linha - incluindo MENOR_LQ`,
         `Total de inconsistentes`, `Total de Consistentes não detectados`, 
         `Total de parâmetros com MENOR_LQ`,
         `Total de Consistentes detectados Abaixo do VMP`,
         `Total de Consistentes detectados Acima do VMP`,Total_Detectados ,
         all_of(cnaes_acrilamida)) |> arrange(municipio)

View(tabela_acrilamida)


tabela_acrilamida<-tabela_acrilamida |> 
  mutate(across(11:19, as.double))

tabela_acrilamida <-as.data.frame(tabela_acrilamida)


zero_sd_cols <- sapply(tabela_acrilamida[, -c(1:3)], function(x) sd(x, na.rm = TRUE) == 0)
zero_sd_cols  # Retorna TRUE para colunas com desvio padrão zero

t2<-cor(tabela_acrilamida[,-c(1:3)])
View(t2)

heatmaply(t2)
write_csv(tabela_acrilamida, "planilha_acrilamida.csv")


hist(tabela_acrilamida$Total_Detectados)
plot(tabela_acrilamida$`2029100`,tabela_acrilamida$Total_Detectados)

cor.test(tabela_acrilamida$`2029100`,tabela_acrilamida$Total_Detectados, method = "kendall")


table(tabela_acrilamida$`2029100`,tabela_acrilamida$Total_Detectados)

prop.table(table(tabela_acrilamida$x,tabela_acrilamida$y ))


tabela_acrilamida<-tabela_acrilamida |> 
  mutate(y= ifelse(`2029100` > 0,1,0),
         x= ifelse(Total_Detectados >0,1,0))

chisq.test(table(tabela_acrilamida$x,tabela_acrilamida$y ))
prop.table(table(tabela_acrilamida$y,tabela_acrilamida$x ),2)

table(tabela_acrilamida$y,tabela_acrilamida$x )
#### TESTE PARA MATRIZ Dx#### TESTE PARA MATRIZ DE CORRELAÇÃO




#https://cran.r-project.org/web/packages/heatmaply/vignettes/heatmaply.html






























