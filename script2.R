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
