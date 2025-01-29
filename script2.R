library(readr)

dados_sisagua_p7 <- read_csv("planilha7_pivotresult_sisagua_25_out.csv") # planilha 8s
View(dados_sisagua_p7)

dim(table(dados_sisagua_p7$município)) # sao 2782
dim(table(result_principais$municipio_nome)) # 5246

colnames(dados_sisagua_p7)[colnames(dados_sisagua_p7)=='município']<-'municipio'
colnames(result_principais)[colnames(result_principais)=='municipio_nome']<-'municipio' # adicionar código ibge

# GRUPAR O DATASET DOS DADOS DO SISAGUA POR MUNICIPIO E PARAMETRO
# dados_sisagua_p7_agrupados<-dados_sisagua_p7 |> 
#   group_by(municipio, uf, parâmetro) |> 
#   summarise(contagem_parametro = n(), .groups = "drop")
# 
# view(dados_sisagua_p7_agrupados)

dados_sisagua_p7_agrupados <- dados_sisagua_p7 |>
  select(municipio, parâmetro, uf, código_ibge, `Total de testes substâncias em geral para cada linha - incluindo MENOR_LQ`,
         ) |>
  
  
  
  
  group_by(municipio, parâmetro, ) |>
  reframe(across(everything(), first), contagem_parametro = n()) # reframe - mantem todas as colunas e adiciona a nova
                                                                 # across(everything(), first) mantém as colunas selecionadas         
view(dados_sisagua_p7_agrupados)

# TRANSFORMAR PARA FORMATO WIDER (FORMATO LONGO)

result_principais_wide <- result_principais |> pivot_wider(
  names_from = cnae_codigo_primario,
  values_from = total_empresas_primario,
  values_fill = list(total_empresas_primario = 0)
)

View(result_principais_wide)

# COMBINANDO AS DUAS TABELAS - SISAGUA E CNAE PRIMARIO
# AQUI TEMOS A PLANILHA 10 !!!!!

dados_combinados <- dados_sisagua_p7_agrupados |> 
  left_join(result_principais_wide, by='municipio') # mudar aqui

attach(dados_combinados)

View(dados_combinados)

summary(dados_combinados)

dim(table(dados_combinados$municipio)) # 2782



### TESTANDO ALGUNS FILTROS 

# Paramentros - Acrilamida, Antimônio, Arsênio, Bário, Cádmio, Chumbo, Cromo, Cobre, Níquel, Nitrato (como N), Selênio

# cnaes_nitrato <- c(
#   "111301", "111302", "111303", "111399", "112101", "112102", "112199", "113000", 
#   "114800", "115600", "116401", "116402", "116403", "116499", "119901", "119902", 
#   "119903", "119904", "119905", "119906", "119907", "119908", "119909", "119999", 
#   "121101", "121102", "122900", "131800", "132600", "133401", "133402", "133403", 
#   "133404", "133405", "133406", "133407", "133408", "133409", "133410", "133411", 
#   "133499", "134200", "135100", "139301", "139302", "139303", "139304", "139305", 
#   "139306", "139399", "141501", "141502", "142300", "151201", "151202", "151203", 
#   "152101", "152102", "152103", "153901", "153902", "154700", "155501", "155502", 
#   "155503", "155504", "155505", "159899", "161001", "161003", "210101", "210102", 
#   "210103", "210104", "210105", "210106", "210108", "220902", "1013901", "1052000", 
#   "2012600", "2013401", "2013402", "2019399", "2092401", "2092402", "2110600", 
#   "2121101", "2122000", "3839401"
# )


tabela_nitrato <- dados_combinados |> 
  filter(parâmetro == "Nitrato (como N)") |> 
  select(municipio,parâmetro,`Total de Consistentes detectados Acima do VMP`,contagem_parametro, uf, all_of(cnaes_nitrato),
         x=dados_combinados$`Total de Consistentes detectados Acima do VMP`+dados_combinados$`Total de Consistentes detectados Abaixo do VMP`)

  

View(tabela_nitrato)

cor.test(tabela_nitrato$`Total de Consistentes detectados Acima do VMP`,tabela_nitrato$`151201`)

plot(tabela_nitrato$`Total de Consistentes detectados Acima do VMP`,tabela_nitrato$`151201`)

plot(tabela_nitrato$`151201`,tabela_nitrato$`Total de Consistentes detectados Acima do VMP`)


plot(tabela_nitrato$`151201`,tabela_nitrato$x)

?cor.test
t1<-cor(tabela_nitrato[,-c(1,2,4,5)])

View(t1)

apply(tabela_nitrato[,-c(1,2,4,5)],2,summary)
### 
(tabela_nitrato[-1,6])





