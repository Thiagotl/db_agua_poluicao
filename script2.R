library(readr)

dados_sisagua_p7 <- read_csv("planilha7_pivotresult_sisagua_25_out.csv") # planilha 8s
View(dados_sisagua_p7)

dim(table(dados_sisagua_p7$município)) # sao 2782
dim(table(result_principais$municipio_nome)) # 5246

colnames(dados_sisagua_p7)[colnames(dados_sisagua_p7)=='município']<-'municipio'
colnames(result_principais)[colnames(result_principais)=='municipio_nome']<-'municipio'

# GRUPAR O DATASET DOS DADOS DO SISAGUA POR MUNICIPIO E PARAMETRO
# dados_sisagua_p7_agrupados<-dados_sisagua_p7 |> 
#   group_by(municipio, uf, parâmetro) |> 
#   summarise(contagem_parametro = n(), .groups = "drop")
# 
# view(dados_sisagua_p7_agrupados)

dados_sisagua_p7_agrupados <- dados_sisagua_p7 |>
  select(municipio, parâmetro, uf, código_ibge, `Total de Consistentes detectados Acima do VMP`, `Total de Consistentes detectados Abaixo do VMP`) |>
  group_by(municipio, parâmetro) |>
  reframe(across(everything(), first), contagem_parametro = n()) # reframe - mantem todas as colunas e adiciona a nova
                                                                 # across(everything(), first) mantém as colunas selecionadas         
view(dados_sisagua_p7_agrupados)

# TRANSFORMAR PARA FORMATO WIDER (FORMATO LONGO)

result_principais_wide <- result_principais |> pivot_wider(
  names_from = cnae_codigo_primario,
  values_from = total_empresas_primario,
  #values_fill = list(total_empresas_primario = 0)
)

View(result_principais_wide)

# COMBINANDO AS DUAS TABELAS - SISAGUA E CNAE PRIMARIO
# AQUI TEMOS A PLANILHA 10 !!!!!

dados_combinados <- dados_sisagua_p7_agrupados |> 
  left_join(result_principais_wide, by='municipio')

attach(dados_combinados)

View(dados_combinados)

dim(table(dados_combinados$municipio)) # 2782



### TESTANDO ALGUNS FILTROS 

# Paramentros - Acrilamida, Antimônio, Arsênio, Bário, Cádmio, Chumbo, Cromo, Cobre, Níquel, Nitrato (como N), Selênio



tabela_nitrato <- dados_combinados |> 
   filter(parâmetro == "Nitrato (como N)") 
  # select()

View(tabela_nitrato)

### 





