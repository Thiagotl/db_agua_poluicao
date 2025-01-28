library(DBI)
library(RPostgres)
library(dplyr)
library(dbplyr)
library(openxlsx)
library(readr)
library(tidyverse)
library(glue)


# LEMBRA DE SUBIR O BANCO !!!!!!!!!!!!

con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "mydatabase",
  host = "localhost",  # ou IP do servidor
  port = 5432:5432,         # Porta padrão do PostgreSQL
  user = "myuser",
  password = "mypassword"
)

tabela <-tbl(con, "cnaes")



#cnae_list <- load_csv_data(csv_path)
load_csv_data <- function(csv_path) {
  data <- read.csv(csv_path, stringsAsFactors = FALSE)
  cnae_column <- "CNAE"  # Ajuste o nome da coluna que contém os CNAEs no CSV
  return(data[[cnae_column]])
}

# Caminho do CSV
csv_path <- "Poluentes_Ref._Planilha_6 e 1_Samara_04-12-2024.csv"

# Carregar os CNAEs do CSV
cnae_list <- load_csv_data(csv_path)

# Verificar o tamanho da lista de CNAEs
length(cnae_list)

# Gerar a string de CNAEs formatada para a consulta SQL
cnae_values <- paste(cnae_list, collapse = ",")
print(cnae_values)


typeof(cnae_values)

# NOVA QUERY  
# Executa a query no banco de dados
# Construir a query apenas para CNAEs principais
query_cnaes_principais <- glue_sql("
  SELECT 
      m.nome AS municipio_nome,
      c.codigo AS cnae_codigo_primario,
      COUNT(e.id) AS total_empresas_primario
  FROM 
      municipios m
  LEFT JOIN 
      estabelecimentos e ON m.codigo = e.municipio_id
  LEFT JOIN 
      cnaes c ON e.cnae_fiscal_principal = c.codigo
  WHERE 
      c.codigo IN ({DBI::SQL(cnae_values)})
  GROUP BY 
      m.nome, c.codigo
  ORDER BY 
      m.nome, c.codigo;
", .con = con)

# Executar a query no banco de dados
result_principais <- dbGetQuery(con, query_cnaes_principais)

# Visualizar os resultados
print(result_principais)

View(result_principais)

dim(table(result_principais$cnae_codigo))

# CONSULTA CNAEs secundario

# Construir a query para CNAEs secundários
query_cnaes_secundarios <- glue_sql("
  SELECT 
      m.nome AS municipio_nome,
      c.codigo AS cnae_codigo_secundario,
      COUNT(DISTINCT e.id) AS total_empresas_secundario
  FROM 
      municipios m
  LEFT JOIN 
      estabelecimentos e ON m.codigo = e.municipio_id
  LEFT JOIN 
      estabelecimento_cnaes_secundarios ecs ON e.id = ecs.estabelecimento_id
  LEFT JOIN 
      cnaes c ON ecs.cnae_id = c.codigo
  WHERE 
      c.codigo IN ({DBI::SQL(cnae_values)})
  GROUP BY 
      m.nome, c.codigo
  ORDER BY 
      m.nome, c.codigo;
", .con = con)

# Executar a query no banco de dados
result_secundarios <- dbGetQuery(con, query_cnaes_secundarios)

# Visualizar os resultados
print(result_secundarios)

View(result_secundarios)



## data set com a junção dos cnaes primarios e secundarios


resultado_final_cnae_ps<-result_principais |> 
  left_join(result_secundarios, by = 'municipio_nome', relationship = 'manny-to-many') |> 
  group_by(municipio_nome, cnae_codigo_primario) |> 
  summarise(
    total_empresas_primario = sum(total_empresas_primario, na.rm = TRUE),
    total_empresas_somado = total_empresas_primario + sum(total_empresas_secundario, na.rm = TRUE),
    .groups = "drop"
  )




# Opcional: Salvar o resultado em um arquivo CSV para análise
write.csv(result_pivot_principais, "cnaes_principais_pivot.csv", row.names = FALSE)


# # Mostrar o caminho do arquivo gerado
# cat("Arquivo exportado para:", export_file_path, "\n")
# 

 # IMPORTANTE
# # Desconectar do banco de dados
# dbDisconnect(con)


