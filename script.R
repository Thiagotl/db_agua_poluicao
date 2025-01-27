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

# Construir e interpolar a consulta SQL
query_completa <- sqlInterpolate(
  con,
  "SELECT e.*, c1.codigo AS cnae_principal, c2.codigo AS cnae_secundario, m.nome AS municipio_nome
   FROM estabelecimentos e
   INNER JOIN cnaes c1 ON e.cnae_fiscal_principal = c1.codigo
   LEFT JOIN estabelecimento_cnaes_secundarios ecs ON e.id = ecs.estabelecimento_id
   LEFT JOIN cnaes c2 ON ecs.cnae_id = c2.codigo
   INNER JOIN municipios m ON e.municipio_id = m.codigo
   WHERE c1.codigo IN (?) OR c2.codigo IN (?)",
  DBI::SQL(cnae_values),
  DBI::SQL(cnae_values)  # Passando a mesma lista formatada para os dois placeholders
)

# NOVA QUERY  
# Executa a query no banco de dados
# Construir a query apenas para CNAEs principais
query_cnaes_principais <- glue_sql("
  SELECT 
      m.nome AS municipio_nome,
      c.codigo AS cnae_codigo,
      COUNT(e.id) AS total_empresas
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
      c.codigo AS cnae_codigo,
      COUNT(DISTINCT e.id) AS total_empresas
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






























# Garantir que todos os CNAEs estejam presentes
# Criar um tibble com todos os CNAEs e combinar com os resultados
all_cnaes <- tibble(cnae_codigo = cnae_list)

# Combinar os CNAEs com os resultados
result_full <- result_principais %>%
  full_join(all_cnaes, by = "cnae_codigo") %>%
  mutate(total_empresas = replace_na(total_empresas, 0))  # Preencher NA com 0

# Pivotar os resultados para garantir que todos os CNAEs se tornem colunas
result_pivot <- result_full %>%
  pivot_wider(
    names_from = cnae_codigo,  # Transformar os CNAEs em colunas
    values_from = total_empresas,  # Usar as contagens como valores
    values_fill = 0  # Preencher células vazias com 0
  )

# Visualizar os resultados pivotados
print(result_pivot)

# Visualizar o resultado pivotado
print(result_pivot_principais)

View(result_pivot_principais)
# Opcional: Salvar o resultado em um arquivo CSV para análise
write.csv(result_pivot_principais, "cnaes_principais_pivot.csv", row.names = FALSE)







# Executar a consulta
iestabelecimentos <- dbGetQuery(con, query_completa)

View(iestabelecimentos)
# Criar uma tabela pivotada com municípios como linhas, CNAEs como colunas e soma das quantidades como valores
quant_cnaes <- iestabelecimentos  |> 
  select(municipio_nome, cnae_principal, cnae_secundario)  |> 
  pivot_longer(
    cols = c(cnae_principal, cnae_secundario),
    names_to = "tipo",
    values_to = "cnae"
  )  |> 
  group_by(municipio_nome, cnae)  |> 
  summarise(quantidade = n(), .groups = "drop")  |> 
  pivot_wider(
    names_from = cnae,
    values_from = quantidade,
    values_fill = 0
  ) 

View(quant_cnaes)

# # Criar um arquivo Excel
export_file_path <- "municipios_cnaes_formatado.csv"
write.csv(quant_cnaes, export_file_path)

# 
# # Mostrar o caminho do arquivo gerado
# cat("Arquivo exportado para:", export_file_path, "\n")
# 

 # IMPORTANTE
# # Desconectar do banco de dados
# dbDisconnect(con)


