library(DBI)
library(RPostgres)
library(dplyr)
library(dbplyr)
library(openxlsx)
library(readr)
library(tidyverse)



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


# Função para carregar os dados do CSV
load_csv_data <- function(csv_path) {
  # Ler o arquivo CSV
  csv_data <- read_csv(csv_path)
  
  # Extrair os CNAEs e colocar numa lista
  cnaes_list <- csv_data$CNAE
  
  return(cnaes_list)
}

csv_path <- "Poluentes_Ref._Planilha_6 e 1_Samara_04-12-2024.csv"


cnae_list <- load_csv_data(csv_path)

# Gerar a string de CNAEs formatada para a consulta SQL
cnae_values <- paste0("'", cnae_list, "'", collapse = ",")

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

# Executar a consulta
iestabelecimentos <- dbGetQuery(con, query_completa)

# Criar uma tabela pivotada com municípios como linhas, CNAEs como colunas e soma das quantidades como valores
quant_cnaes <- iestabelecimentos  |> 
  select(municipio_nome, cnae_principal, cnae_secundario) %>%
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

