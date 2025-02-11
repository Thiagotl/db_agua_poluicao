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

# Carregar o arquivo CSV
file_path <- "Poluentes_Ref._Planilha_6 e 1_Samara_04-12-2024.csv"
df <- read_csv(file_path)

# Garantir que a coluna CNAE é um vetor de strings com 7 dígitos
df <- df  |>  mutate(CNAE = sprintf("%07d", as.integer(CNAE)))

# Obter os CNAEs únicos do arquivo
cnaes_do_arquivo <- unique(df$CNAE)

# Exibir os primeiros CNAEs para conferência
View(cnaes_do_arquivo)

##########################################
# Query SQL para contar estabelecimentos por município e CNAE primário
query <- glue::glue_sql("
  SELECT 
      m.codigo_ibge,
      m.nome AS municipio,
      e.cnae_fiscal_principal AS cnae,
      COUNT(*) AS num_estabelecimentos
  FROM estabelecimentos e
  JOIN municipios m ON e.municipio_id = m.codigo
  WHERE e.cnae_fiscal_principal IN ({cnaes_do_arquivo*})
  GROUP BY m.codigo_ibge, m.nome, e.cnae_fiscal_principal
", .con = con)

# Executar a query e carregar os dados
df_db <- dbGetQuery(con, query)

# Transformar a tabela para formato pivotado
df_cnaes_primarios <- df_db  |> 
  pivot_wider(names_from = cnae, values_from = num_estabelecimentos, values_fill = 0)

# Adicionar a coluna codigo_ibge como primeira coluna
# AQUI NAO PRECISA RODAR 
# 
# df_cnaes_primarios <- df_db  |> 
#   select(codigo_ibge, municipio, everything())
# 
# View(df_cnaes_primarios)

# Salvar a tabela em CSV se necessário
#write_csv(df_pivot, "cnaes_por_municipio.csv")


###########################################
# CONSULTA CNAEs secundario


# Query SQL para contar estabelecimentos por município e CNAE secundário, incluindo código IBGE
query_sec <- glue::glue_sql("
  WITH cnaes_exploded AS (
      SELECT 
          e.id AS estabelecimento_id,
          m.codigo_ibge,
          m.nome AS municipio,
          unnest(e.cnaes_secundarios) AS cnae
      FROM estabelecimentos e
      JOIN municipios m ON e.municipio_id = m.codigo
  )
  SELECT 
      codigo_ibge,
      municipio,
      cnae,
      COUNT(*) AS num_estabelecimentos
  FROM cnaes_exploded
  WHERE cnae IN ({cnaes_do_arquivo*})
  GROUP BY codigo_ibge, municipio, cnae
  ORDER BY municipio, num_estabelecimentos DESC
", .con = con)

# Executar a query e carregar os dados
df_db_sec <- dbGetQuery(con, query_sec)

# Transformar a tabela para formato pivotado
df_pivot_sec <- df_db_sec %>%
  pivot_wider(names_from = cnae, values_from = num_estabelecimentos, values_fill = 0)

View(df_pivot_sec)
# Adicionar a coluna codigo_ibge como primeira coluna
df_pivot <- df_pivot %>%
  select(codigo_ibge, municipio, everything())

# Visualizar os primeiros registros
View(df_pivot)

# Salvar a tabela em CSV se necessário
write_csv(df_pivot, "cnaes_secundarios_por_municipio.csv")

# Fechar a conexão
dbDisconnect(con)
##########################################


# Query SQL para contar estabelecimentos por município considerando CNAEs Primários e Secundários
query <- glue::glue_sql("
  WITH cnaes_exploded AS (
      -- CNAEs Primários
      SELECT 
          e.id AS estabelecimento_id,
          m.codigo_ibge,
          m.nome AS municipio,
          e.cnae_fiscal_principal AS cnae
      FROM estabelecimentos e
      JOIN municipios m ON e.municipio_id = m.codigo
      WHERE e.cnae_fiscal_principal IN ({cnaes_do_arquivo*})
      
      UNION ALL
      
      -- CNAEs Secundários (Explodindo o ARRAY)
      SELECT 
          e.id AS estabelecimento_id,
          m.codigo_ibge,
          m.nome AS municipio,
          unnest(e.cnaes_secundarios) AS cnae
      FROM estabelecimentos e
      JOIN municipios m ON e.municipio_id = m.codigo
      WHERE unnest(e.cnaes_secundarios) IN ({cnaes_do_arquivo*})
  )
  SELECT 
      codigo_ibge,
      municipio,
      cnae,
      COUNT(*) AS num_estabelecimentos
  FROM cnaes_exploded
  GROUP BY codigo_ibge, municipio, cnae
  ORDER BY municipio, num_estabelecimentos DESC
", .con = con)

# Executar a query e carregar os dados
df_db <- dbGetQuery(con, query)

# Transformar a tabela para formato pivotado
df_pivot <- df_db %>%
  pivot_wider(names_from = cnae, values_from = num_estabelecimentos, values_fill = 0)

# Adicionar a coluna codigo_ibge como primeira coluna
df_pivot <- df_pivot %>%
  select(codigo_ibge, municipio, everything())

# Visualizar os primeiros registros
View(df_pivot)

# Salvar a tabela em CSV se necessário
write_csv(df_pivot, "cnaes_primario_secundario_por_municipio.csv")

# Fechar a conexão
dbDisconnect(con)


