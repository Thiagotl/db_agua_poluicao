
# CONSULTA DAS EMPRESAS BASEADA NOS DADOS FILTRADOS PARA CADA PARÂMETRO

library(DBI)
library(RPostgres)
library(dplyr)
library(dbplyr)
library(openxlsx)
library(readr)
library(tidyverse)
library(glue)
library(writexl)



# CONEXÃO 


con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "mydatabase",
  host = "localhost",  # ou IP do servidor
  port = 5432:5432,         # Porta padrão do PostgreSQL
  user = "myuser",
  password = "mypassword"
)


# ARSÊNIO


codigos_ibge <- c(3170800,4303707,3539103,3537800)
cnaes <- c(724301,1510600,729404,2449199,2019399)

query <- glue::glue_sql("
  SELECT 
    e.*, 
    m.codigo_ibge, 
    m.nome AS municipio_nome, 
    uf.nome AS estado_nome 
  FROM estabelecimentos e
  JOIN municipios m ON e.municipio_id = m.codigo
  JOIN estados uf ON m.codigo_uf = uf.uf
  WHERE m.codigo_ibge IN ({codigos_ibge*})
    AND e.cnae_fiscal_principal IN ({cnaes*})
", .con = con)


empresa_detalhe <- dbGetQuery(con, query)

empresa_detalhe <- empresa_detalhe |> 
  mutate(cnpj_completo = str_c(cnpj_basico,cnpj_ordem,cnpj_dv)) |> 
  relocate(cnpj_completo, .after = cnpj_dv)

empresa_detalhe <- empresa_detalhe |> 
  relocate(municipio_nome, .after = cnpj_completo) |> 
  relocate(uf, .after = municipio_nome) |> 
  relocate(codigo_ibge, .after = cnpj_completo)

empresa_detalhe <- empresa_detalhe |> 
  mutate(situacao_cadastral_desc = case_when(
    situacao_cadastral == 1 ~ "NULA",
    situacao_cadastral == 2 ~ "ATIVA",
    situacao_cadastral == 3 ~ "SUSPENSA",
    situacao_cadastral == 4 ~ "INAPTA",
    situacao_cadastral == 8 ~ "BAIXADA"
  )) |> 
  relocate(situacao_cadastral_desc, .after = situacao_cadastral) |> 
  rename(e_mail = situacao_especial)


empresa_detalhe <- empresa_detalhe |> 
  select(-c(id,cnpj_basico,cnpj_ordem,cnpj_dv,nome_cidade_exterior, pais, estado_nome,
            data_situacao_especial, email))

write_xlsx(empresa_detalhe, "empresas_filtradas_com_cnpj_arsenio.xlsx")


