
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


####ARSÊNIO REFERENCIA PROPORÇÃO PROP2----


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
  mutate(motivo_situacao_cadastral_desc=case_when(
    motivo_situacao_cadastral == 0 ~ "AUSENCIA DE MOTIVO",
    motivo_situacao_cadastral == 1 ~ "EXTINCAO POR ENCERRAMENTO LIQUIDACAO VOLUNTARIA",
    motivo_situacao_cadastral == 2 ~ "INCORPORACAO",
    motivo_situacao_cadastral == 9 ~ "NAO INICIO DE ATIVIDADES",
    motivo_situacao_cadastral == 18 ~ "INTERRUPCAO TEMPORARIA DAS ATIVIDADES",
    motivo_situacao_cadastral == 54 ~ "BAIXA TRATAMENTO DIFERENCIADO DADO AS ME E EPP LEI COMPLEMENTAR 123_2006",
    motivo_situacao_cadastral == 63 ~ "OMISSAO DE DECLARACOES",
    motivo_situacao_cadastral == 71 ~ "INAPTIDAO LEI 11.941_2009 Art.54",
    motivo_situacao_cadastral == 73 ~ "OMISSAO CONTUMAZ"
  )) |> 
  relocate(motivo_situacao_cadastral_desc, .after = motivo_situacao_cadastral)

empresa_detalhe <-empresa_detalhe |> 
  select(codigo_ibge,municipio_nome,uf,cnpj_completo,cnae_fiscal_principal,cnaes_secundarios,identificador_matriz_filial,nome_fantasia,situacao_cadastral, 
         situacao_cadastral_desc, data_situacao_cadastral,motivo_situacao_cadastral, motivo_situacao_cadastral_desc,data_inicio_atividade,tipo_logradouro,logradouro,
         numero, complemento,bairro,cep,ddd1,telefone1,e_mail)



write_xlsx(empresa_detalhe, "empresas_filtradas_com_cnpj_arsenio_prop2.xlsx")

####ARSÊNIO REFERENCIA PROPORÇÃO PROP1----

codigos_ibge <- c(3170800,3111101,5212501,5201405,5201108)
cnaes <- c(724301,729404,1510600,2019399,2312500,2449199,2610800)


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
  mutate(cnpj_completo = str_c(cnpj_basico, cnpj_ordem, cnpj_dv)) |> 
  relocate(cnpj_completo, .after = cnpj_dv)

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
  mutate(motivo_situacao_cadastral_desc=case_when(
    motivo_situacao_cadastral == 0 ~ "AUSENCIA DE MOTIVO",
    motivo_situacao_cadastral == 1 ~ "EXTINCAO POR ENCERRAMENTO LIQUIDACAO VOLUNTARIA",
    motivo_situacao_cadastral == 9 ~ "NAO INICIO DE ATIVIDADES",
    motivo_situacao_cadastral == 18 ~ "INTERRUPCAO TEMPORARIA DAS ATIVIDADES",
    motivo_situacao_cadastral == 54 ~ "BAIXA TRATAMENTO DIFERENCIADO DADO AS ME E EPP LEI COMPLEMENTAR 123_2006",
    motivo_situacao_cadastral == 63 ~ "OMISSAO DE DECLARACOES",
    motivo_situacao_cadastral == 71 ~ "INAPTIDAO LEI 11.941_2009 Art.54",
    motivo_situacao_cadastral == 73 ~ "OMISSAO CONTUMAZ"
   )) |> 
  relocate(motivo_situacao_cadastral_desc, .after = motivo_situacao_cadastral)


empresa_detalhe <-empresa_detalhe |> 
  select(codigo_ibge,municipio_nome,uf,cnpj_completo,cnae_fiscal_principal,cnaes_secundarios,identificador_matriz_filial,nome_fantasia,situacao_cadastral, 
         situacao_cadastral_desc, data_situacao_cadastral,motivo_situacao_cadastral, motivo_situacao_cadastral_desc,data_inicio_atividade,tipo_logradouro,logradouro,
         numero, complemento,bairro,cep,ddd1,telefone1,e_mail)




write_xlsx(empresa_detalhe, "empresas_filtradas_com_cnpj_arsenio_pro1.xlsx")




####ARSÊNIO REFERENCIA CNAES SIGNIFICATIVOS----
# 5300108 código de brasilia, tive que trocar para puxar são sebastião

codigos_ibge <- c(3170800,5212501,3111101,5201405,4126272,4308003,4318499,5201108,
                  3537800, 3526209, 4317905, 3539103,5218904,4303707,5211909, 3501152,
                  5209705, 4317509, 5300108, 4201505,3506607,4315602,1702109,4305207)

cnaes <- c(2019399,2610800,729403,2449199,724302,1510600)


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
  mutate(cnpj_completo = str_c(cnpj_basico, cnpj_ordem, cnpj_dv)) |> 
  relocate(cnpj_completo, .after = cnpj_dv)

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
  mutate(motivo_situacao_cadastral_desc=case_when(
    motivo_situacao_cadastral == 0 ~ "AUSENCIA DE MOTIVO",
    motivo_situacao_cadastral == 1 ~ "EXTINCAO POR ENCERRAMENTO LIQUIDACAO VOLUNTARIA",
    motivo_situacao_cadastral == 9 ~ "NAO INICIO DE ATIVIDADES",
    motivo_situacao_cadastral == 15 ~ "INEXISTENTE DE FATO",
    motivo_situacao_cadastral == 18 ~ "INTERRUPCAO TEMPORARIA DAS ATIVIDADES",
    motivo_situacao_cadastral == 36 ~ "PRATICA IRREGULAR DE OPRECACAO DE COMERCIO EXTERIOR",
    motivo_situacao_cadastral == 54 ~ "BAIXA TRATAMENTO DIFERENCIADO DADO AS ME E EPP LEI COMPLEMENTAR 123_2006",
    motivo_situacao_cadastral == 63 ~ "OMISSAO DE DECLARACOES",
    motivo_situacao_cadastral == 71 ~ "INAPTIDAO LEI 11.941_2009 Art.54",
    motivo_situacao_cadastral == 73 ~ "OMISSAO CONTUMAZ"
  )) |> 
  relocate(motivo_situacao_cadastral_desc, .after = motivo_situacao_cadastral)


empresa_detalhe <-empresa_detalhe |> 
  select(codigo_ibge,municipio_nome,uf,cnpj_completo,cnae_fiscal_principal,cnaes_secundarios,identificador_matriz_filial,nome_fantasia,situacao_cadastral, 
         situacao_cadastral_desc, data_situacao_cadastral,motivo_situacao_cadastral, motivo_situacao_cadastral_desc,data_inicio_atividade,tipo_logradouro,logradouro,
         numero, complemento,bairro,cep,ddd1,telefone1,e_mail)




write_xlsx(empresa_detalhe, "empresas_filtradas_com_cnpj_arsenio_cnaes_significativos.xlsx")





#### SELECIONANDO TODAS AS EMPRESAS DENTRO DOS MUNICIPIOS ----



codigos_ibge <- c(3170800,5212501,3111101,5201405,4126272,4308003,4318499,5201108,
                  3537800, 3526209, 4317905, 3539103,5218904,4303707,5211909, 3501152,
                  5209705, 4317509, 5300108, 4201505,3506607,4315602,1702109,4305207)

cnaes <- c(724302,729403,729404,1510600,1610205,2019399,2312500,2449199,2610800)


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
  mutate(motivo_situacao_cadastral_desc=case_when(
    motivo_situacao_cadastral == 0 ~ "AUSENCIA DE MOTIVO",
    motivo_situacao_cadastral == 1 ~ "EXTINCAO POR ENCERRAMENTO LIQUIDACAO VOLUNTARIA",
    motivo_situacao_cadastral == 2 ~ "INCORPORACAO",
    motivo_situacao_cadastral == 9 ~ "NAO INICIO DE ATIVIDADES",
    motivo_situacao_cadastral == 15 ~ "INEXISTENTE DE FATO",
    motivo_situacao_cadastral == 18 ~ "INTERRUPCAO TEMPORARIA DAS ATIVIDADES",
    motivo_situacao_cadastral == 36 ~ "PRATICA IRREGULAR DE OPRECACAO DE COMERCIO EXTERIOR",
    motivo_situacao_cadastral == 54 ~ "BAIXA TRATAMENTO DIFERENCIADO DADO AS ME E EPP LEI COMPLEMENTAR 123_2006",
    motivo_situacao_cadastral == 63 ~ "OMISSAO DE DECLARACOES",
    motivo_situacao_cadastral == 71 ~ "INAPTIDAO LEI 11.941_2009 Art.54",
    motivo_situacao_cadastral == 73 ~ "OMISSAO CONTUMAZ"
  )) |> 
  relocate(motivo_situacao_cadastral_desc, .after = motivo_situacao_cadastral)


empresa_detalhe <-empresa_detalhe |> 
  select(codigo_ibge,municipio_nome,uf,cnpj_completo,cnae_fiscal_principal,cnaes_secundarios,identificador_matriz_filial,nome_fantasia,situacao_cadastral, 
         situacao_cadastral_desc, data_situacao_cadastral,motivo_situacao_cadastral, motivo_situacao_cadastral_desc,data_inicio_atividade,tipo_logradouro,logradouro,
         numero, complemento,bairro,cep,ddd1,telefone1,e_mail)

write_xlsx(empresa_detalhe, "empresas_filtradas_com_cnpj_arsenio_todos_cnaes.xlsx")


