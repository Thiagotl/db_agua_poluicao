library(tidyverse)
library(readxl)
library(lubridate)
library(stringr)
library(writexl)





municipios_teste_arsenio <- read_excel("municipios_teste_arsenio.xlsx")
attach(municipios_teste_arsenio)
View(municipios_teste_arsenio)

# AJUSTE DOS DADOS DOS MUNICIPIOS 

municipios_teste_arsenio<-municipios_teste_arsenio |> 
  select(município,uf,código_ibge,ponto_de_monitoramento,
         data_da_análise, parâmetro, consistencia)

municipios_teste_arsenio<-municipios_teste_arsenio |> 
  mutate(
    data_da_análise=dmy(data_da_análise),
    ano = year(data_da_análise),
    semestre = ifelse(month(data_da_análise)<=6, 1, 2)
  )


empresas_filtradas_todos_cnaes <- read_excel("empresas_filtradas_com_cnpj_arsenio_todos_cnaes.xlsx")
attach(empresas_filtradas_todos_cnaes)

empresas_filtradas_todos_cnaes<-empresas_filtradas_todos_cnaes |> 
  mutate(código_ibge = str_sub(codigo_ibge, end = -2))

empresas_filtradas_todos_cnaes<-empresas_filtradas_todos_cnaes |> 
  mutate(data_situacao_cadastral=ymd(data_situacao_cadastral),
         ano_sit_cadastral = year(data_situacao_cadastral))

empresas_filtradas_todos_cnaes<-empresas_filtradas_todos_cnaes |> 
  mutate(data_inicio_atividade=ymd(data_inicio_atividade),
         ano_ini_atividade = year(data_inicio_atividade)) |> 
  rename(ano = ano_sit_cadastral)

empresas_filtradas_todos_cnaes <- empresas_filtradas_todos_cnaes |>
  mutate(código_ibge = as.double(código_ibge))



normalizar_nome <- function(nome) {
  nome_sem_acentos <- stringi::stri_trans_general(nome, "Latin-ASCII")
  nome_sem_espacos <- gsub(" ", "_", nome_sem_acentos)
  nome_limpo <- gsub("[^A-Za-z0-9_]", "", nome_sem_espacos)
  return(nome_limpo)
}

lista_municipios <- empresas_filtradas_todos_cnaes |>
  group_by(código_ibge) |>
  group_split() |>
  map(function(df_municipio) {
    codigo <- unique(df_municipio$código_ibge)
    nome <- unique(df_municipio$municipio_nome)
    nome_limpo <- normalizar_nome(nome)
    aba <- paste0("municipio_", codigo, "_", nome_limpo)
    
    df_formatado <- df_municipio |>
      select(
        cnpj_completo,
        código_ibge,
        municipio_nome,
        uf,
        situacao_cadastral_desc,
        ano
        
      )  |> 
      pivot_wider(
        names_from = cnpj_completo,
        values_from = situacao_cadastral_desc,
        names_prefix = "cnpj_",
        values_fill = NA
      )
    
    setNames(list(df_formatado), aba)
  }) |>
  flatten()

# Exportar para Excel com múltiplas abas
write_xlsx(lista_municipios, "municipios_cnpj_arsenio.xlsx")



#### juntar tudo -----

empresas_filtradas_todos_cnaes <- empresas_filtradas_todos_cnaes |>
  mutate(código_ibge = as.double(código_ibge))

municipios_teste_arsenio <- municipios_teste_arsenio |>
  mutate(código_ibge = as.double(código_ibge))



lista_municipios <- empresas_filtradas_todos_cnaes |>
  group_by(código_ibge) |>
  group_split() |>
  map(function(df_empresas) {
    codigo <- unique(df_empresas$código_ibge)
    nome <- unique(df_empresas$municipio_nome)
    nome_limpo <- normalizar_nome(nome)
    aba <- paste0("municipio_", codigo, "_", nome_limpo)
    
    # Filtrar os dados de arsênio desse município
    df_arsenio <- municipios_teste_arsenio |>
      filter(código_ibge == codigo)
    
    # Pivotar os dados das empresas
    df_empresas_largo <- df_empresas |>
      select(
        cnpj_completo,
        código_ibge,
        municipio_nome,
        uf,
        situacao_cadastral_desc,
        ano
      ) |>
      pivot_wider(
        names_from = cnpj_completo,
        values_from = situacao_cadastral_desc,
        names_prefix = "cnpj_",
        values_fill = NA
      )
    
    # Unir com os dados de arsênio por código_ibge e ano
    df_final <- left_join(df_arsenio, df_empresas_largo, by = c("código_ibge", "ano"))
    
    # Nomear a aba
    setNames(list(df_final), aba)
  }) |>
  flatten()

# 3. Salvar no Excel
write_xlsx(lista_municipios, "municipios_join_cnpj_arsenio_teste.xlsx")



##### Mais um teste---

# Anos de interesse
anos_interesse <- 2014:2023

# Preenche situação para todos os anos
expandir_situacao <- function(df_emp) {
  df_expandido <- df_emp |>
    select(cnpj_completo, código_ibge, ano_ini_atividade, ano, situacao_cadastral_desc) |>
    filter(!is.na(ano)) |>
    complete(
      ano = anos_interesse,
      nesting(cnpj_completo, código_ibge, ano_ini_atividade),
      fill = list(situacao_cadastral_desc = NA)
    ) |>
    arrange(cnpj_completo, ano) |>
    group_by(cnpj_completo) |>
    mutate(
      situacao_cadastral_desc = ifelse(ano < ano_ini_atividade, NA, situacao_cadastral_desc),
      situacao_cadastral_desc = zoo::na.locf(situacao_cadastral_desc, na.rm = FALSE),
    ) |>
    ungroup()
  
  return(df_expandido)
}

# Aplica para cada município
lista_municipios <- empresas_filtradas_todos_cnaes |>
  group_by(código_ibge) |>
  group_split() |>
  map(function(df_empresas) {
    codigo <- unique(df_empresas$código_ibge)
    nome <- unique(df_empresas$municipio_nome)
    nome_limpo <- normalizar_nome(nome)
    aba <- paste0("municipio_", codigo, "_", nome_limpo)
    
    df_arsenio <- municipios_teste_arsenio |>
      filter(código_ibge == codigo)
    
    # Expandir situação para todos os anos
    df_expandido <- expandir_situacao(df_empresas)
    
    # Pivotar
    df_empresas_largo <- df_expandido |>
      pivot_wider(
        id_cols = c(ano, código_ibge),
        names_from = cnpj_completo,
        values_from = situacao_cadastral_desc,
        names_prefix = "cnpj_",
        values_fill = NA
      )
    
    # Juntar com dados de arsênio
    df_final <- left_join(df_arsenio, df_empresas_largo, by = c("código_ibge", "ano"))
    
    setNames(list(df_final), aba)
  }) |>
  flatten()


# Salvar no Excel
write_xlsx(lista_municipios, "municipios_join_cnpj_arsenio_completos_teste2.xlsx")


###############
# TENTATIVAS 



# Criar um data frame com os anos de interesse
anos_df <- data.frame(ano = 2014:2023)

# Função para expandir situação para todos os anos de interesse
expandir_situacao <- function(df_emp) {
  anos_interesse <- anos_df$ano
  
  df_expandido <- df_emp |>
    select(cnpj_completo, código_ibge, ano_ini_atividade, ano, situacao_cadastral_desc) |>
    filter(!is.na(ano)) |>
    complete(
      ano = anos_interesse,
      nesting(cnpj_completo, código_ibge, ano_ini_atividade),
      fill = list(situacao_cadastral_desc = NA)
    ) |>
    arrange(cnpj_completo, ano) |>
    group_by(cnpj_completo) |>
    mutate(
      # 1. Preenche para frente
      situacao_cadastral_desc = zoo::na.locf(situacao_cadastral_desc, na.rm = FALSE),
      # 2. Remove qualquer status antes do início da empresa
      situacao_cadastral_desc = ifelse(ano < ano_ini_atividade, NA, situacao_cadastral_desc)
    ) |>
    # 3. Preenche NAs restantes (anteriores ao primeiro status conhecido) como "ATIVA"
    group_by(cnpj_completo) |>
    mutate(
      primeira_situacao_ano = min(ano[!is.na(situacao_cadastral_desc)], na.rm = TRUE),
      situacao_cadastral_desc = ifelse(
        is.na(situacao_cadastral_desc) & ano >= ano_ini_atividade & ano < primeira_situacao_ano,
        "ATIVA",
        situacao_cadastral_desc
      )
    ) |>
    select(-primeira_situacao_ano) |>
    ungroup()
  
  return(df_expandido)
}


# Aplica para cada município
lista_municipios <- empresas_filtradas_todos_cnaes |>
  group_by(código_ibge) |>
  group_split() |>
  map(function(df_empresas) {
    codigo <- unique(df_empresas$código_ibge)
    nome <- unique(df_empresas$municipio_nome)
    nome_limpo <- normalizar_nome(nome)
    aba <- paste0("municipio_", codigo, "_", nome_limpo)
    
    df_arsenio <- municipios_teste_arsenio |>
      filter(código_ibge == codigo)
    
    # Expandir situação para todos os anos
    df_expandido <- expandir_situacao(df_empresas)
    
    # Pivotar
    df_empresas_largo <- df_expandido |>
      pivot_wider(
        id_cols = c(ano, código_ibge),
        names_from = cnpj_completo,
        values_from = situacao_cadastral_desc,
        names_prefix = "cnpj_",
        values_fill = NA
      )
    
    # Juntar com dados de arsênio
    df_final <- left_join(df_arsenio, df_empresas_largo, by = c("código_ibge", "ano"))
    
    setNames(list(df_final), aba)
  }) |>
  flatten()

# Salvar no Excel
write_xlsx(lista_municipios, "municipios_join_cnpj_arsenio_completos_teste4.xlsx")






