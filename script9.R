library(tidyverse)
library(readxl)
library(lubridate)
library(stringr)
library(writexl)
library(purrr)


# ── arsênio: manter variáveis de interesse, ajustar IBGE e criar resumo ──
arsenio_raw <- read_excel("municipios_teste_arsenio.xlsx",
                          .name_repair = "unique") |>
  select(município, uf, código_ibge, tipo_de_resultado, parâmetro,
         data_da_coleta, consistencia, atendimento_ao_padrao) |>
  #  AJUSTE DO CÓDIGO IBGE (SÃO SEBASTIÃO-DF → Brasília)
  mutate(
    código_ibge = if_else(
       uf == "DF",
      530010,             # código de Brasília
      código_ibge
    )
  ) |>
  filter(tipo_de_resultado == "QUANTIFICADO")

arsenio_resumo <- arsenio_raw |>
  mutate(
    data_da_coleta   = dmy(data_da_coleta),
    ano              = year(data_da_coleta),
    semestre         = if_else(month(data_da_coleta) <= 6, 1, 2),
    atendimento_ao_padrao = factor(
      atendimento_ao_padrao,
      levels = c("Acima do VMP", "Abaixo do VMP", "not applicable"))
  ) |>
  group_by(código_ibge, ano, semestre) |>
  mutate(
    n_testes               = n(),
    quant_testes_acima_VMP = sum(atendimento_ao_padrao == "Acima do VMP")
  ) |>
  ungroup() |>
  arrange(código_ibge, ano, semestre, atendimento_ao_padrao) |>
  distinct(código_ibge, ano, semestre, .keep_all = TRUE) |>
  relocate(c(n_testes, quant_testes_acima_VMP), .after = semestre) |>
  mutate(atendimento_ao_padrao = as.character(atendimento_ao_padrao))




empresas <- read_excel("empresas_filtradas_com_cnpj_arsenio_todos_cnaes.xlsx",
                       .name_repair = "unique") |>
  mutate(
    código_ibge           = as.double(str_sub(codigo_ibge, end = -2)),
    data_situacao_cadastral = ymd(data_situacao_cadastral),
    ano_sit_cadastral       = year(data_situacao_cadastral),
    data_inicio_atividade = ymd(data_inicio_atividade),
    ano_ini_atividade       = year(data_inicio_atividade)
  ) |>
  rename(ano = ano_sit_cadastral)

# ── REMOVER CNPJs BAIXADOS entre 2014 e 2023 ───────────────────────────
cnpjs_baixados <- empresas |>
  filter(
    ano <= 2023,                                         # qualquer ano ≤ 2023
    str_trim(str_to_upper(situacao_cadastral_desc)) == "BAIXADA"
  ) |>
  pull(cnpj_completo) |>
  unique()

empresas <- empresas |>
  filter(!cnpj_completo %in% cnpjs_baixados)

# 
# 
# expandir_situacao <- function(df_emp) {
#   anos_interesse <- 2014:2023
# 
#   df_emp |>
#     select(cnpj_completo, código_ibge, ano_ini_atividade,
#            ano, situacao_cadastral_desc) |>
#     filter(!is.na(ano)) |>
#     complete(
#       ano = anos_interesse,
#       nesting(cnpj_completo, código_ibge, ano_ini_atividade),
#       fill = list(situacao_cadastral_desc = NA)
#     ) |>
#     arrange(cnpj_completo, ano) |>
#     group_by(cnpj_completo) |>
#     mutate(
#       situacao_cadastral_desc = zoo::na.locf(situacao_cadastral_desc,
#                                              na.rm = FALSE),
#       situacao_cadastral_desc =
#         ifelse(ano < ano_ini_atividade, NA, situacao_cadastral_desc)
#     ) |>
#     mutate(
#       primeira_situacao_ano = min(ano[!is.na(situacao_cadastral_desc)],
#                                   na.rm = TRUE),
#       situacao_cadastral_desc = ifelse(
#         is.na(situacao_cadastral_desc) & ano >= ano_ini_atividade &
#           ano < primeira_situacao_ano,
#         "CARECE DE INFORMAÇÕES",
#         situacao_cadastral_desc
#       )
#     ) |>
#     select(-primeira_situacao_ano) |>
#     ungroup()
# }


expandir_situacao <- function(df_emp) {
  anos_interesse <- 2014:2023
  
  df_emp |>
    select(cnpj_completo, código_ibge, ano_ini_atividade,
           ano, situacao_cadastral_desc) |>
    filter(!is.na(ano)) |>
    complete(
      ano = anos_interesse,
      nesting(cnpj_completo, código_ibge, ano_ini_atividade),
      fill = list(situacao_cadastral_desc = NA)
    ) |>
    arrange(cnpj_completo, ano) |>
    group_by(cnpj_completo) |>
    
    mutate(
      # salvar antes da propagação
      situacao_original = situacao_cadastral_desc,
      
      # preencher para frente
      situacao_cadastral_desc = zoo::na.locf(situacao_cadastral_desc, na.rm = FALSE),
      
      # identificar primeiro ano com informação real
      primeira_situacao_ano = min(ano[!is.na(situacao_cadastral_desc)], na.rm = TRUE),
      
      # aplicar rótulos nos anos ausentes
      situacao_cadastral_desc = case_when(
        ano < ano_ini_atividade ~ "CNPJ NAO EXISTE NO PERIODO",
        ano == ano_ini_atividade & is.na(situacao_original) ~ "CNPJ CRIADO NESTE ANO",
        ano >  ano_ini_atividade & ano < primeira_situacao_ano & is.na(situacao_original) ~ "CARECE DE INFORMAÇÕES",
        TRUE ~ situacao_cadastral_desc
      )
    ) |>
    select(-situacao_original, -primeira_situacao_ano) |>
    ungroup()
}


normalizar_nome <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("[[:space:]/]", "_") |>
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT") |>
    str_replace_all("[^a-z0-9_]", "")
}

lista_planilhas <- empresas |>
  group_by(código_ibge) |>
  group_split() |>
  map(function(df_empresas) {
    codigo      <- unique(df_empresas$código_ibge)
    nome        <- unique(df_empresas$municipio_nome)
    aba         <- paste0("municipio_", codigo, "_", normalizar_nome(nome))
    
    # 1) dados de arsênio (já resumidos) desse município
    df_ars     <- arsenio_resumo |>
      filter(código_ibge == codigo)
    
    # 2) expandir situação das empresas
    df_expand  <- expandir_situacao(df_empresas)
    
    # 3) pivotar empresas (uma coluna por CNPJ) – ainda anual
    empresas_largo <- df_expand |>
      pivot_wider(
        id_cols    = c(ano, código_ibge),
        names_from = cnpj_completo,
        values_from = situacao_cadastral_desc,
        names_prefix = "cnpj_",
        values_fill  = NA
      )
    
    # 4) juntar com arsênio
    #    > arsênio tem (ano, semestre) • empresas têm apenas ano
    #    ⇒ junção por ano + código_ibge, mantendo semestre no resultado
    df_final <- left_join(df_ars, empresas_largo,
                          by = c("código_ibge", "ano")) |>
      select(-tipo_de_resultado, -data_da_coleta, -consistencia)
    
    setNames(list(df_final), aba)
  }) |>
  purrr::flatten()


write_xlsx(lista_planilhas,
           "municipios_join_cnpj_arsenio_RESUMO2.xlsx")
