# refazer tudo do script 7

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
  select(município,uf,código_ibge,tipo_de_resultado,parâmetro,data_da_coleta, consistencia, atendimento_ao_padrao)

municipios_teste_arsenio <- municipios_teste_arsenio  |> 
  mutate(data_da_coleta = dmy(data_da_coleta))
municipios_teste_arsenio <-municipios_teste_arsenio |> mutate(
  ano_coleta = year(data_da_coleta),
  semestre_coleta = if_else(month(data_da_coleta) <= 6, 1, 2)
)


df <- municipios_teste_arsenio  |> 
  mutate(data_da_coleta = dmy(data_da_coleta))   

df <- df %>%
  mutate(
    ano_coleta = year(data_da_coleta),
    semestre_coleta = if_else(month(data_da_coleta) <= 6, 1, 2))

df<-df |> 
  select(município,uf,código_ibge,tipo_de_resultado,parâmetro,ano_coleta,semestre_coleta, consistencia, atendimento_ao_padrao)



df <- df %>% 
  add_count(código_ibge, ano_coleta, semestre_coleta, name = "n_testes") |> 
  distinct(código_ibge, ano_coleta, semestre_coleta, .keep_all = TRUE)



#############
# TESTES 
# ── Pacotes ────────────────────────────────────────────────────────────


# ── 1) Carregar o arquivo e manter só as colunas de interesse ──────────
colunas <- c("município", "uf", "código_ibge", "tipo_de_resultado",
             "parâmetro", "data_da_coleta", "consistencia",
             "atendimento_ao_padrao")

df <- read_excel("municipios_teste_arsenio.xlsx",
                 .name_repair = "unique") |>  # evita nomes duplicados
  select(all_of(colunas))

# ── 2) Preparar variáveis de data, ano e semestre ──────────────────────
df <- df |>
  mutate(
    data_da_coleta   = dmy(data_da_coleta),
    ano_coleta       = year(data_da_coleta),
    semestre_coleta  = if_else(month(data_da_coleta) <= 6, 1, 2),
    # fator ordenado: “Acima do VMP” tem prioridade
    atendimento_ao_padrao = factor(
      atendimento_ao_padrao,
      levels = c("Acima do VMP", "Abaixo do VMP", "not applicable")
    )
  )

# ── 3) Adicionar contagem de testes por grupo ──────────────────────────
df <- df |>
  add_count(código_ibge, ano_coleta, semestre_coleta, name = "n_testes")

# ── 4) Manter apenas a “pior” linha (Acima do VMP > Abaixo > n/a) ─────
df_resumo <- df |>
  arrange(código_ibge, ano_coleta, semestre_coleta, atendimento_ao_padrao) |>
  distinct(código_ibge, ano_coleta, semestre_coleta, .keep_all = TRUE) |>
  relocate(n_testes, .after = semestre_coleta)

# ── 5) Resultado ───────────────────────────────────────────────────────
print(df_resumo)










