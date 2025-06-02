library(tidyverse)
library(readxl)
library(readr)
library(stringr)
library(writexl)


planilha0 <- read_csv("planilha0.csv")

dados_poluentes <- read_excel("Poluentes_Ref._Planilha_6 e 1_Samara_04-12-2024.xlsx", 
                                                             sheet = "Planilha 6 sem obs.")


parametro_com_cultivo<-dados_poluentes |> 
  filter(str_detect(str_to_lower(`Denominação Geral`), "cultivo")) |> 
  pull(Parâmetro) |> unique()
  

planilha_teste<-head(planilha0, 50)

write_xlsx(planilha_teste,
           "planilha0_resumo.xlsx")



cod_ibge<-c(411340, 520015, 520800)

dados_filtrados<-planilha0 |> 
  filter(código_ibge %in% cod_ibge)

# gr_parametro<-c("Agrotóxicos")
# 
# parametro<-c("Cobre", "Nitrato (como N)")
# 
# dados_filtrados <- dados_filtrados %>%
#   filter(grupo_de_parâmetros == "Agrotóxicos" | parâmetro %in% c("Cobre", "Nitrato (como N)"))
# 
# 
#  dados_filtrados<-dados_filtrados[, 1:31]

dados_filtrados<-dados_filtrados |> 
   filter(parâmetro %in% parametro_com_cultivo)
 
library(writexl)
write_xlsx(dados_filtrados,
           "dados_filtrados_paramentros_culturas.xlsx")




planilha0_testes_quantificados <- planilha0 |> 
  filter(tipo_de_resultado == "QUANTIFICADO")


planilha0_testes_quantificados <- planilha0_testes_quantificados |> 
  filter(consistencia == "Consistente")


write_csv(planilha0_testes_quantificados,
           "planilha0_quantificados_consistentes.csv")

planilha0_resumo<-head(planilha0_testes_quantificados,100)


write_xlsx(planilha0_resumo,
           "planilha0_quantificados_consistentes_resumo.xlsx")

# ----------



cod_ibge<-c(411340, 520015, 520800)

dados_filtrados<-planilha0_testes_quantificados |> 
  filter(código_ibge %in% cod_ibge)






dados_filtrados<-dados_filtrados[,1:31]

dados_filtrados<-dados_filtrados |> 
  filter(parâmetro %in% parametro_com_cultivo)

write_xlsx(dados_filtrados,
           "quant_consist_parametros_culturas.xlsx")



