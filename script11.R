library(readxl)
library(tidyverse)


dados <- read_excel("municipios_area_planta_area_colhida.xlsx")
View(dados)


# CULTURAS TEMPORARIAS-------------

dados_area_temporaria<-dados |> 
  filter(tipo_lavoura == "Temporária") 
dados_area_temporaria<-dados_area_temporaria[,1:10]

dados_area_temporaria_winde<- dados_area_temporaria |> 
  pivot_wider(
    names_from = `Produto das lavouras temporárias e permanentes`,
    values_from = Valor,
    values_fill = 0
  )




write_xlsx(dados_area_temporaria_winde,
           "area_cultura_temporaria.xlsx")


# CULTARAS PERMANENTES -------------------

dados_area_permanente <- dados |> 
  filter(tipo_lavoura == "Permanente")

dados_area_permanente<-dados_area_permanente |> 
  select(-c(`Produto das lavouras temporárias e permanentes`))


dados_area_permanente_winde<- dados_area_permanente |> 
  pivot_wider(
    names_from = `Produto das lavouras permanentes`,
    values_from = Valor,
    values_fill = 0
  )


write_xlsx(dados_area_permanente_winde,
           "area_cultura_permanente.xlsx")


