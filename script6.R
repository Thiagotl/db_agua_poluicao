library(tidyverse)
library(readr)

library(writexl)

dados<- read_csv("total_sisagua_anos_2014_2022_download_14_set_2024.csv")

View(dados)

attach(dados)

dados_arsenio <- dados |> 
  filter(parâmetro=="Arsênio")


municipios_desejdos<-c("VARZEA DA PALMA","LUZIANIA","CAMPINA VERDE", "APARECIDA DE GOIANIA",
                       "SAUDADE DO IGUACU","FAXINAL DO SOTURNO","SAO JOSE DO INHACORA",
                       "ANAPOLIS","PIEDADE","JUQUITIBA","SANTO CRISTO","PIRAPORA DO BOM JESUS",
                       "RUBIATABA","CAMPINA DAS MISSOES","JATAI","ALUMINIO","HIDROLANDIA",
                       "SANTO ANGELO","SÃO SEBASTIAO","ARMAZEM","BIRITIBA-MIRIM","RIO GRANDE",
                       "ARAGUAINA","CERRO LARGO")


dados_arsenio <- dados_arsenio |> 
  filter(município %in% municipios_desejdos)

View(dados_arsenio)


write_xlsx(dados_arsenio, "municipios_teste_arsenio.xlsx")


