library(sidrar)
library(dplyr)
library(purrr)
library(writexl)



get_area_agricola <- function(table_id, tipo_lavoura) {
  var <- if (tipo_lavoura == "Temporária") 8331 else 216
  
  get_sidra(
    x           = table_id,
    variable    = var,
    period      = "2014-2023",
    geo         = "City",
    geo.filter  = list("City" = c(5200159, 5208004, 4113403)),
    classific   = "all",
    category    = "all",
    format      = 3
  ) |>
    mutate(tipo_lavoura = tipo_lavoura)
}

temp_data <- get_area_agricola(5457, "Temporária")  # Área plantada
perm_data <- get_area_agricola(1613, "Permanente")  # Área colhida

dados_final <- bind_rows(temp_data, perm_data)

dados_final_limpo <- dados_final |> 
  filter(!is.na(Valor))
write_xlsx(dados_final_limpo,
           "municipios_area_planta_area_colhida.xlsx")
