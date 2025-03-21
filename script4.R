library(dplyr)
library(purrr)
library(tibble)


#### ACRILAMIDA ---- 

# Identifica todas as colunas que começam com "cnae_"
cnae_cols <- grep("^cnae_", names(tabela_acrilamida), value = TRUE)

# Função que retorna um tibble com estatísticas do teste para cada cnae
resultados <- map_dfr(cnae_cols, ~{
  temp <- tabela_acrilamida |>
    mutate(
      num_empresa = ifelse(.data[[.x]] > 0, 1, 0),
      deteccao   = ifelse(Total_Detectados > 0, 1, 0)
    )
  
  tabela_contigencia <- table(temp$num_empresa,temp$deteccao)
  frequencias_esperadas <- chisq.test(tabela_contigencia, correct = FALSE)$expected
  
  if (all(frequencias_esperadas >=5)){
    teste<- chisq.test(tabela_contigencia)
    metodo <- "Qui-Quadrado"
  }
  else{
    teste <- fisher.test(tabela_contigencia)
    metodo <- "Fisher"
  }
  
  
  teste <- chisq.test(table(temp$num_empresa, temp$deteccao))
  
  tibble(
    cnae          = .x,
    p_value       = teste$p.value,
    #statistic     = as.numeric(teste$statistic),
    #df            = as.integer(teste$parameter),
    metodo = metodo,
    significativo = ifelse(teste$p.value < 0.05, "Sim", "Não")
  )
})

# Visualizar resultados ordenados pelo menor p‑valor
resultados |> arrange(p_value)


#### ANTIMONIO ---- 

# Identifica todas as colunas que começam com "cnae_"
cnae_cols <- grep("^cnae_", names(tabela_antimonio), value = TRUE)

# Função que retorna um tibble com estatísticas do teste para cada cnae
resultados <- map_dfr(cnae_cols, ~{
  temp <- tabela_antimonio |>
    mutate(
      num_empresa = ifelse(.data[[.x]] > 0, 1, 0),
      deteccao   = ifelse(Total_Detectados > 0, 1, 0)
    )
  
  tabela_contigencia <- table(temp$num_empresa,temp$deteccao)
  frequencia_esperada <- chisq.test(tabela_contigencia, correct = FALSE)$expected

  if(all(frequencia_esperada >=5)){
    teste <- chisq.test(tabela_contigencia)
    metodo <- "Qui-Quadrado"
  }
  else{
    teste <- fisher.test(tabela_contigencia)
    metodo <- "Fisher"
  }
    
  tibble(
    cnae = .x,
    p_value = teste$p.value,
    metodo = metodo,  
    #statistic     = as.numeric(teste$statistic),
    #df            = as.integer(teste$parameter),
    significativo = ifelse(teste$p.value < 0.05, "Sim", "Não")
  )
})

# Visualizar resultados ordenados pelo menor p‑valor
resultados |> arrange(p_value)

 #### ARSENIO ---- 

# Identifica todas as colunas que começam com "cnae_"
cnae_cols <- grep("^cnae_", names(tabela_arsenio), value = TRUE)

# Função que retorna um tibble com estatísticas do teste para cada cnae
resultados <- map_dfr(cnae_cols, ~{
  temp <- tabela_arsenio |>
    mutate(
      num_empresa = ifelse(.data[[.x]] > 0, 1, 0),
      deteccao   = ifelse(Total_Detectados > 0, 1, 0)
    )
  
  tabela_contigencia <- table(temp$num_empresa,temp$deteccao)
  frequencia_esperada <- chisq.test(tabela_contigencia, correct = FALSE)$expected
  
  if(all(frequencia_esperada >= 5)){
    teste <- chisq.test(tabela_contigencia)
    metodo <- "Qui-Quadrado"
  }
  else{
    teste <- fisher.test(tabela_contigencia)
    metodo <- "Fisher"
  }
  tibble(
    cnae = .x,
    p_value = teste$p.value,
    metodo =  metodo, 
    #statistic     = as.numeric(teste$statistic),
    #df            = as.integer(teste$parameter),
    significativo = ifelse(teste$p.value < 0.05, "Sim", "Não")
  )
})

# Visualizar resultados ordenados pelo menor p‑valor
resultados |> arrange(p_value)

#### BÁRIO ---- 

cnae_cols <- grep("^cnae_", names(tabela_bario), value = TRUE)

# Função que retorna um tibble com estatísticas do teste para cada cnae
resultados <- map_dfr(cnae_cols, ~{
  temp <- tabela_bario |>
    mutate(
      num_empresa = ifelse(.data[[.x]] > 0, 1, 0),
      deteccao   = ifelse(Total_Detectados > 0, 1, 0)
    )
  
  tabela_contigencia <- table(temp$num_empresa, temp$deteccao)
  frequencia_esperada <- chisq.test(tabela_contigencia, correct = FALSE)$expected
  
  if(all(frequencia_esperada >= 5)){
    teste <- chisq.test(tabela_contigencia)
    metodo <- "Qui-Quadrado"
  }
  else{
    teste <- fisher.test(tabela_contigencia)
    metodo <- "Fisher"
  }
  
  tibble(
    cnae          = .x,
    p_value       = teste$p.value,
    metodo = metodo, 
    #statistic     = as.numeric(teste$statistic),
    #df            = as.integer(teste$parameter),
    significativo = ifelse(teste$p.value < 0.05, "Sim", "Não")
  )
})

# Visualizar resultados ordenados pelo menor p‑valor
resultados |> arrange(p_value)


#### CADMIO ----

cnae_cols <- grep("^cnae_", names(tabela_cadmio), value = TRUE)

# Função que retorna um tibble com estatísticas do teste para cada cnae
resultados <- map_dfr(cnae_cols, ~{
  temp <- tabela_cadmio |>
    mutate(
      num_empresa = ifelse(.data[[.x]] > 0, 1, 0),
      deteccao   = ifelse(Total_Detectados > 0, 1, 0)
    )
  
  tabela_contigencia <- table(temp$num_empresa, temp$deteccao)
  frequencia_esperada <- chisq.test(tabela_contigencia, correct = FALSE)$expected
  
  if(all(frequencia_esperada >= 5)){
    teste <- chisq.test(tabela_contigencia)
    metodo <- "Qui-Quadrado"
  }
  else{
    teste <- fisher.test(tabela_contigencia)
    metodo <- "Fisher"
  }
  
  tibble(
    cnae          = .x,
    p_value       = teste$p.value,
    metodo =  metodo,
    #statistic     = as.numeric(teste$statistic),
    #df            = as.integer(teste$parameter),
    significativo = ifelse(teste$p.value < 0.05, "Sim", "Não")
  )
})

# Visualizar resultados ordenados pelo menor p‑valor
resultados |> arrange(p_value)


#### CHUMBO ----

cnae_cols <- grep("^cnae_", names(tabela_chumbo), value = TRUE)

# Função que retorna um tibble com estatísticas do teste para cada cnae
resultados <- map_dfr(cnae_cols, ~{
  temp <- tabela_chumbo |>
    mutate(
      num_empresa = ifelse(.data[[.x]] > 0, 1, 0),
      deteccao   = ifelse(Total_Detectados > 0, 1, 0)
    )
  
  tabela_contigencia <- table(temp$num_empresa,temp$deteccao)
  frequencia_esperada <- chisq.test(tabela_contigencia, correct = FALSE)$expected


  if(all(frequencia_esperada >= 5)){
    teste <- chisq.test(tabela_contigencia)
    metodo <- "Qui-Quadrado"
  }
  else{
    teste <- fisher.test(tabela_contigencia)
    metodo <- "Fisher"
  }
    
  tibble(
    cnae          = .x,
    p_value       = teste$p.value,
    metodo = metodo,
    #statistic     = as.numeric(teste$statistic),
    #df            = as.integer(teste$parameter),
    significativo = ifelse(teste$p.value < 0.05, "Sim", "Não")
  )
})

# Visualizar resultados ordenados pelo menor p‑valor
resultados |> arrange(p_value)


#### CROMO ----

cnae_cols <- grep("^cnae_", names(tabela_cromo), value = TRUE)

# Função que retorna um tibble com estatísticas do teste para cada cnae
resultados <- map_dfr(cnae_cols, ~{
  temp <- tabela_cromo |>
    mutate(
      num_empresa = ifelse(.data[[.x]] > 0, 1, 0),
      deteccao   = ifelse(Total_Detectados > 0, 1, 0)
    )
  
  tabela_contigencia <- table(temp$num_empresa, temp$deteccao)
  frequencia_esperada <- chisq.test(tabela_contigencia, correct = FALSE)$expected
  
  if(all(frequencia_esperada >= 5)){
    teste<-chisq.test(tabela_contigencia)
    metodo<-"Qui-Quadrado"
  }
  else{
    teste<-fisher.test(tabela_contigencia)
    metodo <- "Fisher"
  }
  
  tibble(
    cnae          = .x,
    p_value       = teste$p.value,
    metodo = metodo,
    #statistic     = as.numeric(teste$statistic),
    #df            = as.integer(teste$parameter),
    significativo = ifelse(teste$p.value < 0.05, "Sim", "Não")
  )
})

# Visualizar resultados ordenados pelo menor p‑valor
resultados |> arrange(p_value)



#### COBRE ----

cnae_cols <- grep("^cnae_", names(tabela_cobre), value = TRUE)

# Função que retorna um tibble com estatísticas do teste para cada cnae
resultados <- map_dfr(cnae_cols, ~{
  temp <- tabela_cobre |>
    mutate(
      num_empresa = ifelse(.data[[.x]] > 0, 1, 0),
      deteccao   = ifelse(Total_Detectados > 0, 1, 0)
    )
  
  tabela_contigencia <- table(temp$num_empresa, temp$deteccao)
  frequencia_esperada <- chisq.test(tabela_contigencia, correct = FALSE)$expected
  
  if(all(frequencia_esperada >= 5)){
    teste<-chisq.test(tabela_contigencia)
    metodo<-"Qui-Quadrado"
  }
  else{
    teste<-fisher.test(tabela_contigencia)
    metodo <- "Fisher"
  }
  
  tibble(
    cnae          = .x,
    p_value       = teste$p.value,
    metodo = metodo,
    #statistic     = as.numeric(teste$statistic),
    #df            = as.integer(teste$parameter),
    significativo = ifelse(teste$p.value < 0.05, "Sim", "Não")
  )
})

# Visualizar resultados ordenados pelo menor p‑valor
resultados |> arrange(p_value)

View(resultados |> filter(significativo == "Sim"))



#### NIQUEL ----


cnae_cols <- grep("^cnae_", names(tabela_niquel), value = TRUE)

# Função que retorna um tibble com estatísticas do teste para cada cnae
resultados <- map_dfr(cnae_cols, ~{
  temp <- tabela_niquel |>
    mutate(
      num_empresa = ifelse(.data[[.x]] > 0, 1, 0),
      deteccao   = ifelse(Total_Detectados > 0, 1, 0)
    )
  
  tabela_contigencia <- table(temp$num_empresa, temp$deteccao)
  frequencia_esperada <- chisq.test(tabela_contigencia, correct = FALSE)$expected
  
  if(all(frequencia_esperada >= 5)){
    teste<-chisq.test(tabela_contigencia)
    metodo<-"Qui-Quadrado"
  }
  else{
    teste<-fisher.test(tabela_contigencia)
    metodo <- "Fisher"
  }
  
  tibble(
    cnae          = .x,
    p_value       = teste$p.value,
    metodo = metodo,
    #statistic     = as.numeric(teste$statistic),
    #df            = as.integer(teste$parameter),
    significativo = ifelse(teste$p.value < 0.05, "Sim", "Não")
  )
})

# Visualizar resultados ordenados pelo menor p‑valor
resultados |> arrange(p_value)

View(resultados |> filter(significativo == "Sim"))



#### SELENIO ----

cnae_cols <- grep("^cnae_", names(tabela_selenio), value = TRUE)

# Função que retorna um tibble com estatísticas do teste para cada cnae
resultados <- map_dfr(cnae_cols, ~{
  temp <- tabela_selenio |>
    mutate(
      num_empresa = ifelse(.data[[.x]] > 0, 1, 0),
      deteccao   = ifelse(Total_Detectados > 0, 1, 0)
    )
  
  tabela_contigencia <- table(temp$num_empresa, temp$deteccao)
  frequencia_esperada <- chisq.test(tabela_contigencia, correct = FALSE)$expected
  
  if(all(frequencia_esperada >= 5)){
    teste<-chisq.test(tabela_contigencia)
    metodo<-"Qui-Quadrado"
  }
  else{
    teste<-fisher.test(tabela_contigencia)
    metodo <- "Fisher"
  }
  
  tibble(
    cnae          = .x,
    p_value       = teste$p.value,
    metodo = metodo,
    #statistic     = as.numeric(teste$statistic),
    #df            = as.integer(teste$parameter),
    significativo = ifelse(teste$p.value < 0.05, "Sim", "Não")
  )
})

# Visualizar resultados ordenados pelo menor p‑valor
resultados |> arrange(p_value)

View(resultados |> filter(significativo == "Sim"))


#### NITRATO ----

cnae_cols <- grep("^cnae_", names(tabela_nitrato), value = TRUE)

# Função que retorna um tibble com estatísticas do teste para cada cnae
resultados <- map_dfr(cnae_cols, ~{
  temp <- tabela_nitrato |>
    mutate(
      num_empresa = ifelse(.data[[.x]] > 0, 1, 0),
      deteccao   = ifelse(Total_Detectados > 0, 1, 0)
    )
  
  tabela_contigencia <- table(temp$num_empresa, temp$deteccao)
  frequencia_esperada <- chisq.test(tabela_contigencia, correct = FALSE)$expected
  
  if(all(frequencia_esperada >= 5)){
    teste<-chisq.test(tabela_contigencia)
    metodo<-"Qui-Quadrado"
  }
  else{
    teste<-fisher.test(tabela_contigencia)
    metodo <- "Fisher"
  }
  
  tibble(
    cnae          = .x,
    p_value       = teste$p.value,
    metodo = metodo,
    #statistic     = as.numeric(teste$statistic),
    #df            = as.integer(teste$parameter),
    significativo = ifelse(teste$p.value < 0.05, "Sim", "Não")
  )
})

# Visualizar resultados ordenados pelo menor p‑valor
resultados |> arrange(p_value)

View(resultados |> filter(significativo == "Sim"))



#### APENAS CONSISTENTE ----

#### ACRILAMIDA ----

cnae_cols <- grep("^cnae_", names(tabela_acrilamida_filtrada), value = TRUE)

resultados_nitrato <- map_dfr(cnae_cols, ~{
  temp <- tabela_nitrato_filtrada |> 
    mutate(
      num_empresa = ifelse(.data[[.x]] > 0, 1, 0),
      deteccao   = ifelse(Total_Detectados > 0, 1, 0)
    )
  tabela_contigencia <- table(temp$num_empresa, temp$deteccao)
  frequencia_esperada <- chisq.test(tabela_contigencia, correct = FALSE)$expected
  
  if(all(frequencia_esperada >= 5)){
    teste<-chisq.test(tabela_contigencia)
    metodo<-"Qui-Quadrado"
  }
  else{
    teste<-fisher.test(tabela_contigencia)
    metodo <- "Fisher"
  }
  
  tibble(
    cnae          = .x,
    p_value       = teste$p.value,
    metodo = metodo,
    #statistic     = as.numeric(teste$statistic),
    #df            = as.integer(teste$parameter),
    significativo = ifelse(teste$p.value < 0.05, "Sim", "Não")
  )
  
})



#### NITRATO FILTRADO----

cnae_cols <- grep("^cnae_", names(tabela_nitrato_filtrada), value = TRUE)

# Aplica o teste para cada cnae e monta um tibble com os resultados
resultados_nitrato <- map_dfr(cnae_cols, ~{
  temp <- tabela_nitrato_filtrada |>
    mutate(
      num_empresa = ifelse(.data[[.x]] > 0, 1, 0),
      deteccao   = ifelse(Total_Detectados > 0, 1, 0)
    )
  
  tabela_contigencia <- table(temp$num_empresa, temp$deteccao)
  frequencia_esperada <- chisq.test(tabela_contigencia, correct = FALSE)#$expected
  
  if(all(frequencia_esperada >= 5)){
    teste<-chisq.test(tabela_contigencia)
    metodo<-"Qui-Quadrado"
  }
  else{
    teste<-fisher.test(tabela_contigencia)
    metodo <- "Fisher"
  }
  
  tibble(
    cnae          = .x,
    p_value       = teste$p.value,
    metodo = metodo,
    #statistic     = as.numeric(teste$statistic),
    #df            = as.integer(teste$parameter),
    significativo = ifelse(teste$p.value < 0.05, "Sim", "Não")
  )
})

View(resultados_nitrato)


#### ANTIMONIO FILTRADO----

cnae_cols <- grep("^cnae_", names(tabela_antimonio_filtrada), value = TRUE)

# Aplica o teste para cada cnae e monta um tibble com os resultados
resultados_antimonio <- map_dfr(cnae_cols, ~{
  temp <- tabela_antimonio_filtrada |>
    mutate(
      num_empresa = ifelse(.data[[.x]] > 0, 1, 0),
      deteccao   = ifelse(Total_Detectados > 0, 1, 0)
    )
  
  
})

resultados_antimonio


#### ACRILAMIDA FILTRADO----

cnae_cols <- grep("^cnae_", names(tabela_acrilamida_filtrada), value = TRUE)

# Aplica o teste para cada cnae e monta um tibble com os resultados
resultados_acrilamida <- map_dfr(cnae_cols, ~{
  temp <- tabela_acrilamida_filtrada |>
    mutate(
      num_empresa = ifelse(.data[[.x]] > 0, 1, 0),
      deteccao   = ifelse(Total_Detectados > 0, 1, 0)
    )
  
  # Cria a tabela de contingência
  tab_cont <- table(temp$num_empresa, temp$deteccao)
  
  # Verifica se a tabela possui pelo menos 2 categorias em cada variável
  if(nrow(tab_cont) < 2 || ncol(tab_cont) < 2) {
    tibble(
      cnae          = .x,
      p_value       = NA_real_,
      significativo = "Teste não aplicável"
    )
  } else {
    teste <- chisq.test(tab_cont)
    tibble(
      cnae          = .x,
      p_value       = teste$p.value,
      significativo = ifelse(teste$p.value < 0.05, "Sim", "Não")
    )
  }
})

resultados_acrilamida



#### COBRE FILTRADO----
cnae_cols <- grep("^cnae_", names(tabela_cobre_filtrada), value = TRUE)

# Aplica o teste para cada cnae e monta um tibble com os resultados
resultados_cobre <- map_dfr(cnae_cols, ~{
  temp <- tabela_cobre_filtrada |>
    mutate(
      num_empresa = ifelse(.data[[.x]] > 0, 1, 0),
      deteccao   = ifelse(Total_Detectados > 0, 1, 0)
    )
  
  # Cria a tabela de contingência
  tab_cont <- table(temp$num_empresa, temp$deteccao)
  
  # Verifica se a tabela possui pelo menos 2 categorias em cada variável
  if(nrow(tab_cont) < 2 || ncol(tab_cont) < 2) {
    tibble(
      cnae          = .x,
      p_value       = NA_real_,
      significativo = "Teste não aplicável"
    )
  } else {
    teste <- chisq.test(tab_cont)
    tibble(
      cnae          = .x,
      p_value       = teste$p.value,
      significativo = ifelse(teste$p.value < 0.05, "Sim", "Não")
    )
  }
})

View(resultados_cobre)













