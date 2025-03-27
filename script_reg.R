 teste<-dados_sisagua_p7_agrupados |> 
   filter(parâmetro=="Arsênio")

teste<-teste[,-c(1,2,4,5,7,8,9,10,13,14)]
library(readr)
dados_combinado <- read_csv("dados_combinado.csv")
View(dados_combinado)
attach(dados_combinado)

teste<-dados_combinado |> 
  filter(parâmetro=="Arsênio",
         consistente == 1) 


#teste<-teste[,-c(1,2,4,5,7,8,9,10,13,14)]

teste<-teste |> 
  select(uf, `Total de inconsistentes`, Total_Detectados, prop1, `724301`,	
         `724302`,	`729403`,
         `729404`,	`1510600`,	
         `1610205`,	`2019399`	,`2312500`	,`2449199`	,`2610800`)

model<-gamlss::gamlss(prop1 ~ ., family = "BEINF", data=teste[,-1])
summary(model)

library(gamlss)

model_gaic<-stepGAIC(model)

summary(model_gaic)

Rsq(model_gaic)

shapiro.test(model_gaic$residuals)

plot(model_gaic)
