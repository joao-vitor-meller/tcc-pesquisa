estaduais <- read.csv("tcc/Estaduais.csv",sep=";",header = T)
names(estaduais)
head(estaduais)

library(lubridate)
estaduais$ano <- dmy(estaduais$ano)

variaveis_significantes <- data.frame(as.numeric(estaduais$preco_terra),
                                      as.numeric(estaduais$area_soja),
                                      as.numeric(estaduais$producao_soja),
                                      as.numeric(estaduais$rendimento_trigo),
                                      as.numeric(estaduais$cotacao_soja),
                                      as.numeric(estaduais$producao_trigo))

names(variaveis_significantes)[1] <- "Preço_Terra"
names(variaveis_significantes)[2] <- "Area_Soja"
names(variaveis_significantes)[3] <- "Produção_Soja"
names(variaveis_significantes)[4] <- "Rendimento_Trigo"
names(variaveis_significantes)[5] <- "Cotacao_Soja"
names(variaveis_significantes)[6] <- "Produção_Trigo"

head(variaveis_significantes)

set.seed(123)
library(caret)
library(e1071)

#dividindo banco de dados para o modelo de regressão
dataindex <- createDataPartition(variaveis_significantes$Preço_Terra, p= .7, list=FALSE)

#seperando 70% do bd para treino e o restante para teste
e_treino <- variaveis_significantes[dataindex,]
e_teste <- variaveis_significantes[-dataindex,]

print(e_treino)
print(e_teste)

library(caret)

model_rl <- train(Preço_Terra~., data = e_treino,  method = 'lmStepAIC')

#algoritmo Random Forest
model_ranger <- train(Preço_Terra~., data = e_treino, method="ranger", importance="impurity")

# Decision Tree:
model_dt <- train(Preço_Terra~., data = e_treino, method = "rpart")

# kNN
model_knn <- train(Preço_Terra~., data = e_treino, method = "knn")

# comparando os modelos
listamodelos <- list(ranger=model_ranger, dt=model_dt, knn=model_knn, rl=model_rl)
comparacao <- resamples(listamodelos)
dotplot(comparacao)

library(caTools)
plot(varImp(model_rl)) 

#fazendo projecoes com ML
predict(model_rl, newdata = e_teste) # Com base no que aprendeu, faz projeções.

print(e_teste)

