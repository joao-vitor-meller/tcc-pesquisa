regioes <- read.csv("C:/Users/vitor/OneDrive/Documentos/tcc/regioes.csv",sep=";",header = T,fileEncoding = "UTF-8")
names(regioes)
head(regioes)

library(factoextra)
library(FactoMineR)
library(Factoshiny)

regioes_atualizadas <- data.frame(factor(regioes$regiao),
                                  factor(regioes$municipio),
                                  factor(regioes$atividade),
                                  factor(regioes$produção), 
                                  as.numeric(regioes$ano), 
                                  as.numeric(regioes$valor))

regioes_atualizadas <- data.frame(as.numeric(regioes$regiao),
                                  as.numeric(regioes$municipio),
                                  as.numeric(regioes$atividade),
                                  as.numeric(regioes$produção), 
                                  as.numeric(regioes$ano), 
                                  as.numeric(regioes$valor))

names(regioes_atualizadas)[1] <- "Região"
names(regioes_atualizadas)[2] <- "Municipio"
names(regioes_atualizadas)[3] <- "Atividade"
names(regioes_atualizadas)[4] <- "Produção"
names(regioes_atualizadas)[5] <- "Ano"
names(regioes_atualizadas)[6] <- "Valor"

names(regioes_atualizadas)


rl_regioes <- lm(Valor ~ Região + Municipio + Atividade + Produção + Ano , data = regioes_atualizadas)

summary(rl_regioes)

step(rl_regioes, direction = "both", scale =8136^2)

dados.anova <- aov(regioes_atualizadas$Valor ~  regioes_atualizadas$Região + regioes_atualizadas$Municipio + 
                     regioes_atualizadas$Atividade +regioes_atualizadas$Produção + regioes_atualizadas$Ano )

summary(dados.anova)

#res_pca <- PCAshiny(regioes_atualizadas)


res.pca <- prcomp(regioes_atualizadas, scale = TRUE)

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

set.seed(123)
res.km <- kmeans(var$coord, centers = 3, nstart = 25)

grp <- as.factor(res.km$cluster)
# Color variables by groups

fviz_pca_var(res.pca, col.var = grp, 
             palette = c("#00AFBB","#FC4E07","#E7B800"),
             legend.title = "Cluster")


matcor <- round(cor(regioes_atualizadas), 2)
matcor

library(ggcorrplot)

ggcorrplot(matcor, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlograma", 
           ggtheme=theme_bw)

p.mat <- cor_pmat(regioes_atualizadas)

ggcorrplot(matcor,
           hc.order = TRUE,
           type = "lower",
           p.mat = p.mat)

set.seed(123)
library(caret)
library(e1071)

#dividindo banco de dados para o modelo de regressão
dataindex <- createDataPartition(regioes_atualizadas$Valor, p= .7, list=FALSE)

#seperando 70% do bd para treino e o restante para teste
e_treino <- regioes_atualizadas[dataindex,]
e_teste <- regioes_atualizadas[-dataindex,]

#algoritmo Random Forest
modeloML2 <- train(Valor~., data = e_treino, method="ranger", importance="impurity")

# Decision Tree:
model_dt <- train(Valor~., data = e_treino, method = "rpart")

# kNN
model_knn <- train(Valor~., data = e_treino, method = "knn")

# comparando os modelos
listamodelos <- list(ranger=modeloML2, dt=model_dt, knn=model_knn)
comparacao <- resamples(listamodelos)
dotplot(comparacao)

library(caTools) 
plot(varImp(modeloML2)) 
