estaduais <- read.csv("tcc/Estaduais.csv",sep=";",header = T)
names(estaduais)
head(estaduais)

library(factoextra)
library(FactoMineR)
library(Factoshiny)

culturas <- data.frame(as.numeric(estaduais$preco_terra),
                       as.numeric(estaduais$area_soja),as.numeric(estaduais$producao_soja),as.numeric(estaduais$rendimento_soja),
                       as.numeric(estaduais$area_trigo),as.numeric(estaduais$producao_trigo),as.numeric(estaduais$rendimento_trigo),
                       as.numeric(estaduais$area_milho),as.numeric(estaduais$producao_milho),as.numeric(estaduais$rendimento_milho),
                       as.numeric(estaduais$cotacao_dolar),as.numeric(estaduais$cotacao_soja), as.numeric(estaduais$cotacao_trigo),as.numeric(estaduais$cotacao_milho),
                       as.numeric(estaduais$ibovespa), as.numeric(estaduais$ipca),as.numeric(estaduais$taxa_selic))

names(culturas)[1] <- "Preço_Terra"
names(culturas)[2] <- "Area_Soja"
names(culturas)[3] <- "Produção_Soja"
names(culturas)[4] <- "Rendimento_Soja"
names(culturas)[5] <- "Area_Trigo"
names(culturas)[6] <- "Produção_Trigo"
names(culturas)[7] <- "Rendimento_Trigo"
names(culturas)[8] <- "Area_Milho"
names(culturas)[9] <- "Produção_Milho"
names(culturas)[10] <- "Rendimento_Milho"
names(culturas)[11] <- "Cotação_Dólar"
names(culturas)[12] <- "Cotação_Soja"
names(culturas)[13] <- "Cotação_Trigo"
names(culturas)[14] <- "Cotação_Milho"
names(culturas)[15] <- "Ibovespa"
names(culturas)[16] <- "IPCA"
names(culturas)[17] <- "Selic"

#anova
dados.anova <- aov(culturas$Preço_Terra ~ culturas$Area_Soja + culturas$Produção_Soja + culturas$Rendimento_Soja +
                     culturas$Area_Trigo + culturas$Produção_Trigo + culturas$Rendimento_Trigo +
                     culturas$Area_Milho + culturas$Produção_Milho + culturas$Rendimento_Milho +
                     culturas$Cotação_Dólar + culturas$Cotação_Soja + culturas$Cotação_Trigo + culturas$Cotação_Milho +
                     culturas$Ibovespa + culturas$IPCA + culturas$Selic)

summary(dados.anova)

anova(dados.anova)

#shapiro-test
shapiro.test(resid(dados.anova))

#função que gera shiny de PCA
#res_pca <- PCAshiny(culturas)

#z-score
res.pca <- prcomp(culturas, scale = TRUE)
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

get_eigenvalue(res.pca)

var <- get_pca_var(res.pca)

head(var$coord)
head(var$cos2)
var$contrib

fviz_contrib(res.pca, choice = "var", axes = 1:3)

library("corrplot")
corrplot(var$cos2, is.corr=FALSE)
corrplot(var$contrib, is.corr=FALSE)  

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#correlograma
matcor <- round(cor(culturas), 2)
matcor

cor(culturas)

heatmap(abs(cor(culturas)))

library(ggcorrplot)

ggcorrplot(matcor, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlograma", 
           ggtheme=theme_bw)

p.mat <- cor_pmat(culturas)

ggcorrplot(matcor,
           hc.order = TRUE,
           type = "lower",
           p.mat = p.mat)


