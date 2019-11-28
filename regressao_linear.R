estaduais <- read.csv("tcc/Estaduais.csv",sep=";",header = T)
names(estaduais)
head(estaduais)

library(lubridate)
estaduais$ano <- dmy(estaduais$ano)

regressao_linear <- data.frame(as.Date(estaduais$ano),
                                      as.numeric(estaduais$preco_terra),
                                      as.numeric(estaduais$area_soja),
                                      as.numeric(estaduais$producao_soja),
                                      as.numeric(estaduais$rendimento_trigo),
                                      as.numeric(estaduais$cotacao_soja),
                                      as.numeric(estaduais$producao_trigo))

names(regressao_linear)[1] <- "Ano"
names(regressao_linear)[2] <- "Preço_Terra"
names(regressao_linear)[3] <- "Area_Soja"
names(regressao_linear)[4] <- "Produção_Soja"
names(regressao_linear)[5] <- "Rendimento_Trigo"
names(regressao_linear)[6] <- "Cotacao_Soja"
names(regressao_linear)[7] <- "Produção_Trigo"

# predizer preço da terra em 2017, 2018, 2019, 2020
rl <- lm(Preço_Terra~Ano,data=regressao_linear)
summary(rl)

plot(Preço_Terra~Ano, data=regressao_linear, type="l", col="blue", xlab="Ano", ylab="Preço da Terra")

#novo objeto para saber: Qual vai ser o preço da terra em 2018 e 2020?
AnoNovo <- data.frame(Ano=c(date("2017-12-01"),date("2018-06-01"),date("2018-12-01"),date("2019-06-01"),date("2019-12-01"),date("2020-06-01"))) #cria objeto com novos valores
predict(rl, AnoNovo, type = "response") #predizendo quanto vai ser o preço

predicao <- predict(rl, AnoNovo, type = "response")
predicao_data <- c(regressao_linear$Ano,AnoNovo$Ano)
predicao_preco <- c(regressao_linear$Preço_Terra,predicao)
predicao <- data.frame(predicao_data,predicao_preco)

plot(predicao_preco~predicao_data, data=regressao_linear, xlab="Ano", ylab="Preço da Terra", ylim=c(0,20000), xlim=c(date("1994-06-01"),date("2020-12-01")))
p <- predict(rl, AnoNovo, type = "response")
points(c(date("2017-12-01"),date("2018-06-01"),date("2018-12-01"),date("2019-06-01"),date("2019-12-01"),date("2020-06-01")), p, col="red")
summary(rl)
curve(-1.926e+04+2.006e+00*x, add = T, col="blue") #y=a+b*x

# --------------------------------------------------------2016-2019------------------------------------------------------------------------- #

rl_multiplo <- lm(formula = Preço_Terra ~ Ano + Area_Soja + Produção_Soja + 
                    Rendimento_Trigo + Cotacao_Soja + Produção_Trigo, data = regressao_linear)

summary(rl_multiplo)

step(rl_multiplo, direction = "both", scale = 967.5^2)

shapiro.test(rl_multiplo$residuals)

rl_multiplo$coefficients

#predizer preço da terra em 2018, 2019, 2020

plot(Preço_Terra~Ano, data=regressao_linear, type="l", col="blue", xlab="Ano", ylab="Preço da Terra")

plot(predicao_preco~predicao_data, data=regressao_linear, type="l", xlab="Ano", ylab="Preço da Terra", ylim=c(1000,35000), xlim=c(date("2010-06-01"),date("2025-12-01")))
p <- predict(rl, AnoNovo, type = "response")
points(c(date("2017-12-01"),date("2018-06-01"),date("2018-12-01"),date("2019-06-01"),date("2019-12-01"),date("2020-06-01")), p, col="red")

AnoNovo2 <- data.frame(Ano=c(date("2017-12-01"),date("2018-06-01"),date("2018-12-01"),date("2019-06-01"),date("2019-12-01"),date("2020-06-01")), 
                       Area_Soja=c(5464087,5541880,5541880,5709034,5709034,5778000), 
                       Produção_Soja= c(16209892,18744181,18744181,17538575,17538575,19187000),
                       Rendimento_Trigo= c(3265,1777,1777,2469,2469,2192), 
                       Cotacao_Soja=c(1005,948,962,862,894,881),
                       Produção_Trigo=c(2541889,1226474,1226474,1753099,1753099,1620894))

predict(rl_multiplo, AnoNovo2, type = "response")
ps <- predict(rl_multiplo, AnoNovo2, type = "response")
points(c(date("2017-12-01"),date("2018-06-01"),date("2018-12-01"),date("2019-06-01"),date("2019-12-01"),date("2020-06-01")), ps, col="blue")

# --------------------------------------------------------2012-2015------------------------------------------------------------------------- #

p <- predict(rl, AnoNovo, type = "response")
points(c(date("2017-12-01"),date("2018-06-01"),date("2018-12-01"),date("2019-06-01"),date("2019-12-01"),date("2020-06-01")), p, col="red")

AnoNovo2 <- data.frame(Ano=c(date("2017-12-01"),date("2018-06-01"),date("2018-12-01"),date("2019-06-01"),date("2019-12-01"),date("2020-06-01")), 
                       Area_Soja=c(2134624,2363917,2363917,2493271,2493271,2631950), 
                       Produção_Soja= c(2972622,6378289,6378289,6520613,6520613,7850132),
                       Rendimento_Trigo= c(1941,3164,3164,1417,1417,1592), 
                       Cotacao_Soja=c(1570,1444,1296,1415,1019,987),
                       Produção_Trigo=c(933127,1675575,1675575,835312,835312,695915))

predict(rl_multiplo, AnoNovo2, type = "response")
ps <- predict(rl_multiplo, AnoNovo2, type = "response")
points(c(date("2017-12-01"),date("2018-06-01"),date("2018-12-01"),date("2019-06-01"),date("2019-12-01"),date("2020-06-01")), ps, col="green")

# ----------------------------------------------------2013-2016-------------------------------------------------------------------------- #

p <- predict(rl, AnoNovo, type = "response")
points(c(date("2017-12-01"),date("2018-06-01"),date("2018-12-01"),date("2019-06-01"),date("2019-12-01"),date("2020-06-01")), p, col="red")

AnoNovo2 <- data.frame(Ano=c(date("2017-12-01"),date("2018-06-01"),date("2018-12-01"),date("2019-06-01"),date("2019-12-01"),date("2020-06-01")), 
                       Area_Soja=c(2363917,2493271,2493271,2631950,2631950,2732042), 
                       Produção_Soja= c(6378289,6520613,6520613,7850132,7850132,8104946),
                       Rendimento_Trigo= c(3164,1417,1417,1592,1592,3265), 
                       Cotacao_Soja=c(1296,1296,1019,987,894,990),
                       Produção_Trigo=c(1675575,835312,835312,695915,695915,1270945))

predict(rl_multiplo, AnoNovo2, type = "response")
ps <- predict(rl_multiplo, AnoNovo2, type = "response")
points(c(date("2017-12-01"),date("2018-06-01"),date("2018-12-01"),date("2019-06-01"),date("2019-12-01"),date("2020-06-01")), ps, col="yellow")

# --------------------------------------------------2014-2017------------------------------------------------------------------------------- #

p <- predict(rl, AnoNovo, type = "response")
points(c(date("2017-12-01"),date("2018-06-01"),date("2018-12-01"),date("2019-06-01"),date("2019-12-01"),date("2020-06-01")), p, col="red")

AnoNovo2 <- data.frame(Ano=c(date("2017-12-01"),date("2018-06-01"),date("2018-12-01"),date("2019-06-01"),date("2019-12-01"),date("2020-06-01")), 
                       Area_Soja=c(2493271,2631950,2631950,2732042,2732042,2770940), 
                       Produção_Soja= c(6520613,7850132,7850132,8104946,8104946,9372091),
                       Rendimento_Trigo= c(1417,1592,1592,3265,3265,1777), 
                       Cotacao_Soja=c(1019,987,894,990,995,971),
                       Produção_Trigo=c(835312,695915,695915,1270945,1270945,613237))

predict(rl_multiplo, AnoNovo2, type = "response")
ps <- predict(rl_multiplo, AnoNovo2, type = "response")
points(c(date("2017-12-01"),date("2018-06-01"),date("2018-12-01"),date("2019-06-01"),date("2019-12-01"),date("2020-06-01")), ps, col="black")





