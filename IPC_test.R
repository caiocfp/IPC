####pacote####

library(dplyr)
library(plot3D)
library(ggplot2)
library(lmtest)
library(normtest)
library(fpc)
library(cluster)
library(NbClust)
library(factoextra)
library(xlsx)
library(glm2)
library(boot)

###planilhas###

ipc1 <- read.csv('IPCMaps_2019_Mun.csv',
                 header = T,
                 sep = ";",
                 dec = ",",
                 stringsAsFactor = T)


names(ipc1)


##algumas interacoes##

#uma funcao de utilidade cobb#

model <- function(x,y) {(x**(x/y))*((y)**((x/y)-1))}

x <- ipc1$PotCONS_BEB_TOTAL   
y <- ipc1$PotCONS_ALIM_DOM_TOTAL




cobb <- model(x,y)

cobb <- as.matrix(cobb)

scatter3D(x , y, cobb, phi = 0, bty ="g")






####distancia pib#####


plot(ipc2$CONS_PCAP_URB,
     ipc2$PotCONS_MatrMens_TOTAL,
     xlab = "consumo per capita Urbano",
     ylab = " potencial de matrículas e mensalidades",
     main = "ensino e renda",
     pch  =  20,
     col  = "red")

dist_cons <- cbind(ipc1$CONS_PCAP_URB,
                   ipc1$DISTANCIA_CAP)

colnames(dist_cons) <- c("consumo_capita",
                          "distancia_capital")

dist_cons <- as.data.frame(dist_cons)

grau <- 1:9
cv.error = rep(0,9)



for(g in grau) {
  dist.fit = glm(consumo_capita~poly(distancia_capital, g),
              data = dist_cons)
  cv.error[g] = cv.glm(dist_cons,
                        dist.fit,
                        K = 10)$delta[1]         
}


####descritiva referente as especificacoes#####
summary(ipc1$DISTANCIA_CAP)

ipc2 <- ipc1 %>%
         subset(DISTANCIA_CAP <= 150)

ipc3 <- ipc1 %>%
         subset(DISTANCIA_CAP > 150)

summary(ipc2$DISTANCIA_CAP)


#potencial de consumo#
summary(ipc2$PotCONS_MatrMens_TOTAL)

par(mfrow = c(3,3))

plot(ipc2$DISTANCIA_CAP,
     ipc2$PotCONS_MatrMens_TOTAL,
     xlab = "distância da capital",
     ylab = "pdc matrícula-mensalidade",
     main = "TOTAL",
     col = "black",
     pch = 20)
     

plot(ipc2$DISTANCIA_CAP,
     ipc2$PotCONS_MatrMens_A,
     xlab = "distância da capital",
     ylab = "pdc matrícula-mensalidade",
     main = "A",
     col = "blue",
     pch = 20)

plot(ipc2$DISTANCIA_CAP,
     ipc2$PotCONS_MatrMens_B1,
     xlab = "distância da capital",
     ylab = "pdc matrícula-mensalidade",
     main = "B1",
     col = "green",
     pch = 20)

plot(ipc2$DISTANCIA_CAP,
     ipc2$PotCONS_MatrMens_B2,
     xlab = "distância da capital",
     ylab = "pdc matrícula-mensalidade",
     main = "B2",
     col = "purple",
     pch = 20)

plot(ipc2$DISTANCIA_CAP,
     ipc2$PotCONS_MatrMens_C1,
     xlab = "distância da capital",
     ylab = "pdc matrícula-mensalidade",
     main = "C1",
     col = "red",
     pch = 20)

plot(ipc2$DISTANCIA_CAP,
     ipc2$PotCONS_MatrMens_C2,
     xlab = "distância da capital",
     ylab = "pdc matrícula-mensalidade",
     main = "C2",
     col = "brown",
     pch = 20)

plot(ipc2$DISTANCIA_CAP,
     ipc2$PotCONS_MatrMens_DE,
     xlab = "distância da capital",
     ylab = "pdc matrícula-mensalidade",
     main = "DE",
     col = "magenta",
     pch = 20)




###Proporção da despesa de consumo###

Per_PotCONS_MatrMens_TOTAL <- (ipc2$PotCONS_MatrMens_TOTAL/ipc2$PotCONS_DespTotal_TOTAL)
Per_PotCONS_MatrMens_A     <- (ipc2$PotCONS_MatrMens_A/ipc2$PotCONS_DespTotal_A)
Per_PotCONS_MatrMens_B1    <- (ipc2$PotCONS_MatrMens_B1/ipc2$PotCONS_DespTotal_B1)
Per_PotCONS_MatrMens_B2    <- (ipc2$PotCONS_MatrMens_B2/ipc2$PotCONS_DespTotal_B2)
Per_PotCONS_MatrMens_C1    <- (ipc2$PotCONS_MatrMens_C1/ipc2$PotCONS_DespTotal_C1)
Per_PotCONS_MatrMens_C2    <- (ipc2$PotCONS_MatrMens_C2/ipc2$PotCONS_DespTotal_C2)
Per_PotCONS_MatrMens_DE    <- (ipc2$PotCONS_MatrMens_DE/ipc2$PotCONS_DespTotal_DE)



summary(Per_PotCONS_MatrMens_TOTAL)
summary(Per_PotCONS_MatrMens_A)
summary(Per_PotCONS_MatrMens_B1)
summary(Per_PotCONS_MatrMens_B2)
summary(Per_PotCONS_MatrMens_C1)
summary(Per_PotCONS_MatrMens_C2)
summary(Per_PotCONS_MatrMens_DE)

par(mfrow = c(3,3))

plot(ipc2$DISTANCIA_CAP,
     ipc2$Per_PotCONS_MatrMens_TOTAL,
     xlab = "distância da capital",
     ylab = "mensalidade/despesa TOTAL",
     main = "Percentual da despesa com educação e distância da capital TOTAL ",
     col = "black",
     pch = 20)

plot(ipc2$DISTANCIA_CAP,
     ipc2$Per_PotCONS_MatrMens_A,
     xlab = "distância da capital",
     ylab = "mensalidade/despesa A",
     main = "Percentual da despesa com educação e distância da capital A",
     col = "blue",
     pch = 20)

plot(ipc2$DISTANCIA_CAP,
     ipc2$Per_PotCONS_MatrMens_B1,
     xlab = "distância da capital",
     ylab = "mensalidade/despesa B1",
     main = "Percentual da despesa com educação e distância da capital B1",
     col = "green",
     pch = 20)

plot(ipc2$DISTANCIA_CAP,
     ipc2$Per_PotCONS_MatrMens_B2,
     xlab = "distância da capital",
     ylab = "mensalidade/despesa B2",
     main = "Percentual da despesa com educação e distância da capital B2",
     col = "purple",
     pch = 20)

plot(ipc2$DISTANCIA_CAP,
     ipc2$Per_PotCONS_MatrMens_C1,
     xlab = "distância da capital",
     ylab = "mensalidade/despesa C1",
     main = "Percentual da despesa com educação e distância da capital C1",
     col = "red",
     pch = 20)

plot(ipc2$DISTANCIA_CAP,
     ipc2$Per_PotCONS_MatrMens_C2,
     xlab = "distância da capital",
     ylab = "mensalidade/despesa C2",
     main = "Percentual da despesa com educação e distância da capital C2",
     col = "brown",
     pch = 20)

plot(ipc2$DISTANCIA_CAP,
     ipc2$Per_PotCONS_MatrMens_DE,
     xlab = "distância da capital",
     ylab = "mensalidade/despesa DE",
     main = "Percentual da despesa com educação e distância da capital DE",
     col = "magenta",
     pch = 20)


hist()

hist(Per_PotCONS_MatrMens_TOTAL, 
     main="Histograma para a proporção de despesa com estudos", 
     xlab="Proporção", 
     border="blue", 
     col="gray", 
     breaks=5, 
     prob = TRUE)

lines(density(Per_PotCONS_MatrMens_TOTAL),
      col = "red")



summary(Per_PotCONS_MatrMens_TOTAL)


###Regressao com a proporcao de mensalidade/despesa total###

###Cross Validation-melhor polinômio####

grau <- 1:5
cv.error = rep(0,5)

ens_PIBcap <- cbind(
                    log(ipc2$PotCONS_MatrMens_TOTAL),
                    log(ipc2$PIB_PCAP),
                    log(1 + ipc2$DISTANCIA_CAP),
                    log(ipc2$POP_H_18_24 + ipc2$POP_M_18_24 ),
                    log(ipc2$POP_H_25_29 + ipc2$POP_H_25_29))

colnames(ens_PIBcap) <- c( 
                           "matric_desp",
                           "PIB_perCapita",
                           "distancia",
                           "pop_18_24",
                            "pop_25_29")



ens_PIBcap <- as.data.frame(ens_PIBcap)


ens_PIBcap_test <- cbind(log(ipc3$PotCONS_MatrMens_TOTAL),
                         log(ipc3$PIB_PCAP),
                         log(1 + ipc3$DISTANCIA_CAP),
                         log(ipc3$POP_H_18_24),
                         log(ipc3$POP_H_25_29))

ens_PIBcap_test <- as.data.frame(ens_PIBcap_test)

colnames(ens_PIBcap_test) <- c( 
                                "matric_desp",
                                "PIB_perCapita",
                                "distancia",
                                "pop_18_24",
                                "pop_25_29")

## histograma ##

attach(ens_PIBcap)

hist(matric_desp)

lines((matric_desp[matric_desp < 17 &
                          matric_desp > 12 ]),
      col = "red")


for(g in grau) {
  matric.fit = glm(prop_matric_desp~poly(distancia, g),
                 data = ens_PIBcap)
  cv.error[g] = cv.glm(ens_PIBcap,
                       matric.fit,
                       K = 10)$delta[1]         
}                                                                                                                                                               

cv.error

for(g in grau) {
  matric.fit = glm(prop_matric_desp~poly(PIB_perCapita, g),
                   data = ens_PIBcap)
  cv.error[g] = cv.glm(ens_PIBcap,
                       matric.fit,
                       K = 10)$delta[1]         
}

cv.error


###Regressao Com proporcao###

reg.fit <- glm(matric_desp ~ 
               PIB_perCapita +
               distancia +
               pop_18_24 +
               pop_25_29,   
               data = ens_PIBcap)

summary(reg.fit)
coeftest(reg.fit)

hist(reg.fit$residuals, 
     main="residuos", 
     xlab="residuos prop_matric", 
     border="white", 
     col="red", 
     breaks=5, 
     prob = TRUE)

lines(density(reg.fit$residuals),
      col = "black")

plot(reg.fit$fitted.values,
     reg.fit$residuals,
     main = "residuals and fitted",
     xlab = "fitted",
     ylab = "residuals",
     col =   "red")

plot(log(ens_PIBcap$PIB_perCapita),
     (reg.fit$fitted.values),
     main = "fitted",
     xlab = "Pib per capita",
     ylab = "fitted",
     col = "red")



#testes de normalidade#
jb.norm.test(reg.fit$residuals,
             nrepl = 2000)
kurtosis.norm.test(reg.fit$residuals,
                   nrepl = 2000)
skewness.norm.test(reg.fit$residuals
                   , nrepl=2000)
ajb.norm.test(reg.fit$residuals,
               nrepl=2000)



#subconjunto de teste#

pred_test <- predict.glm(reg.fit,
                         data = ens_PIBcap_test)


reg.fit2 <- glm(matric_desp ~ 
                 PIB_perCapita +
                 distancia +
                 pop_18_24 +
                 pop_25_29,   
                 data = ens_PIBcap_test)

summary(reg.fit2)

pred_test2 <- predict.glm(reg.fit2,
                          data = ens_PIBcap)

#testes de normalidade 2#
jb.norm.test(reg.fit2$residuals,
             nrepl = 2000)
kurtosis.norm.test(reg.fit$residuals,
                   nrepl = 2000)
skewness.norm.test(reg.fit$residuals
                   , nrepl=2000)
ajb.norm.test(reg.fit$residuals,
              nrepl=2000)



###comparacao entre modelos ####

diferen <- function(x,y){sum((y - x)^2)}

diferen(reg.fit$residuals,
        pred_test)

diferen(reg.fit$residuals,
        reg.fit$fitted.values)

diferen(reg.fit2$residuals,
        pred_test2)


####separação por estado#########

(unique(ipc1$UF))

AC <- ipc2 %>%
  subset(UF == "AC")

AL <- ipc2 %>%
  subset(UF == "AL")

AM <- ipc2 %>%
  subset(UF == "AM")

AP <- ipc2 %>%
  subset(UF == "AP")

BA <- ipc2 %>%
  subset(UF == "BA")

CE <- ipc2 %>%
  subset(UF == "CE")

DF <- ipc2 %>%
  subset(UF == "DF")

ES <- ipc2 %>%
  subset(UF == "ES")

GO <- ipc2 %>%
  subset(UF == "GO")

MA <- ipc2 %>%
  subset(UF == "MA")

MG <- ipc2 %>%
  subset(UF == "MG")

MS <- ipc2 %>%
  subset(UF == "MS")

MT <- ipc2 %>%
  subset(UF == "MT")

PA <- ipc2 %>%
  subset(UF == "PA")

PB <- ipc2 %>%
  subset(UF == "PB")

PE <- ipc2 %>%
  subset(UF == "PE")

PI <- ipc2 %>%
  subset(UF == "PI")

PR <- ipc2 %>%
  subset(UF == "PR")

RJ <- ipc2 %>%
  subset(UF == "RJ")

RN <- ipc2 %>%
  subset(UF == "RN")

RO <- ipc2 %>%
  subset(UF == "RO")

RR <- ipc2 %>%
  subset(UF == "RR")

RS <- ipc2 %>%
  subset(UF == "RS")

SC <- ipc2 %>%
  subset(UF == "SC")

SE <- ipc2 %>%
  subset(UF == "SE")

SP <- ipc2 %>%
  subset(UF == "SP")

TO <- ipc2 %>%
  subset(UF == "TO")


###cluster com per capita e com a populaçao de 15-29###

#normalização dos dados#

Nor_pop_25_29 <- scale(ipc2$POP_M_25_29 + ipc2$POP_H_25_29)
Nor_PibPerCap <- scale(ipc2$PIB_PCAP)

Ens2 <- cbind(Nor_pop_25_29,
              Nor_PibPerCap)

colnames(Ens2) <- c("populacao",
                    "PibPerCap")

Ens2 <- as.data.frame(Ens2)

###decisao do numero de cluster###


fviz_nbclust(Ens2,
             kmeans,
             method = "wss")

fviz_nbclust(Ens2,
             kmeans,
             method = "silhouette")


###k means###

C_ens2_K  <-  kmeans(Ens2,2)

plotcluster(Ens2,
            C_ens2_K$cluster)

clusplot(Ens2,
         C_ens2_K$cluster,
         color=TRUE,
         shade=TRUE,
         main = "Grupamento da população 25-29 e Pib per capita",
         labels=2,
         lines=0)
 
Ens2 <- cbind(Ens2,
               C_ens2_K$cluster)

Ens2 <- cbind(Ens2,
              ipc2$PIB_PCAP,
              (ipc2$POP_H_25_29 + ipc2$POP_M_25_29))

Ens2 <- Ens2[,-(4:5)]

colnames(Ens2) <- c("populacao_Nor",
                    "pib_cap_Nor",
                    "cluster",
                    "pib_cap",
                    "populacao")

attach(Ens2)  

par(mfrow = c(1,2))

plot(populacao[cluster == 1],
     pib_cap[cluster == 1],
     xlab = "Populacao",
     ylab = "Pib Per capita",
     main = "Grupamento 1",
     col = "red")

plot(populacao[cluster == 2],
     pib_cap[cluster == 2],
     xlab = "Populacao",
     ylab = "Pib Per capita",
     main = "Grupamento 2",
     col = "blue")


####resultados dos grupamento####

length(cluster[cluster == 2])
length(cluster[cluster == 1])


summary(populacao[cluster == 2])
summary(pib_cap[cluster == 2])


summary(populacao[cluster == 1])
summary(pib_cap[cluster == 1])

Ens2 <- cbind(Ens2,
              ipc2$MUNICIPIO,
              ipc2$UF)

colnames(Ens2) <- c("populacao_Nor",
                    "pib_cap_Nor",
                    "cluster",
                    "pib_cap",
                   "populacao",
                    "município",
                    "estado")

Ens2_grp2 <- Ens2 %>%
             subset(cluster == 2)

attach(Ens2)

(município[pib_cap == 334])
(município[pib_cap == 978103])

(município[populacao == 20796.98])



write.xlsx(as.data.frame(Ens2_grp2), 
           file = "C:/Users/Admin/Desktop/Projetos/IPC/Ens2_grp2.xlsx",
           sheetName = "Sheet1",
           col.names = T,
           row.names = F,
           append = F)
