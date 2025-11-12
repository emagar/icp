rm(list=ls())
setwd("~/Desktop/icp/parcial2/data")
raw <- read.csv("base-datos-encuesta-refelec-itam-incluye-nrs.csv")
colnames(raw)
##
## make numeric
raw <- within(raw, {
    folio    <- as.numeric(folio)
    dia      <- as.numeric(dia)
    sexo     <- as.numeric(sexo)
    edad     <- as.numeric(edad)
    cu       <- as.numeric(cu)
    p02      <- as.numeric(p02)
    p03a     <- as.numeric(p03a)
    p03b     <- as.numeric(p03b)
    p03c     <- as.numeric(p03c)
    p03d     <- as.numeric(p03d)
    p03e     <- as.numeric(p03e)
    p03f     <- as.numeric(p03f)
    p03g     <- as.numeric(p03g)
    p03h     <- as.numeric(p03h)
    p04a     <- as.numeric(p04a)
    p04b     <- as.numeric(p04b)
    p04c     <- as.numeric(p04c)
    p05      <- as.numeric(p05)
    p06      <- as.numeric(p06)
    p07      <- as.numeric(p07)
    p08      <- as.numeric(p08)
    p09a     <- as.numeric(p09a)
    p09b     <- as.numeric(p09b)
    p09c     <- as.numeric(p09c)
    p09d     <- as.numeric(p09d)
    p09e     <- as.numeric(p09e)
    p10a     <- as.numeric(p10a)
    p10b     <- as.numeric(p10b)
    p10c     <- as.numeric(p10c)
    p10d     <- as.numeric(p10d)
    p10e     <- as.numeric(p10e)
    p10f     <- as.numeric(p10f)
    p11      <- as.numeric(p11)
    p12      <- as.numeric(p12)
    p13      <- as.numeric(p13)
    p14      <- as.numeric(p14)
    p15      <- as.numeric(p15)
    p16      <- as.numeric(p16)
    p17      <- as.numeric(p17)
    p18      <- as.numeric(p18)
    p19      <- as.numeric(p19)
    p20      <- as.numeric(p20)
    p21      <- as.numeric(p21)
    p22      <- as.numeric(p22)
    p23      <- as.numeric(p23)
    p24      <- as.numeric(p24)
    p25      <- as.numeric(p25)
    p26      <- as.numeric(p26)
    p27      <- as.numeric(p27)
    p28      <- as.numeric(p28)
    hora_fin <- as.numeric(hora_fin)
    min_fin  <- as.numeric(min_fin)
    duracion <- as.numeric(duracion)
    prog     <- as.numeric(prog)
    int      <- as.numeric(int)
    })
##
## quita cuestionarios extraviados
raw <- raw[-which(raw$status=="sustituido"),]
##
## regress edad on cu to predict missing ages
cu <- c(222470, 224164, 224502, 222717, 214041, 214844, 218098, 225582, 207705, 225191, 201735, 213108, 213746, 201958, 207729, 208446, 197753, 200354, 202468, 188497, 196717, 197232, 201033, 202145, 202499, 198202, 197776, 198395, 168552)
edad <- c(17, 18, 18, 19, 20, 20, 20, 20, 21, 21, 21, 21, 21, 22, 22, 22, 23, 23, 23, 23, 23, 23, 23, 23, 23, 24, 24, 24, 30)
m <- lm(edad ~ cu)
summary(m)

##pdf("../reporte/plots/reg.pdf")
plot(edad ~ cu, main = "Modelo para imputar edades", xlab = "clave única")
abline(reg=m, lty=2)
points(x=raw$cu, y=rep(17.5, nrow(raw)), pch=20)
##dev.off()

## predict edades de quienes no respondieron
raw$edad[is.na(raw$cu)==FALSE] <- predict(m, newdata = data.frame(cu=raw$cu[is.na(raw$cu)==FALSE]))
## round edad
raw$edad <- round(raw$edad, 1)
## 
## edades pablo vs resto
mean(raw$edad[raw$encuestador=="Pablo"], na.rm=TRUE)
mean(raw$edad[raw$encuestador!="Pablo"], na.rm=TRUE)
mean(raw$edad[raw$encuestador=="Vania"], na.rm=TRUE)
mean(raw$edad[raw$encuestador!="Vania"], na.rm=TRUE)



## tasa de respuesta
      table(contestó=raw$dresp)
round(table(contestó=raw$dresp)/nrow(raw), 2)
round(table(raw$status)/nrow(raw), 2)
      table(raw$equipo, raw$status)

## Edades
table(contestó=raw$dresp, edad=raw$edad, useNA = "ifany")

##pdf("../reporte/plots/edad.pdf")
par(mfrow=c(2,1))
par(mar=c(0,5,3,3))
hist(raw$edad[raw$dresp==1] , xlim=c(17,33) , ylab="Sí contestó", xlab=""    , ylim=c(0,22) , xaxt="n", las=1 , col="darkgreen", breaks=10 , main="Edades según respuesta")
par(mar=c(5,5,0,3))
hist(raw$edad[raw$dresp==0] , xlim=c(17,33) , ylab="No contestó", xlab="Edad", ylim=c(22,0)           , las=1 , col="tomato3"  , breaks=10 , main="" )
##dev.off()

## Algunos no sabes
raw$p13[is.na(raw$p13) & raw$dresp==1] <- 5

## Re-escala calificaciones para que ni bien ni mal esté en medio y corran de mal a bien
library(plyr)
raw <- within(raw, {
    p02  <- mapvalues(p02  , from = c(1,2,3,4)   , to = c(4,3,2,1))
    p03a <- mapvalues(p03a , from = c(1,2,3,4,5) , to = c(5,4,2,1,3))
    p03b <- mapvalues(p03b , from = c(1,2,3,4,5) , to = c(5,4,2,1,3))
    p03c <- mapvalues(p03c , from = c(1,2,3,4,5) , to = c(5,4,2,1,3))
    p03d <- mapvalues(p03d , from = c(1,2,3,4,5) , to = c(5,4,2,1,3))
    p03e <- mapvalues(p03e , from = c(1,2,3,4,5) , to = c(5,4,2,1,3))
    p03f <- mapvalues(p03f , from = c(1,2,3,4,5) , to = c(5,4,2,1,3))
    p03g <- mapvalues(p03g , from = c(1,2,3,4,5) , to = c(5,4,2,1,3))
    p03h <- mapvalues(p03h , from = c(1,2,3,4,5) , to = c(5,4,2,1,3))
    p04a <- mapvalues(p04a , from = c(1,2,3,4,5) , to = c(5,4,2,1,3))
    p04b <- mapvalues(p04b , from = c(1,2,3,4,5) , to = c(5,4,2,1,3))
    p04c <- mapvalues(p04c , from = c(1,2,3,4,5) , to = c(5,4,2,1,3))
    p05  <- mapvalues(p05  , from = c(1,2,3,4,5) , to = c(5,4,2,1,3))
    p09a <- mapvalues(p09a , from = c(1,2,3,4,5) , to = c(5,4,2,1,3))
    p09b <- mapvalues(p09b , from = c(1,2,3,4,5) , to = c(5,4,2,1,3))
    p09c <- mapvalues(p09c , from = c(1,2,3,4,5) , to = c(5,4,2,1,3))
    p09d <- mapvalues(p09d , from = c(1,2,3,4,5) , to = c(5,4,2,1,3))
    p09e <- mapvalues(p09e , from = c(1,2,3,4,5) , to = c(5,4,2,1,3))
    p10a <- mapvalues(p10a , from = c(1,2,3,4)   , to = c(4,3,2,1))
    p10b <- mapvalues(p10b , from = c(1,2,3,4)   , to = c(4,3,2,1))
    p10c <- mapvalues(p10c , from = c(1,2,3,4)   , to = c(4,3,2,1))
    p10d <- mapvalues(p10d , from = c(1,2,3,4)   , to = c(4,3,2,1))
    p10e <- mapvalues(p10e , from = c(1,2,3,4)   , to = c(4,3,2,1))
    p10f <- mapvalues(p10f , from = c(1,2,3,4)   , to = c(4,3,2,1))
    p11  <- mapvalues(p11  , from = c(1,2,3,4,5) , to = c(5,4,2,1,3))
    p12  <- mapvalues(p12  , from = c(1,2,3,4)   , to = c(4,3,2,1))
    p13  <- mapvalues(p13  , from = c(1,2,3,4,5) , to = c(4,3,2,1,5)) ## conserva No sabe
    p22  <- mapvalues(p22  , from = c(1,2,3,4,5,6,7,8,9,10,11) , to = c(1,2,3,4,5,6,7,8,9,10,NA))
    p23  <- mapvalues(p23  , from = c(1,2,3,4,5,6,7,8,9,10,11) , to = c(1,2,3,4,5,6,7,8,9,10,NA))
    p28  <- mapvalues(p28  , from = c(1,2,3,4)   , to = c(4,3,2,1))
    })
##
## data with factors
fac <- raw
fac$prog <- factor(raw$prog , levels = 1:4, labels = c("Cpol","+Eco","+Der","+RI"))
## aprobación CSP
fac$p02  <- factor(raw$p02  , levels = 1:4, labels = c("Desapr. mucho","Desapr. algo","Aprueba algo","Aprueba mucho"))
## calificación asuntos
fac$p03a <- factor(raw$p03a , levels = 1:5, labels = c("Muy mal","Mal","Ni bien ni mal","Bien","Muy bien"))
fac$p03b <- factor(raw$p03b , levels = 1:5, labels = c("Muy mal","Mal","Ni bien ni mal","Bien","Muy bien"))
fac$p03c <- factor(raw$p03c , levels = 1:5, labels = c("Muy mal","Mal","Ni bien ni mal","Bien","Muy bien"))
fac$p03d <- factor(raw$p03d , levels = 1:5, labels = c("Muy mal","Mal","Ni bien ni mal","Bien","Muy bien"))
fac$p03e <- factor(raw$p03e , levels = 1:5, labels = c("Muy mal","Mal","Ni bien ni mal","Bien","Muy bien"))
fac$p03f <- factor(raw$p03f , levels = 1:5, labels = c("Muy mal","Mal","Ni bien ni mal","Bien","Muy bien"))
fac$p03g <- factor(raw$p03g , levels = 1:5, labels = c("Muy mal","Mal","Ni bien ni mal","Bien","Muy bien"))
fac$p03h <- factor(raw$p03h , levels = 1:5, labels = c("Muy mal","Mal","Ni bien ni mal","Bien","Muy bien"))
fac$p04a <- factor(raw$p04a , levels = 1:5, labels = c("Muy mal","Mal","Ni bien ni mal","Bien","Muy bien"))
fac$p04b <- factor(raw$p04b , levels = 1:5, labels = c("Muy mal","Mal","Ni bien ni mal","Bien","Muy bien"))
fac$p04c <- factor(raw$p04c , levels = 1:5, labels = c("Muy mal","Mal","Ni bien ni mal","Bien","Muy bien"))
fac$p05  <- factor(raw$p05  , levels = 1:5, labels = c("Muy mal","Mal","Ni bien ni mal","Bien","Muy bien"))
fac$p06  <- factor(raw$p06  , levels = 1:2, labels = c("De acuerdo","Desacuerdo"))
fac$p07  <- factor(raw$p07  , levels = 1:2, labels = c("Colaborar EEUU","Defender soberanía"))
fac$p08  <- factor(raw$p08  , levels = 1:2, labels = c("Aprueba","Desaprueba"))
fac$p09a <- factor(raw$p09a , levels = 1:5, labels = c("Muy mal","Mal","Ni bien ni mal","Bien","Muy bien"))
fac$p09b <- factor(raw$p09b , levels = 1:5, labels = c("Muy mal","Mal","Ni bien ni mal","Bien","Muy bien"))
fac$p09c <- factor(raw$p09c , levels = 1:5, labels = c("Muy mal","Mal","Ni bien ni mal","Bien","Muy bien"))
fac$p09d <- factor(raw$p09d , levels = 1:5, labels = c("Muy mal","Mal","Ni bien ni mal","Bien","Muy bien"))
fac$p09e <- factor(raw$p09e , levels = 1:5, labels = c("Muy mal","Mal","Ni bien ni mal","Bien","Muy bien"))
fac$p10a <- factor(raw$p10a , levels = 1:4, labels = c("Nada","Poca","Algo","Mucha"))
fac$p10b <- factor(raw$p10b , levels = 1:4, labels = c("Nada","Poca","Algo","Mucha"))
fac$p10c <- factor(raw$p10c , levels = 1:4, labels = c("Nada","Poca","Algo","Mucha"))
fac$p10d <- factor(raw$p10d , levels = 1:4, labels = c("Nada","Poca","Algo","Mucha"))
fac$p10e <- factor(raw$p10e , levels = 1:4, labels = c("Nada","Poca","Algo","Mucha"))
fac$p10f <- factor(raw$p10f , levels = 1:4, labels = c("Nada","Poca","Algo","Mucha"))
fac$p11  <- factor(raw$p11  , levels = 1:5, labels = c("Muy mal","Mal","Ni bien ni mal","Bien","Muy bien"))
fac$p12  <- factor(raw$p12  , levels = 1:4, labels = c("Nada","Poca","Algo","Mucho"))
fac$p13  <- factor(raw$p13  , levels = 1:5, labels = c("Nada","Poco","Algo","Muy importante","No sabe"))
fac$p14  <- factor(raw$p14  , levels = 1:3, labels = c("Elecciones libres","Reducir costos","No sabe"))
fac$p15  <- factor(raw$p15  , levels = 1:3, labels = c("Autonomía INE","Gobierno organice","No sabe"))
fac$p16  <- factor(raw$p16  , levels = 1:2, labels = c("Al partido mayorit.","Proporcional"))
fac$p17  <- factor(raw$p17  , levels = 1:3, labels = c("Mayoritario","Proporcional","Mixto"))
fac$p18  <- factor(raw$p18  , levels = 1:2, labels = c("CSP","Con oposición y ciudadanía"))
fac$p19  <- factor(raw$p19  , levels = 1:2, labels = c("INE imparcial","Partidocracia y corrupción"))
fac$p20  <- factor(raw$p20  , levels = 1:7, labels = c("PAN","PRI","PVEM","PT","MC","Morena","Indep."))
fac$p21  <- factor(raw$p21  , levels = c(1:7,9), labels = c("Muy priista","Algo priista","Muy panista","Algo panista","Muy morenista","Algo morenista","Otro","Ninguno"))
fac$p24  <- factor(raw$p24  , levels = 1:8, labels = c("Ning","Prim","Sec","Prepa","Técnica","Univ","Lic","Posgrado"))
fac$p25  <- factor(raw$p25  , levels = 1:5, labels = c("Alta","Media alta","Media","Media baja","Baja"))
fac$p26  <- factor(raw$p26  , levels = 1:4, labels = c("Yo","Familiar","Ambos","No"))
fac$p27  <- factor(raw$p27  , levels = 1:3, labels = c("Sí, envían","Sí, no envían", "No"))
fac$p28  <- factor(raw$p28  , levels = 1:4, labels = c("Nada feliz","Poco feliz","Algo feliz","Muy feliz"))

## Programas
table(fac$prog, useNA = "ifany")

##pdf("../reporte/plots/prog.pdf")
par(mfrow=c(2,1))
par(mar=c(3,5,3,3))
plot(fac$prog[raw$dresp==1] , ylim=c(0,50)  , ylab="Sí contestó"            , las=1 , col="darkgreen" , main = "Programa según respuesta")
par(mar=c(3,5,1,3))
plot(fac$prog[raw$dresp==0] , ylim=c(50,0)  , ylab="No contestó" , xaxt="n" , las=1 , col="tomato3"   , main = "")
##dev.off()

## Eval dicotómicas
raw$dapruebaCSP <- as.numeric(raw$p02  > 2)
raw$dapruebaEco <- as.numeric(raw$p03a > 2)
raw$dapruebaSeg <- as.numeric(raw$p03b > 2)
raw$dapruebaCor <- as.numeric(raw$p03c > 2)
raw$dapruebaApo <- as.numeric(raw$p03d > 2)
raw$dapruebaEdu <- as.numeric(raw$p03e > 2)
raw$dapruebaSal <- as.numeric(raw$p03f > 2)
raw$dapruebaCri <- as.numeric(raw$p03g > 2)
raw$dapruebaTru <- as.numeric(raw$p03h > 2)
raw$dhonestidad <- as.numeric(raw$p04a > 2)
raw$dliderazgo  <- as.numeric(raw$p04b > 2)
raw$dresultados <- as.numeric(raw$p04c > 2)
raw$dUSA        <- as.numeric(raw$p05  > 2)
raw$dmarcelo    <- as.numeric(raw$p09a > 2)
raw$dharfuch    <- as.numeric(raw$p09b > 2)
raw$dnorona     <- as.numeric(raw$p09c > 2)
raw$dadan       <- as.numeric(raw$p09d > 2)
raw$dpina       <- as.numeric(raw$p09e > 2)
raw$dconfINE    <- as.numeric(raw$p10a > 2)
raw$dconfTEPJF  <- as.numeric(raw$p10b > 2)
raw$dconfSCJN   <- as.numeric(raw$p10c > 2)
raw$dconfTrib   <- as.numeric(raw$p10d > 2)
raw$dconfLeg    <- as.numeric(raw$p10e > 2)
raw$dconfEjec   <- as.numeric(raw$p10f > 2)
raw$dnuevaSCJN  <- as.numeric(raw$p11  > 2)
raw$dinteres    <- as.numeric(raw$p12  > 2)
raw$dreforma    <- as.numeric(raw$p13  > 2)
raw$dmaj        <- as.numeric(raw$p16 == 1)

## partidismo simple
fac$part <- mapvalues(raw$p21 , from = 1:9 , to = c(1,1,2,2,3,3,4,NA,5))
fac$part <- factor(fac$part , levels = 1:5 , labels = c("PRI","PAN","Morena","Otro","Ninguno"))

## Drop NAs
fac <- fac[raw$dresp==1,]
raw <- raw[raw$dresp==1,]

table(raw$dapruebaCSP)
mean(raw$dapruebaCSP)

## importancia
table(fac$p13, useNA = "ifany")
## nacional en Moreno
tmpNal <- c(rep(1,7), rep(2,20), rep(3,32), rep(4,39), rep(5,2))
tmpNal  <- factor(tmpNal  , levels = 1:5, labels = c("Nada","Poco","Algo","Muy importante","No sabe"))
##pdf("../reporte/plots/import.pdf")
par(mfrow=c(2,1))
par(mar=c(3,5,3,3))
plot(fac$p13[raw$dresp==1] , ylim=c(0,45)  , ylab="ITAM"            , las=1 , col="darkgreen" , main = "Importancia")
par(mar=c(3,5,1,3))
plot(tmpNal                , ylim=c(45,0)  , ylab="Nacional" , xaxt="n" , las=1 , col="tomato3"   , main = "")
##dev.off()

## objetivos
table(fac$p14, useNA = "ifany")
table(fac$p15, useNA = "ifany")

## imagen INE
table(fac$p19, useNA = "ifany")

## MR o RP
table(fac$p16, useNA = "ifany")
table(fac$p17, useNA = "ifany")


## intención de voto
round(table(fac$p20, useNA = "ifany") / nrow(fac), 2)
## partidismo
round(table(fac$part, useNA = "ifany") / nrow(fac), 2)

table(partidismo=fac$part , voto=fac$p20 , useNA = "ifany")



## izq-der
round(table(raw$p22, useNA = "ifany") / nrow(fac), 2)
mean(raw$p22)
##pdf("../reporte/plots/self.pdf")
plot(as.factor(raw$p22), main = "Auto-ubicación izq-der")
##dev.off()

## felicidad
round(table(fac$p28, useNA = "ifany") / nrow(fac), 2)
mean(raw$p28)
##pdf("../reporte/plots/self.pdf")
plot(as.factor(raw$p22), main = "Auto-ubicación izq-der")
##dev.off()

## pjud
round(table(fac$p11, useNA = "ifany") / nrow(fac), 2)
## norma piña
round(table(fac$p09e, useNA = "ifany") / nrow(fac), 2)

## confianza ine
plot(fac$p10a, main = "Confianza INE")
plot(fac$p10b, main = "Confianza TEPJF")
plot(fac$p10c, main = "Confianza SCJN")
plot(fac$p10d, main = "Confianza legislativo")
plot(fac$p10e, main = "Confianza ejecutivo")


tapply(raw$dreforma, raw$dapruebaCSP, mean, na.rm = TRUE)
tapply(raw$dreforma, raw$dapruebaCSP, mean, na.rm = TRUE)
tapply(raw$dreforma, fac$part, mean, na.rm = TRUE)
tapply(raw$dconfINE, fac$part, mean, na.rm = TRUE)
by(raw$dreforma, raw$dapruebaCSP, mean, na.rm = TRUE)

plot(fac$p02  , main = "Aprobación CSP")
plot(fac$p03a , main = "CSP economía")
plot(fac$p03b , main = "CSP seguridad")
plot(fac$p03c , main = "CSP corrupción")
plot(fac$p03d , main = "CSP apoyos")
plot(fac$p03e , main = "CSP educación")
plot(fac$p03f , main = "CSP salud")
plot(fac$p03g , main = "CSP crimen org")
plot(fac$p03h , main = "CSP Trump")
plot(fac$p04a , main = "Honestidad CSP")
plot(fac$p04b , main = "Liderazgo CSP")
plot(fac$p04c , main = "Resultados CSP")

plot(fac$part)

table(fac$p27  , useNA = "ifany")

