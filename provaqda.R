library(car)
library(MASS)
library(class)
load("C:/Users/Alessandro/Desktop/APPLIED/Lab/LAB_5/mcshapiro.test.RData")

dati=DATI100TRAIN[1:100,]
head(dati)
attach(dati)

#elimino zero e NA
zero=which(dati$m1==0)
na=which(is.na(dati$m1))

prova=c(zero,na)

dati1=dati[-prova,]


##divide in the 3 cluster
i1=which(dati1$osm_surf=='unpaved')
i2=which(dati1$osm_surf=='paved')
i3=which(dati1$osm_surf=='asphalt')


street=cbind(dati1$m1,dati1$m2,dati1$m3)
unpaved=unpaved
paved=paved
asphalt=asphalt

n1 <- length(i1)
n2 <- length(i2)
n3 <- length(i3)
n <- n1+n2+n3

g=3 
p=3


##remuve outlighter for each dfferent cluster


d1 <- mahalanobis(unpaved, colMeans(unpaved), cov(unpaved))   
a1=which(d1>4)
unpavednorm=unpaved[-a1,]
mcshapiro.test(unpavednorm)
l1=dim(unpavednorm)[1]

d2 <- mahalanobis(paved, colMeans(paved), cov(paved))   
a2=which(d2>4)
pavednorm=paved[-a2,]
mcshapiro.test(pavednorm)
l2=dim(pavednorm)[1]

d3 <- mahalanobis(asphalt, colMeans(asphalt), cov(asphalt))   
a3=which(d3>4)
asphaltnorm=asphalt[-a3,]
mcshapiro.test(asphaltnorm) ##now it's normal
l3=dim(asphaltnorm)[1]


streetnorm=rbind(asphaltnorm,pavednorm,unpavednorm)
r=rep(c('asphalt','paved','unpaved'),c(l3,l2,l1))
streetnormcomplete=cbind(streetnorm,r)

###create new variable
species.name <- factor(r, labels=c('asphalt','paved','unpaved'))

##ADESSO POSSO FARE QDA CON TUTTI I DATI CHE VOGLIO TRAFORMATI DAL PCA CON I
## LOADINGS CHE HO OTTENUTO 

#DOMANDE A CUI DEVO RISPODERE COME FUNZIONA? BENE O MALE? MAGARI CI POSSO METTERE I DATI SOLO DELLA VEGETATION PER FAR PRIMA
#PER MIGLIORARLO DEVO TROVARE TUTTE LE IMMAGINE 1000 DI TRAINING


qda.street <- qda(streetnorm, species.name)
qda.street

Qda.street <- predict(qda.street, streetnorm)


Qda.street$class
species.name
table(class.true=species.name, class.assigned=Qda.street$class)

errorsq <- (Qda.street$class != species.name)
errorsq

APERq   <- sum(errorsq)/length(species.name)
APERq


## qda with cross validation
QdaCV.street <- qda(streetnorm, species.name, CV=T)
QdaCV.street$class
species.name
table(class.true=species.name, class.assignedCV=QdaCV.street$class)

errorsqCV <- (QdaCV.street$class != species.name)
errorsqCV

AERqCV   <- sum(errorsqCV)/length(species.name)
AERqCV



###PREDICTION WITH GIULIA'S DATASET
###giulia= dati di giulia
Qda.street.giulia <- predict(qda.street, giulia)







### I add length because giulia's anova, the same for typo

street <- a
street2=cbind(a,dati1$Length)
street3=cbind(street2,dati1$osm_typo) ## errore
