##ATTACCO TABELLA PIXEL A TABELLA ATTRIBUTI
library(dplyr)
medie2<-medie5002[,c(2,3,4,5)]
for(i in 0:499){
  medie2[i+1,5]<-paste0("img_",i)
}
colnames(medie2) = c("m1","m2","m3","colore","image")

#estraggo train
riga3<-which(!is.na(medie2$image))
mediegiusto<-medie2[riga3,]

#estraggo train
riga3<-which(TabellaAttributi$osm_surf!='unk')
tabellaatt_train<-TabellaAttributi[riga3,]


TAB<-left_join(TabellaAttributi,mediegiusto,by='image')
TABTRAIN<-left_join(tabellaatt_train,mediegiusto,by='image')


ri<-which(!is.na(TABTRAIN$m1))
train<-TABTRAIN[ri,]
tabellaatt_noimg_train<-tabellaattributi_noimg[riga3,]



ri2<-which(!is.na(TAB$m1) & TAB$osm_surf=='unk')
test<-TAB[ri2,]
#seleziono solo le righe con lunghezza,tipo e medie
test2<-test[,c(2,3,7,8,9)]
train2<-train[,c(2,3,7,8,9)]
trainlabel<-train[,4]


righepaved<-which(train$osm_surf=="asphalt"| train$osm_surf=="paved" )
Datapaved<-train[righepaved,]

DataUNpaved<-train[-righepaved,]

dim(Datapaved)
dim(DataUNpaved)

M1P=mean(Datapaved$m1)
M2P=mean(Datapaved$m2)
M3P=mean(Datapaved$m3)

xP=c(M1P,M2P,M3P)

M1UNP=mean(DataUNpaved$m1)
M2UNP=mean(DataUNpaved$m2)
M3UNP=mean(DataUNpaved$m3)

xUNP=c(M1UNP,M2UNP,M3UNP)



coloreP=rgb2hex(xP, maxColorValue=255)
coloreUNP=rgb2hex(xUNP, maxColorValue=255)


### solo asphalt

righeasfalt<-which(train$osm_surf=="asphalt")
Dataasfalt<-train[righeasfalt,]



M1A=mean(Dataasfalt$m1)
M2A=mean(Dataasfalt$m2)
M3A=mean(Dataasfalt$m3)

xA=c(M1A,M2A,M3A)

coloreA=rgb2hex(xA, maxColorValue=255)


##solo paved


righeP<-which(train$osm_surf=="paved")
DataP<-train[righeP,]



M1Pa=mean(DataP$m1)
M2Pa=mean(DataP$m2)
M3Pa=mean(DataP$m3)

xPa=c(M1Pa,M2Pa,M3Pa)

colorePa=rgb2hex(xPa, maxColorValue=255)

# Note: we don't need to verify the Gaussian assumption!
# Warning: we are going to use an asymptotic test, but we only have n = 30 data!


x.mean<-test2[1,c(3,4,5)]
mu0   <- c(M1A,M2A,M3A)

x.T2A   <- n * (x.mean-mu0) %*%  x.invcov  %*% (x.mean-mu0)
cfr.chisq <- qchisq(1-alpha,p)
x.T2A < cfr.chisq # no statistical evidence to reject H0 at level alpha

# Compute the p-value
PA <- 1-pchisq(x.T2A, p)
PAlibrary(mvnormtest)






