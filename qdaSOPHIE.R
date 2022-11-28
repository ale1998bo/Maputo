###qdaSOPHIE
library(car)
library(MASS)
library(class)
load("C:/Users/Alessandro/Desktop/APPLIED/Lab/LAB_5/mcshapiro.test.RData")

dati=read.table('paved_unpaved_coord.txt')
head(dati)
attach(dati)

#elimino zero e NA
zero=which(dati$m1==0)
na=which(is.na(dati$m1))

prova=c(zero,na)

dati1=dati[-prova,]


##divide in the 3 cluster
i1=which(dati$osm_surf=='unpaved')
i2=which(dati$osm_surf=='paved')



street=cbind(dati$m1,dati$m2,dati$m3)
unpaved=street[i1,]
paved=street[i2,]

mcshapiro.test(unpaved)
mcshapiro.test(paved)

n1 <- length(i1)
n2 <- length(i2)


g=2
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

#d3 <- mahalanobis(asphalt, colMeans(asphalt), cov(asphalt))   
#a3=which(d3>4)
#asphaltnorm=asphalt[-a3,]
#mcshapiro.test(asphaltnorm) ##now it's normal
#l3=dim(asphaltnorm)[1]


###PCA
pca.pavednorm=princomp(pavednorm,scores = TRUE)
summary(pca.pavednorm)
pca.pavednorm$loadings
PCA1.paved=pca.pavednorm$scores[,1]
shapiro.test(PCA1.paved)


pca.unpavednorm=princomp(unpavednorm,scores = TRUE)
summary(pca.unpavednorm)
pca.unpavednorm$loadings
PCA1.unpaved=pca.unpavednorm$scores[,1]
shapiro.test(PCA1.unpaved)


### provo a guardafre se i colori sono gauss
R.un=unpaved[,1]
G.un=unpaved[,2]
B.un=unpaved[,3]
RGB.un = (R.un*65536)+(G.un*256)+B.un


R.p=street[,1]
G.p=street[,2]
B.p=street[,3]
RGB.p = (R.p*65536)+(G.p*256)+B.p


shapiro.test(RGB.p)
shapiro.test(RGB.un)


x11()
x1=1:1826
x2=1827:2579
plot(1:2579,log(RGB))
points(x1,log(RGB[i1]),col=2)
points(x2,log(RGB[i2]),col=3)

streetnorm=rbind(asphaltnorm,pavednorm,unpavednorm)
r=rep(c('asphalt','paved','unpaved'),c(l3,l2,l1))
streetnormcomplete=cbind(streetnorm,r)

###create new variable
species.name <- factor(r, labels=c('asphalt','paved','unpaved'))v