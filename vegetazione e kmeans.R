#VEGETAZION && K_MEAN

library(raster)
library(rgdal)
library(sp)


##setting directory and source file
setwd("C:/Users/Alessandro/Desktop/safari/raster")

img1=raster("C:/Users/Alessandro/Desktop/safari/raster_mask/img__438.tif")


#Divided by band
img1_R=raster("C:/Users/Alessandro/Desktop/safari/raster_mask/img__438.tif", band=1)
img1_G=raster("C:/Users/Alessandro/Desktop/safari/raster_mask/img__438.tif", band=2)
img1_B=raster("C:/Users/Alessandro/Desktop/safari/raster_mask/img__438.tif", band=3)
img1_NIR=raster("C:/Users/Alessandro/Desktop/safari/raster_mask/img__438.tif", band=4)


#Create stack
landsat <- stack( img1_R, img1_G,img1_B)  #I can add NIR If the fourth band is NIR
landsatNIR <- stack( img1_G, img1_B,img1_R,img1_NIR)

r <- getValues(landsat)

## vegetation
vi <- function(img, k, i) {
  bk <- img[[k]]
  bi <- img[[i]]
  vi <- (bk - bi) / (bk + bi) 
  return(vi)
}
ndvi <- vi(landsatNIR, 4, 1)
plot(ndvi, col = rev(terrain.colors(10)), main = "Landsat-NDVI")
veg <- reclassify(ndvi, cbind(-Inf, 0.5, NA))
#plot(ndvi)
#plot(veg, main='Vegetation')



#TOLGO PIXEL VEGETATION
#MEAN FOR ALL THE CHANNEL R,G,B and the respective colour
landsat_df<-as.data.frame(landsat,xy=TRUE)
ndvi_df<-as.data.frame(ndvi)
veg_df<-as.data.frame(veg)
rigaveg<-which(veg_df$layer>0)
riga_noveg<-which(is.na(veg_df$layer) )
landsatnoveg<-landsat_df[riga_noveg,] #tolto la vegetazione
riga_noveg2<-which(landsatnoveg$img__438.1!=0 & landsatnoveg$img__438.2!=0 & landsatnoveg$img__438.3!=0) ## tolgo gli zeri (nero)
landsatnovegok<-landsatnoveg[riga_noveg2,]
attach(landsatnovegok)

## ho posto i pixel vegetazione == 0 come se fossero neri
for (i in rigaveg) {
  landsat_df$img__438.1[i]=0
  landsat_df$img__438.2[i]=0
  landsat_df$img__438.3[i]=0
  
}


a=landsat_df[,c(3,4,5)]
str(a)


#I use algorithm llylod, we can change algorithm  and understand how it works
set.seed(99)
kmncluster <- kmeans(na.omit(a), centers = 3, iter.max = 600, nstart = 5, algorithm=c("Lloyd"))
str(kmncluster)


# Use the ndvi object to set the cluster values to a new raster (maybe it doesn't work for dimentionality)
knr <- setValues(landsat, kmncluster$cluster)

#we can choose the colors
mycolor <- c("#fef65b","#ff0000","#0000ff")# "#daa520")  ##"#0000ff")##,"#00ff00","#cbbeb5", "#c3ff5b", "#ff7373", "#00ff00", "#808080")

#I plot the new data division
par(mfrow = c(1,2))
plotRGB(landsat, col = rev(terrain.colors(10)), main = 'Landsat-landsat')
plot(knr$layer.2, main = 'Unsupervised classification', col = mycolor )

# how data chages
dataprovanorm=as.data.frame(landsat)
dataprovaknr=as.data.frame(knr)

## divided cluster and delete the row with 3 zeros
rowszero <- which( landsat_df$img__438.1==0 & landsat_df$img__438.2==0 &  landsat_df$img__438.3==0)
datalandsat=dataprovanorm[-rowszero,]
knr.1=dataprovaknr[-rowszero,]


## select the rgow of the cluster
row1=which(knr.1[,1]==1)
row2=which(knr.1[,1]==2)
row3=which(knr.1[,1]==3)

## compute mean of cluster
M1=colMeans(datalandsat[row1,])
M2=colMeans(datalandsat[row2,])
M3=colMeans(datalandsat[row3,])

col1=rgb2hex(M1, maxColorValue=255)
col2=rgb2hex(M2, maxColorValue=255)
col3=rgb2hex(M3, maxColorValue=255)

#MISS THE RIGHT PACKAGE

### per sceglieri





