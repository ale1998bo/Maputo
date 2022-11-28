##PCA FOR AN IMAGE

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
img1_NIR=raster("C:/Users/Alessandro/Desktop/safari/raster_mask/img__1.tif", band=4)


#Create stack
landsat <- stack( img1_G, img1_B,img1_R)  #I can add NIR If the fourth band is NIR
landsatNIR <- stack( img1_G, img1_B,img1_R,img1_NIR)


#PCA
set.seed(1)
sr <- sampleRandom(landsat, 10000)
plot(sr, main = "RGB plot")
pca <- prcomp(sr, scale = TRUE)
pca 
screeplot(pca)
pci <- predict(landsat, pca, index = 1:2)
par(mfrow = c(1,3))
plotRGB(landsat)
##plotRGB(landsatNIR)
plot(pci[[1]])
plot(pci[[2]])

