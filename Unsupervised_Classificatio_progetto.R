##UNSUPERVIDED CLASSIFICATION FOR AN IMAGE

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


###unsupervised classification  from RS###
r <- getValues(landsat)
str(r)

#I use algorithm llylod, we can change algorithm  and understand how it works
set.seed(99)
kmncluster <- kmeans(na.omit(r), centers = 4, iter.max = 600, nstart = 5, algorithm=c("Lloyd"))
str(kmncluster)

# Use the ndvi object to set the cluster values to a new raster
knr <- setValues(landsat, kmncluster$cluster)

#we can choose the colors
mycolor <- c("#fef65b","#ff0000", "#daa520","#0000ff")  ##"#0000ff")##,"#00ff00","#cbbeb5", "#c3ff5b", "#ff7373", "#00ff00", "#808080")

#I plot the new data division
par(mfrow = c(1,2))
plotRGB(landsat, col = rev(terrain.colors(10)), main = 'Landsat-landsat')
plot(knr$layer.2, main = 'Unsupervised classification', col = mycolor )

# how data chages
dataprovanorm=as.data.frame(landsat)
dataprovaknr=as.data.frame(knr)
