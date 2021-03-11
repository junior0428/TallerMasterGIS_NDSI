#install.packages("sf")
library(sf)
library(raster)
library(rgeos)
library(rgdal)
#direccionar la carpeta
setwd("C:/MASTERGIS/TallerMasterGIS_NDSI")
getwd()
#Cargar las imagenes satelitales 
img1987<-stack(list.files("INPUT/IMAGENES SATELITALES/1987Jul12Jul20Landsat5TM", pattern = ".tif", full.names = TRUE))
img1997<-stack("INPUT/IMAGENES SATELITALES/1997Jun26Jul04Landsat5TM/Landsat 5 TM Collection 1 Tier 1 8-Day TOA Reflectance Composite.tif")
img2004<-stack("INPUT/IMAGENES SATELITALES/2004May08May16Landsat5TM/Landsat 5 TM Collection 1 Tier 1 8-Day TOA Reflectance Composite.tif")
img2007<-stack("INPUT/IMAGENES SATELITALES/2007Jul20Jul2007Landsat5TM/Landsat 5 TM Collection 1 Tier 1 8-Day TOA Reflectance Composite.tif")
img2016<-stack("INPUT/IMAGENES SATELITALES/2016Jun09Jun17Landsat8OLI/Landsat 8 Collection 1 Tier 1 8-Day TOA Reflectance Composite.tif")
img2020<-stack("INPUT/IMAGENES SATELITALES/2020Jul27Aug04Landsat8OLI/Landsat 8 Collection 1 Tier 1 8-Day TOA Reflectance Composite.tif")
plot(img1987[[2]])
#caragr los shps 
inv2018<-readOGR("INPUT/SHAPEFILE/InventarioGlaciar2018.shp")
inv21989<-readOGR("INPUT/SHAPEFILE/InventarioGlaciar1989.shp")
are<-readOGR("INPUT/SHAPEFILE/Area.shp")
plot(inv2018)
#extraer las badas para el calculo del NDSI
GREE<-c(img1987[[2]], img1997[[2]], img2004[[2]], img2007[[2]], img2016[[3]], img2020[[3]])
SWIR<-c(img1987[[5]], img1997[[5]], img2004[[5]], img2007[[5]], img2016[[6]], img2020[[6]])
plot(GREE[[2]])
# calculo del NDSI
rsult<-NULL
for (i in 1:6) {
  NDSI<-(GREE[[i]]-SWIR[[i]])/(GREE[[i]]+SWIR[[i]])
  rsult<-c(rsult, NDSI)
}
plot(rsult[[5]])
#visualizacion de los resultados 
rancol<-colorRampPalette(c("blue", "white"))
plot(rsult[[6]], col=rancol(50))
#reclasificacion del NDSI
rsult_rclas<-NULL
for (j in 1:6) {
  rclas<-reclassify(rsult[[j]], matrix(c(-1, 0.4, 1, 0.4, 1, 2), ncol = 3, byrow = TRUE))
  rsult_rclas<-c(rsult_rclas, rclas)
}

plot(rsult_rclas[[6]], col=rancol(2))
#cortar con el area de trabajo 
plot(are)
rsult_cor<-NULL
for (k in 1:6) {
  cor<-mask(rsult_rclas[[k]], are)
  rsult_cor<-c(rsult_cor, cor)
}
plot(rsult_cor[[1]], col=rancol(2))
#convertir raster a vector 
vect1<-rasterToPolygons(rsult_cor[[1]], dissolve = TRUE) %>% st_as_sf()
vect2<-rasterToPolygons(rsult_cor[[2]], dissolve = TRUE) %>% st_as_sf()
vect3<-rasterToPolygons(rsult_cor[[3]], dissolve = TRUE) %>% st_as_sf()
vect4<-rasterToPolygons(rsult_cor[[4]], dissolve = TRUE) %>% st_as_sf()
vect5<-rasterToPolygons(rsult_cor[[5]], dissolve = TRUE) %>% st_as_sf()
vect6<-rasterToPolygons(rsult_cor[[6]], dissolve = TRUE) %>% st_as_sf()

plot(vect4)
#exportar los resultados

st_write(vect1, "OUTPUT/Año1987.shp", overwrite=TRUE)
st_write(vect2, "OUTPUT/Año1997.shp", overwrite=TRUE)
st_write(vect3, "OUTPUT/Año2004.shp", overwrite=TRUE)
st_write(vect4, "OUTPUT/Año2007.shp", overwrite=TRUE)
st_write(vect5, "OUTPUT/Año2016.shp", overwrite=TRUE)
st_write(vect6, "OUTPUT/Año2020.shp", overwrite=TRUE)

writeRaster(rsult_rclas[[1]], "OUTPUT/Año1987.tif")




