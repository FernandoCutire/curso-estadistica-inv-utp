
library(sp) 

library(RSelenium) 

library(robustbase) 

library(Rcpp) 

library(spData)

library(Matrix) 

library(spatialreg) 

library(maptools) 

library(GWmodel) 

data("EWHP") 

data("DubVoter") 

#estandarizar 

#básico 

Data.scaled <- scale(as.matrix(Dub.voter@data[, 4:11])) 

pca.basic <- princomp(Data.scaled, cor = FALSE) 

str(pca.basic) 

pca.basic$loadings 

(pca.basic$sdev^2 / sum(pca.basic$sdev^2)) * 100 

PVT=36.08+25.59+11.92 

PVT 

pca.basic$loadings 

#A partir de las tablas de cargas, el componente uno  

#parecería representar 

#a los residentes mayores (Edad45_64) en el PCA básico 

 

#Robusto 

R.COV <- covMcd(Data.scaled, cor = FALSE, alpha = 0.75) 

pca.robust <- princomp(Data.scaled, covmat = R.COV, cor = FALSE) 

pca.robust$sdev^2 / sum(pca.robust$sdev^2)*100 

PVTr=42.76+32.18+11.97 

PVTr 

pca.robust$loadings 

#A partir de las tablas de cargas, el componente uno  

#parecería representar a los  

#residentes adinerados (SC1) en el PCA robusto 

 

#Estas son estadísticas e interpretaciones de todo el mapa que  

#representan un promedio de todo Dublín. Sin embargo,  

#es posible que no representen la estructura social local de  

#manera particularmente confiable. 

 

#Antes de pasar el GWPCA, veamos como cargar un shapefile 

#Se descarga un shapefile 

getwd() 

download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="C:/Users/justd/Documents/world_shape_file.zip") 

 

#Instalar el paquete rgdal 

library(rgdal) 

my_spdf <- readOGR(  

  dsn= paste0("C:/Users/justd/Documents/world_shape_file"),  

  layer="TM_WORLD_BORDERS_SIMPL-0.3", 

  verbose=FALSE 

) 

 

#Plot it with base R 

par(mar=c(0,0,0,0)) 

plot(my_spdf, col="#f2f2f2", bg="skyblue", lwd=0.25, border=0 ) 

 

#Kernel bandwidths for GW PCA 

#Básico 

Coords <- as.matrix(cbind(Dub.voter$X, Dub.voter$Y)) 

Data.scaled.spdf <- SpatialPointsDataFrame(Coords, 

                                              + as.data.frame(Data.scaled)) 

bw.gwpca.basic <- bw.gwpca(Data.scaled.spdf, vars = colnames( 

  + Data.scaled.spdf@data), k = 3, robust = FALSE, adaptive = TRUE) 

bw.gwpca.basic 

 

#Robusto 

bw.gwpca.robust <- bw.gwpca(Data.scaled.spdf, vars = colnames( 

  + Data.scaled.spdf@data), k = 3, robust = TRUE, adaptive = TRUE) 

bw.gwpca.robust 

#muestran anchos de banda óptimos (muy similares) 

 

#GWPCA 

#Observe que ahora especificamos todos los componentes k = 8,  

#pero enfocaremos nuestras investigaciones solo en los primeros 

#tres componentes. Esta especificación asegura que la  

#variación contabilizada localmente por cada componente se  

#estima correctamente. 

 

#básico 

gwpca.basic <- gwpca(Data.scaled.spdf, vars = colnames(Data.scaled.spdf@data), bw = bw.gwpca.basic, k = 8, robust = FALSE, adaptive = TRUE) 

 

#robusto 

gwpca.robust <- gwpca(Data.scaled.spdf, vars = colnames(Data.scaled.spdf@data), bw = bw.gwpca.robust, k = 8, robust = TRUE, adaptive = TRUE) 

 

#ahora puede visualizarse e interpretarse, centrándose en:  

#(1) cómo varía espacialmente la dimensionalidad de los datos  

#y (2) cómo las variables originales influyen en los  

#componentes. 

 

#Empecemos con la varianza (PTV) 

prop.var <- function(gwpca.obj, n.components) {return((rowSums(gwpca.obj$var[, 1:n.components]) / rowSums(gwpca.obj$var)) * 100)} 

library(RColorBrewer) 

mypalette.4 <- brewer.pal(8, "YlGnBu") 

#básico 

var.gwpca.basic <- prop.var(gwpca.basic, 3) 

Dub.voter$var.gwpca.basic <- var.gwpca.basic 

#robusto 

var.gwpca.robust <- prop.var(gwpca.robust, 3) 

Dub.voter$var.gwpca.robust <- var.gwpca.robust 

 

#Mapas 

#crear el mapa 

map.na = list("SpatialPolygonsRescale", layout.north.arrow(),offset = c(329000, 261500), scale = 4000, col = 1) 

map.scale.1 = list("SpatialPolygonsRescale", layout.scale.bar(), offset = c(326500, 217000), scale = 5000, col = 1, fill = c("transparent", "blue")) 

map.scale.2 = list("sp.text", c(326500, 217900), "0", cex = 0.9, col = 1) 

map.scale.3 = list("sp.text", c(331500, 217900), "5km", cex = 0.9, col = 1) 

map.layout <- list(map.na, map.scale.1, map.scale.2, map.scale.3) 

 

#colores 

library(RColorBrewer) 

mypalette.4 <- brewer.pal(8, "YlGnBu") 

 

#Básico 

spplot(Dub.voter, "var.gwpca.basic", key.space = "right", col.regions = mypalette.4, cuts = 7, sp.layout = map.layout, main = "PTV for local components 1 to 3 (basic GW PCA)") 

#Robusto 

spplot(Dub.voter, "var.gwpca.robust", key.space = "right", col.regions = mypalette.4, cuts = 7, sp.layout = map.layout, main = "PTV for local components 1 to 3 (robust GW PCA)") 

 

#Existe una clara variación geográfica en los datos de PTV  

#y generalmente se considera una PTV más alta en el caso  

#local que en el caso global. Los patrones espaciales en  

#ambos mapas son ampliamente similares, con porcentajes más  

#altos ubicados en el sur, mientras que porcentajes más bajos  

#se encuentran en el norte. 

 

#Variable ganadora 

#A continuación, podemos visualizar cómo cada una de las ocho  

#variables influye localmente en un componente dado, mapeando 

#la "variable ganadora" con la carga absoluta más alta. 

 

#Colores 

mypalette.5 <- c("lightpink", "blue", "grey", "purple", "orange", "green", "brown", "yellow") 

#Básico 

loadings.pc1.basic <- gwpca.basic$loadings[, , 1] 

win.item.basic = max.col(abs(loadings.pc1.basic)) 

Dub.voter$win.item.basic <- win.item.basic 

spplot(Dub.voter, "win.item.basic", key.space = "right", col.regions = mypalette.5, at = c(1, 2, 3, 4, 5, 6, 7, 8, 9), main = "Winning variable: highest abs. loading on local Comp.1 (basic)", colorkey = FALSE, sp.layout = map.layout) 

#Map legends are: DiffAdd - light pink;  

#LARent - blue; SC1 - grey;  

#Unempl -purple; LowEduc - orange;  

#Age18_24 - green; Age25_44 - brown; and Age45_64 - yellow. 

 

#El bajo nivel educativo (Low_Educ) domina en los DE del  

#norte y suroeste, mientras que la vivienda pública (LARent)  

#domina en los DE del centro de Dublín. 

 

 

#Robusto 

loadings.pc1.robust <- gwpca.robust$loadings[, , 1] 

win.item.robust = max.col(abs(loadings.pc1.robust)) 

Dub.voter$win.item.robust <- win.item.robust 

spplot(Dub.voter, "win.item.robust", key.space = "right", col.regions = mypalette.5, at = c(1, 2, 3, 4, 5, 6, 7, 8, 9), main = "Winning variable: highest abs. loading on local Comp.1 (robust)", colorkey = FALSE, sp.layout = map.layout) 

 

#La correspondiente "variable ganadora" de la PCA es Age45_64,  

#que claramente no es dominante en todo Dublín, pero sí en su 

#gran parte 