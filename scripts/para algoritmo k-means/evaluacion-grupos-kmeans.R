# LIBRERIAS A UTILIZAR

library(tidyverse)

library("cluster")
library("NbClust")

library("factoextra")
library("data.table")
library("stats") 

# METODO ELBOW ------
set.seed(2001)
k.minimo <- 2
k.maximo <- 10
wss <- sapply(k.minimo:k.maximo,
              function(k){kmeans(clts[-1], k, nstart=25)$tot.withinss})
plot(k.minimo:k.maximo, wss,
     type="b", pch = 19, frame = FALSE,
     xlab="Número de grupos k",
     ylab="Suma total error cuadrático dentro de los grupos")
# dibuja linea vertical
abline(v = 5, lty =2) # posiblemente, el codo que vemos esta en 5 grupos


# METODO SILUETA ------
silueta <- rep(0, k.maximo)
# Calcula la silueta entre k min y k max
set.seed(2001)
for(i in k.minimo:k.maximo){
  km.res <- kmeans(clts[-1], centers = i, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(clts[-1], method = "euclidean"))
  silueta[i] <- mean(ss[, 3])
}
# Plot el promedio del índice silueta
plot(1:k.maximo, silueta, type = "b", pch = 19,
     frame = FALSE, xlab = "Número de grupos k")
# Dibuja linea vertical
abline(v = which.max(silueta), lty = 2) # esta en 6 grupos


# COMPARACION DE TODOS LOS METODOS -> SELECCION DEL MEJOR GRUPO -----
set.seed(2001)
nb_kmeans <- NbClust(clts[,-1], distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "kmeans", index ="all")

fviz_nbclust(nb_kmeans) + theme_minimal()


