# LIBRERIAS A UTILIZAR

library(tidyverse)

library("cluster")
library("NbClust")

library("factoextra")
library("data.table")
library("stats") 

glimpse(clts)
glimpse(clientes_limpio)

set.seed(2001) # para la reproduccion exacta de este codigo

# ALGORITMO A EJECUTAR -------
kmedias_2 <- kmeans(clts[-1], centers=2, nstart=25)

kmedias_2$size # grupo 1: 115 clientes /// grupo 2: 85 clientes

# MATRIZ DE PARTICION --------
i <- order(kmedias_2$cluster)
# matriz de particion
matriz_particion_2 <- as_tibble(cbind(objeto=clts$id[i],
                                    grupo=kmedias_2$cluster[i]))
matriz_particion_2 <- matriz_particion_2 %>% 
  mutate(
    objeto = as.character(objeto),
    objeto = paste('cliente', objeto)
  )

head(matriz_particion_2)

# VISUALIZACION DE DATOS -------
clusplot(clts, kmedias_2$cluster, color=TRUE, shade=TRUE,
         labels=4, lines=0, 
         main= 'Segmentacion de clientes, 2-means.')












