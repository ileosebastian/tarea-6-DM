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
kmedias_5 <- kmeans(clts[-1], centers=5, nstart=25)

# grupo 1: 79 clientes /// grupo 2: 23 clientes /// grupo 3: 39 clientes
# grupo 4: 23 clientes /// grupo 5: 26 clientes 
kmedias_5$size 


# MATRIZ DE PARTICION --------
i <- order(kmedias_5$cluster)
# matriz de particion
matriz_particion_5 <- as_tibble(cbind(objeto=clts$id[i],
                                      grupo=kmedias_5$cluster[i]))
matriz_particion_5 <- matriz_particion_5 %>% 
  mutate(
    objeto = as.character(objeto),
    objeto = paste('cliente', objeto)
  )

head(matriz_particion_5)


# VISUALIZACION DE DATOS -------
clusplot(clts, kmedias_5$cluster, color=TRUE, shade=TRUE,
         labels=4, lines=0, 
         main= 'Segmentacion de clientes, 5-means.')