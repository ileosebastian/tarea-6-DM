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
kmedias_3 <- kmeans(clts[-1], centers=3, nstart=25)

# grupo 1: 123 clientes /// grupo 2: 38 clientes /// grupo 3: 39 clientes
kmedias_3$size 


# MATRIZ DE PARTICION --------
i <- order(kmedias_3$cluster)
# matriz de particion
matriz_particion_3 <- as_tibble(cbind(objeto=clts$id[i],
                                      grupo=kmedias_3$cluster[i]))
matriz_particion_3 <- matriz_particion_3 %>% 
  mutate(
    objeto = as.character(objeto),
    objeto = paste('cliente', objeto)
  )

head(matriz_particion_3)


# VISUALIZACION DE DATOS -------
clusplot(clts, kmedias_3$cluster, color=TRUE, shade=TRUE,
         labels=4, lines=0, 
         main= 'Segmentacion de clientes, 3-means.')


