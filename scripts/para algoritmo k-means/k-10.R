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
kmedias_10 <- kmeans(clts[-1], centers=10, nstart=25)

# grupo 1: 27 clientes /// grupo 2: 13 clientes /// grupo 3: 12 clientes
# grupo 4: 10 clientes /// grupo 5: 22 clientes /// grupo 6: 30 clientes
# grupo 7: 22 clientes /// grupo 8: 25 clientes /// grupo 9: 29 clientes
# grupo 10: 10 clientes 
kmedias_10$size 


# MATRIZ DE PARTICION --------
i <- order(kmedias_10$cluster)
# matriz de particion
matriz_particion_10 <- as_tibble(cbind(objeto=clts$id[i],
                                      grupo=kmedias_10$cluster[i]))
matriz_particion_10 <- matriz_particion_10 %>% 
  mutate(
    objeto = as.character(objeto),
    objeto = paste('cliente', objeto)
  )

head(matriz_particion_10)


# VISUALIZACION DE DATOS -------
clusplot(clts, kmedias_10$cluster, color=TRUE, shade=TRUE,
         labels=4, lines=0, 
         main= 'Segmentacion de clientes, 10-means.')