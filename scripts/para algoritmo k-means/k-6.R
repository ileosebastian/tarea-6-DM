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
kmedias_6 <- kmeans(clts[-1], centers=6, nstart=25)

# grupo 1: 45 clientes /// grupo 2: 22 clientes /// grupo 3: 21 clientes
# grupo 4: 35 clientes /// grupo 5: 38 clientes /// grupo 6: 39 clientes
kmedias_6$size 


# MATRIZ DE PARTICION --------
i <- order(kmedias_6$cluster)
# matriz de particion
matriz_particion_6 <- as_tibble(cbind(objeto=clts$id[i],
                                      grupo=kmedias_6$cluster[i]))
matriz_particion_6 <- matriz_particion_6 %>% 
  mutate(
    objeto = as.character(objeto),
    objeto = paste('cliente', objeto)
  )

head(matriz_particion_6)


# VISUALIZACION DE DATOS -------
clusplot(clts, kmedias_6$cluster, color=TRUE, shade=TRUE,
         labels=4, lines=0, 
         main= 'Segmentacion de clientes, 6-means.')