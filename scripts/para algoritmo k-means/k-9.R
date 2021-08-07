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
kmedias_9 <- kmeans(clts[-1], centers=9, nstart=25)

# grupo 1: 25 clientes /// grupo 2: 29 clientes /// grupo 3: 10 clientes
# grupo 4: 36 clientes /// grupo 5: 12 clientes /// grupo 6: 44 clientes
# grupo 7: 22 clientes /// grupo 8: 10 clientes /// grupo 9: 12 clientes
kmedias_9$size 


# MATRIZ DE PARTICION --------
i <- order(kmedias_9$cluster)
# matriz de particion
matriz_particion_9 <- as_tibble(cbind(objeto=clts$id[i],
                                      grupo=kmedias_9$cluster[i]))
matriz_particion_9 <- matriz_particion_9 %>% 
  mutate(
    objeto = as.character(objeto),
    objeto = paste('cliente', objeto)
  )

head(matriz_particion_9)


# VISUALIZACION DE DATOS -------
clusplot(clts, kmedias_9$cluster, color=TRUE, shade=TRUE,
         labels=4, lines=0, 
         main= 'Segmentacion de clientes, 9-means.')