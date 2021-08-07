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
kmedias_7 <- kmeans(clts[-1], centers=7, nstart=25)

# grupo 1: 22 clientes /// grupo 2: 29 clientes /// grupo 3: 10 clientes
# grupo 4: 21 clientes /// grupo 5: 35 clientes /// grupo 6: 38 clientes
# grupo 7: 45 clientes 
kmedias_7$size


# MATRIZ DE PARTICION --------
i <- order(kmedias_7$cluster)
# matriz de particion
matriz_particion_7 <- as_tibble(cbind(objeto=clts$id[i],
                                      grupo=kmedias_7$cluster[i]))
matriz_particion_7 <- matriz_particion_7 %>% 
  mutate(
    objeto = as.character(objeto),
    objeto = paste('cliente', objeto)
  )

head(matriz_particion_7)


# VISUALIZACION DE DATOS -------
clusplot(clts, kmedias_7$cluster, color=TRUE, shade=TRUE,
         labels=4, lines=0, 
         main= 'Segmentacion de clientes, 7-means.')