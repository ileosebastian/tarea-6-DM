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
kmedias_8 <- kmeans(clts[-1], centers=8, nstart=25)

# grupo 1: 21 clientes /// grupo 2: 22 clientes /// grupo 3: 45 clientes
# grupo 4: 10 clientes /// grupo 5: 37 clientes /// grupo 6: 10 clientes
# grupo 7: 29 clientes /// grupo 8: 26 clientes 
kmedias_8$size 


# MATRIZ DE PARTICION --------
i <- order(kmedias_8$cluster)
# matriz de particion
matriz_particion_8 <- as_tibble(cbind(objeto=clts$id[i],
                                      grupo=kmedias_8$cluster[i]))
matriz_particion_8 <- matriz_particion_8 %>% 
  mutate(
    objeto = as.character(objeto),
    objeto = paste('cliente', objeto)
  )

head(matriz_particion_8)


# VISUALIZACION DE DATOS -------
clusplot(clts, kmedias_8$cluster, color=TRUE, shade=TRUE,
         labels=4, lines=0, 
         main= 'Segmentacion de clientes, 8-means.')