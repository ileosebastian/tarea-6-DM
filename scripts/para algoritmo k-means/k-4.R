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
kmedias_4 <- kmeans(clts[-1], centers=4, nstart=25)

# grupo 1: 38 clientes /// grupo 2: 39 clientes /// grupo 3: 95 clientes
# grupo 4: 28 clientes 
kmedias_4$size 


# MATRIZ DE PARTICION --------
i <- order(kmedias_4$cluster)
# matriz de particion
matriz_particion_4 <- as_tibble(cbind(objeto=clts$id[i],
                                      grupo=kmedias_4$cluster[i]))
matriz_particion_4 <- matriz_particion_4 %>% 
  mutate(
    objeto = as.character(objeto),
    objeto = paste('cliente', objeto)
  )

head(matriz_particion_4)


# VISUALIZACION DE DATOS -------
clusplot(clts, kmedias_4$cluster, color=TRUE, shade=TRUE,
         labels=4, lines=0, 
         main= 'Segmentacion de clientes, 4-means.')