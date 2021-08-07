# LIBRERIAS A UTILIZAR
library("tidyverse")

library("factoextra")
library("data.table")
library("stats")
library("dendextend")
library("tidyverse")

# CALCULO DE LA MATRIZ EUCLIDEANA (SOLO UNA VEZ) ----
matriz_dis_eu <- dist(clts[-1], method = "euclidean")


# EJECUCION DEL ALGORITMO JERARQUICO CON ENLACE-SIMPLE ------

hc.single <- hclust(dist(matriz_dis_eu), method = "single")

hc.single$labels[hc.single$order]

# ESTABLECER EL PUNTO DE CORTE -------
# asignacion de objetos a grupos (la altura/2 para esta tarea es irrelevante)
grupo_simple <- cutree(hc.single, k=5)
grupo_simple[hc.single$labels[hc.single$order]]




# MATRIZ DE PARTICION ----
objeto <- c(1:length(grupo_simple))
matriz_particion_simple <- as_tibble(cbind(objeto, grupo_simple))

matriz_particion_simple <- matriz_particion_simple %>% 
  mutate(
    objeto = as.character(objeto),
    objeto = paste('cliente', objeto)
  )

head(matriz_particion_simple)




# VISUALIZACION DEL GRAFICO
# >>>>>>>>>>>>>>>>>>>>>>>>> Usando cuadros
par(mfrow=c(1,1))
plot(
  hc.single, 
  main="Segmentacion de clientes, con enlace simple", 
  xlab="Clientes", ylab="Distancia", 
  cex=.6
)
rect.hclust(hc.single, k=5, border="red") # punto de corte

# >>>>>>>>>>>>>>>>>>>>>>>>> Usando colores
hc.single <- color_branches(hc.single, k = 5)
# Colores de las etiquetas (objetos)
hc.single <- color_labels(hc.single, k = 5)
# Tamanio de fuente de etiquetas
hc.single <- set(hc.single, "labels_cex", 0.4)
# plot
plot(
  hc.single, 
  main = 'Segmentacion de clientes, con enlace simple', 
  ylab = 'Distancia', xlab = "Clientes"
)


# MEJOR NUMERO DE GRUPOS
nb_simple <- NbClust(clts[-1], distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "single", index ="all")

fviz_nbclust(nb_simple) + theme_minimal()


