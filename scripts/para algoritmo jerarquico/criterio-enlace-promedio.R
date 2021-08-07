# LIBRERIAS A UTILIZAR
library("tidyverse")

library("factoextra")
library("data.table")
library("stats")
library("dendextend")
library("tidyverse")


# EJECUCION DEL ALGORITMO JERARQUICO CON ENLACE-SIMPLE ------

hc.average <- hclust(dist(matriz_dis_eu), method = "average")

hc.average$labels[hc.average$order]

# ESTABLECER EL PUNTO DE CORTE -------
# asignacion de objetos a grupos (la altura/2 para esta tarea es irrelevante)
grupo_promedio <- cutree(hc.average, k=5)
grupo_promedio[hc.average$labels[hc.average$order]]




# MATRIZ DE PARTICION ----
objeto <- c(1:length(grupo_promedio))
matriz_particion_promedio <- as_tibble(cbind(objeto, grupo_promedio))

matriz_particion_promedio <- matriz_particion_promedio %>% 
  mutate(
    objeto = as.character(objeto),
    objeto = paste('cliente', objeto)
  )

head(matriz_particion_promedio)




# VISUALIZACION DEL GRAFICO
# >>>>>>>>>>>>>>>>>>>>>>>>> Usando cuadros
par(mfrow=c(1,1))
plot(
  hc.average, 
  main="Segmentacion de clientes, con enlace promedio", 
  xlab="Clientes", ylab="Distancia", 
  cex=.6
)
rect.hclust(hc.average, k=5, border="green") # punto de corte

# >>>>>>>>>>>>>>>>>>>>>>>>> Usando colores
hc.average <- color_branches(hc.average, k = 5)
# Colores de las etiquetas (objetos)
hc.average <- color_labels(hc.average, k = 5)
# Tamanio de fuente de etiquetas
hc.average <- set(hc.average, "labels_cex", 0.4)
# plot
plot(
  hc.average, 
  main = 'Segmentacion de clientes, con enlace simple', 
  ylab = 'Distancia', xlab = "Clientes"
)



# MEJOR NUMERO DE GRUPOS
nb_promedio <- NbClust(clts[-1], distance = "euclidean", min.nc = 2,
                     max.nc = 10, method = "average", index ="all")

fviz_nbclust(nb_promedio) + theme_minimal()




