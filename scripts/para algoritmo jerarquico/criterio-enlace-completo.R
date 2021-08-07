# LIBRERIAS A UTILIZAR
library("tidyverse")

library("factoextra")
library("data.table")
library("stats")
library("dendextend")
library("tidyverse")

# EJECUCION DEL ALGORITMO JERARQUICO CON ENLACE-SIMPLE ------

hc.complete <- hclust(dist(matriz_dis_eu), method = "complete")

hc.complete$labels[hc.complete$order]

# ESTABLECER EL PUNTO DE CORTE -------
# asignacion de objetos a grupos (la altura/2 para esta tarea es irrelevante)
grupo_completo <- cutree(hc.complete, k=5)
grupo_completo[hc.complete$labels[hc.complete$order]]

n = 1:200
objetos <- as_tibble(cbind(indice=n, grupo_completo))
objetos <- objetos %>% filter( grupo_completo == 1)
i <- objetos$indice
grupo_1 <- 
  as_tibble(
    cbind(
      id = clientes_limpio$id[i],
      genero = clientes_limpio$genero[i],
      edad = clientes_limpio$edad[i],
      ingreso_anual = clientes_limpio$ingreso_anual[i],
      puntaje = clientes_limpio$puntaje[i]
    )
  )

n = 1:200
objetos <- as_tibble(cbind(indice=n, grupo_completo))
objetos <- objetos %>% filter( grupo_completo == 2)
i <- objetos$indice
grupo_2 <- 
  as_tibble(
    cbind(
      id = clientes_limpio$id[i],
      genero = clientes_limpio$genero[i],
      edad = clientes_limpio$edad[i],
      ingreso_anual = clientes_limpio$ingreso_anual[i],
      puntaje = clientes_limpio$puntaje[i]
    )
  )

n = 1:200
objetos <- as_tibble(cbind(indice=n, grupo_completo))
objetos <- objetos %>% filter( grupo_completo == 3)
i <- objetos$indice
grupo_3 <- 
  as_tibble(
    cbind(
      id = clientes_limpio$id[i],
      genero = clientes_limpio$genero[i],
      edad = clientes_limpio$edad[i],
      ingreso_anual = clientes_limpio$ingreso_anual[i],
      puntaje = clientes_limpio$puntaje[i]
    )
  )

n = 1:200
objetos <- as_tibble(cbind(indice=n, grupo_completo))
objetos <- objetos %>% filter( grupo_completo == 4)
i <- objetos$indice
grupo_4 <- 
  as_tibble(
    cbind(
      id = clientes_limpio$id[i],
      genero = clientes_limpio$genero[i],
      edad = clientes_limpio$edad[i],
      ingreso_anual = clientes_limpio$ingreso_anual[i],
      puntaje = clientes_limpio$puntaje[i]
    )
  )

n = 1:200
objetos <- as_tibble(cbind(indice=n, grupo_completo))
objetos <- objetos %>% filter( grupo_completo == 5)
i <- objetos$indice
grupo_5 <- 
  as_tibble(
    cbind(
      id = clientes_limpio$id[i],
      genero = clientes_limpio$genero[i],
      edad = clientes_limpio$edad[i],
      ingreso_anual = clientes_limpio$ingreso_anual[i],
      puntaje = clientes_limpio$puntaje[i]
    )
  )



# MATRIZ DE PARTICION ----
objeto <- c(1:length(grupo_completo))
matriz_particion_completo <- as_tibble(cbind(objeto, grupo_completo))

matriz_particion_completo <- matriz_particion_completo %>% 
  mutate(
    objeto = as.character(objeto),
    objeto = paste('cliente', objeto)
  )

head(matriz_particion_completo)




# VISUALIZACION DEL GRAFICO
# >>>>>>>>>>>>>>>>>>>>>>>>> Usando cuadros
par(mfrow=c(1,1))
plot(
  hc.complete, 
  main="Segmentacion de clientes, con enlace completo", 
  xlab="Clientes", ylab="Distancia", 
  cex=.6
)
rect.hclust(hc.complete, k=5, border="purple") # punto de corte

# >>>>>>>>>>>>>>>>>>>>>>>>> Usando colores
hc.complete <- color_branches(hc.complete, k = 5)
# Colores de las etiquetas (objetos)
hc.complete <- color_labels(hc.complete, k = 5)
# Tamanio de fuente de etiquetas
hc.complete <- set(hc.complete, "labels_cex", 0.9)
# plot
plot(
  hc.complete, 
  main = 'Segmentacion de clientes, con enlace completo', 
  ylab = 'Distancia', xlab = "Clientes"
)



# MEJOR NUMERO DE GRUPOS
nb_complete <- NbClust(clts[-1], distance = "euclidean", min.nc = 2,
                     max.nc = 10, method = "complete", index ="all")

fviz_nbclust(nb_complete) + theme_minimal()




