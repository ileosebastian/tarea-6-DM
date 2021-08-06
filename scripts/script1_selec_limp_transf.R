# LIBRERIAS A UTILIZAR

library(tidyverse)

#####
# SELECCION
clientes <- read.csv('datos/mall_clientes.csv')

glimpse(clientes)

##### 
# LIMPIEZA
clientes_limpio <- clientes %>% 
  rename(
    id = CustomerID,
    genero = Gender,
    edad = Age,
    ingreso_anual = Annual.Income..k..,
    puntaje = Spending.Score..1.100.
  )

clientes_limpio <- clientes_limpio %>% 
  mutate(
    genero = ifelse(genero == 'Female', 'Femenino', 'Masculino')
  )


head(clientes_limpio)

# ver si hay o no datos faltantes:
apply(clientes_limpio,2,function(x) sum(is.na(x)))


#####
# TRANSFORMACION

# puede ser lo siguientes?
clts <- clientes_limpio %>% 
  mutate(
    genero = ifelse(genero == 'Femenino', 1, 2)
  )

head(clts)

# OJO: 
# ----> 'clts' son el dataset a usar para la parte de mineria de datos
# ----> 'clientes_limpio' son el dataset a usar para EDA
