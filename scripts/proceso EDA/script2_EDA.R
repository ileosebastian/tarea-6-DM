# LIBRERIAS A UTILIZAR

library(tidyverse)
library(dplyr)
library(e1071)
library(DescTools)
library(magrittr)
library(ggplot2)


######
#
clientes_limpio %>% names

clientes_limpio <- tibble::as_tibble(clientes_limpio)

# medidas de tendencia central ---- 
# >>>> PROMEDIO
#  ...de genero
prom_genero <- clientes_limpio %$% mean(genero) # NA <---- sin sentido aparente
#  ...de edad
prom_edad <- clientes_limpio %$% mean(edad) # 38.85 <- puede ser que la gente menor a 40 anios
                                # en promedi, entre al CC <---- con algo de sentido
#  ...de ingreso_anual
prom_ingreso_anual <- clientes_limpio %$% mean(ingreso_anual) # 60.56 <- puede ser que la gente que tenga
                                        # tarjetas de membresia gana, en promedio,
                                        # 60560 dolares anuales <---- dato con valor
#  ...de puntaje
prom_puntaje <- clientes_limpio %$% mean(puntaje) # 50.2 <- puede ser que, la gente compra 
                                  # moderamente en el CC, segun el comportamiento
                                  # dado por el puntaje, que de media es 50 puntos
                                  # <---- dato con alto valor
# >>>> MEDIANA
#  ...de genero
med_genero <- clientes_limpio %$% median(genero) # NA <---- sin sentido aparente
#  ...de edad
med_edad <- clientes_limpio %$% median(edad) # 36 <- puede ser que 36 anios represente a la gente
                                # de mediana edad que entre al CC 
# esta medida tambien dice que si se corresponde con lo dicho por el promedio, teniendo
# valores extremos de forma muy atipica
# <---- no tiene mucho sentido
                                # pues este grafico demuestra la poca frecuencia que tiene la 
                                # edad 36 en los datos datos.

plot_edad <- ggplot(clientes_limpio, aes(x=edad))
plot_edad + geom_histogram()
table(clientes_limpio$edad)
#  ...de ingreso_anual
med_ingreso_anual <- clientes_limpio %$% median(ingreso_anual) # 61.5 <- puede ser que la gente que tenga
                                          # tarjetas de membresia gana, de forma representariva,
                                          # una mediana de 61500 dolares anuales 
                                          # <---- dato con valor
# esta medida tambien dice que si se corresponde con lo dicho por el promedio, teniendo
# valores extremos de forma muy atipica
plot_ingreso_anual <- ggplot(clientes_limpio, aes(x=ingreso_anual))
plot_ingreso_anual <-  plot_ingreso_anual + geom_histogram()
table(clientes_limpio$ingreso_anual)
#  ...de puntaje
med_puntaje <- clientes_limpio %$% median(puntaje) # 50 <- puede ser que, la gente compra 
                                  # moderamente en el CC, segun el comportamiento
                                  # dado por el puntaje, que de mediana es 50 puntos
# notar que es practicamente igual al dato del promedio, por lo tanto, tiene
# valores extremos muy atipicos
                                  # <---- dato con alto valor
plot_puntaje  <- ggplot(clientes_limpio, aes(x=puntaje))
plot_puntaje + geom_histogram()
table(clientes_limpio$puntaje)

# >>>> MODA
#  ...de genero
moda_genero <- clientes_limpio %$% Mode(genero) # Femenino, con 122
#  esto quiere decir que los clientes femeninos tienen con mayor frecuencia
# una tarjeta de membresia que las personas de genero masculino
# <---- con sentido, puede tener valor
plot_genero <- ggplot(clientes_limpio, aes(x=id, fill=genero))
plot_genero <- plot_genero + geom_histogram()
plot_genero + 
  labs(title = 'Frecuencia con que aparecen personas de diferentes generos',
       x='Clientes', y='Frecuencia')
table(clientes_limpio$genero)

#  ...de edad
moda_edad <- clientes_limpio %$% Mode(edad) # 32 <- puede ser que 32 nos diga que, con frecuencia,
# las personas con 32 anios tienen una tarjeta de membresia
table(clientes_limpio$edad)
#  ...de ingreso_anual
moda_ingreso_anual <- clientes_limpio %$% Mode(ingreso_anual) # 54 y 78 <- este dataset es multimodal, esto quiere
# decir que hay dos ingresos que se repiten con la misma frecuencia. Entonces, podemos deicr 
# que, con frecuencia, las personas que ganan 54 mil y 78 mil al anio, tienen una tarjeta
# de membresia
table(clientes_limpio$ingreso_anual)
#  ...de puntaje
moda_puntaje <- clientes_limpio %$% Mode(puntaje) # 42, con 8 <- puede ser que, la gente frecuenta un
# comportamiento que ha llevado a decir que las personan fecuentan tener 42 puntos en 
# la tarjeta de membresia
table(clientes_limpio$puntaje)

# CONCLUSION: Con lo anterior dicho, se puede aseverar que muchos valores se conentran
# bastante en la poblacion central del dataset, no en su mayoria, pero si corresponde a
# ser un poco signiticativo

# medidas de dispersion ---- 
# >>>> VARIANZA
# ...de genero
var_genero <- var(clientes_limpio$genero, na.rm = TRUE)
var_genero
# ...de edad
var_edad <- var(clientes_limpio$edad, na.rm = TRUE)
var_edad
# ...de ingreso_anual
var_ingreso_anual <- var(clientes_limpio$ingreso_anual, na.rm = TRUE)
var_ingreso_anual
# ...de puntaje
var_puntaje <- var(clientes_limpio$puntaje, na.rm = TRUE)
var_puntaje
# >>>> DESVIACION ESTANDAR
# ...de genero
sd_genero <- sd(clientes_limpio$genero, na.rm = TRUE)
sd_genero
# ...de edad
sd_edad <- sd(clientes_limpio$edad, na.rm = TRUE)
sd_edad
# ...de ingreso_anual
sd_ingreso_anual <- sd(clientes_limpio$ingreso_anual, na.rm = TRUE)
sd_ingreso_anual
# ...de puntaje
sd_puntaje <- sd(clientes_limpio$puntaje, na.rm = TRUE)
sd_puntaje
# >>>> MINIMO Y MAXIMO
# ...de genero
min_max_genero <- range(clientes_limpio$genero, na.rm = TRUE)
min_max_genero
# ...de edad
min_max_edad <- range(clientes_limpio$edad, na.rm = TRUE)
min_max_edad
# ...de ingreso_anual
min_max_ingreso_anual <- range(clientes_limpio$ingreso_anual, na.rm = TRUE)
min_max_ingreso_anual
# ...de puntaje
min_max_puntaje <- range(clientes_limpio$puntaje, na.rm = TRUE)
min_max_puntaje
# >>>> RANGO
# ...de genero
ran_genero <- diff(range(clientes_limpio$genero, na.rm = TRUE))
ran_genero
# ...de edad
ran_edad <- diff(range(clientes_limpio$edad, na.rm = TRUE))
ran_edad
# ...de ingreso_anual
ran_ingreso_anual <- diff(range(clientes_limpio$ingreso_anual, na.rm = TRUE))
ran_ingreso_anual
# ...de puntaje
ran_puntaje <- diff(range(clientes_limpio$puntaje, na.rm = TRUE))
ran_puntaje

# CONCLUSION:
# Como se pude observar, a partir de los datos, la dispersion de los datos es moderada,
# pues no es muy alta, pero tampoco es muy baja, con respecto a valor minimo y 
# maximo de cada atributo.
# *listar cada analisis de cada atributo
# Ademas, el rango es amplio para cada uno de los atributos numericos

# ¿cuántas personas ganan más de 100 dólares?
clientes_limpio %>% 
  filter(
    ingreso_anual > 100
  ) %>% 
  summarise(
    No_Pers_ganan_mas_de_100 = n() # 14 personas
  )
table(clientes_limpio$ingreso_anual) # <- segun esto, es correcto

# ¿cuál es el rango en dólares que gana la mayoría de las personas?
# como hay 200 personas, con mas de 100 estaremos viendo a la 'mayoria de personas'
clientes_limpio %>% 
  filter(
    id < 181
  ) %>% 
  summarise(
    rango_minimo = as.character( min(range(.$ingreso_anual)) ),
    rango_minimo = paste0('$ ',rango_minimo,'k'),
    
    rango_maximo = as.character( max(range(.$ingreso_anual)) ),
    rango_maximo = paste0('$ ',rango_maximo,'k'),
    
    rango_de_ganancia = as.character( diff(range(.$ingreso_anual)) ),
    rango_de_ganancia = paste0('$ ',rango_de_ganancia,'k'),
  )

# ¿de cuánto dólares es el ingreso mínimo?
min_max_ingreso_anual # de 18 mil dolares anuales

# ¿qué rango de edad tienen los clientes más habituales del centro comercial?
# Como se se cuales son clientes habituales y cuales no?
clientes_limpio %>% 
  filter(
    id < 181
  ) %>% 
  summarise(
    rango_minimo = as.character( min(range(.$edad)) ),
    rango_minimo = paste0(rango_minimo,' años'),
    
    rango_maximo = as.character( max(range(.$edad)) ),
    rango_maximo = paste0(rango_maximo,' años'),
    
    rango_de_edad = as.character( diff(range(.$edad)) ),
    rango_de_edad = paste0(rango_de_edad,' años'),
  )

# ¿cuál es el rango de spending score de la mayoría de los clientes?
clientes_limpio %>% 
  filter(
    id < 181
  ) %>% 
  summarise(
    rango_minimo = as.character( min(range(.$puntaje)) ),
    rango_minimo = paste0(rango_minimo,' puntos'),
    
    rango_maximo = as.character( max(range(.$puntaje)) ),
    rango_maximo = paste0(rango_maximo,' puntos'),
    
    rango_de_puntaje = as.character( diff(range(.$puntaje)) ),
    rango_de_puntaje = paste0(rango_de_puntaje,' puntos'),
  )


# MIS PREGUNTAS INTERESANTES ------
# 1. ¿de cuánto dólares es el ingreso maximo?
min_max_ingreso_anual # de 137 mil dolares anuales

# 2. ¿que clientes ganan en promedio, masculinos o femeninos?
clientes_limpio %>% 
  filter(
    genero == 'Femenino'
  ) %$%
  mean(
    .$ingreso_anual
  )
clientes_limpio %>% 
  filter(
    genero == 'Masculino'
  ) %$%
  mean(
    .$ingreso_anual
  )


# 3. ¿cuantos son los clientes que estan en el rango modal de ingreso anual?
moda_menor <- min(moda_ingreso_anual)
moda_menor
moda_mayor <- max(moda_ingreso_anual)
moda_mayor
clientes_limpio %>% 
  filter(
    ingreso_anual >= moda_menor
  ) %>% 
  filter(
    ingreso_anual <= moda_mayor
  ) %>% 
  summarise(
    clientes_del_rango_modal = n() # hay 86 persoans dentro de este rango
  )






