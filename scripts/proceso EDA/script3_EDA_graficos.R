# LIBRERIAS A UTILIZAR

library(tidyverse)
library(dplyr)
library(e1071)
library(DescTools)
library(magrittr)
library(ggplot2)

# GRAFICAS CON RESPECTO A ...

# ... GENERO ------
plot_genero <- clientes_limpio %>% 
  ggplot(
    data = .,
    aes(
      x = genero,
    )
  )
plot_genero <- plot_genero + 
  geom_bar(
    position = 'dodge',
  ) + 
  labs(
    title = '1', y = 'Frecuencia'
  )
plot_genero

plot_genero <- clientes_limpio %>% # con edad
  ggplot(
    data = .,
    aes(
      x = edad,
      fill = genero
    )
  )
plot_genero <- plot_genero + 
  geom_histogram(
    position = 'dodge',
    bins = 15
  ) + 
  labs(
    title = '2', y = 'Frecuencia'
  )
plot_genero <- plot_genero + facet_grid(~genero)
plot_genero

plot_genero <- clientes_limpio %>% # con ingreso anual
  ggplot(
    data = .,
    aes(
      x = ingreso_anual,
      fill = genero
    )
  )
plot_genero <- plot_genero + 
  geom_histogram(
    position = 'dodge',
    bins = 15
  ) + 
  labs(
    title = '3', y = 'Frecuencia'
  )
plot_genero <- plot_genero + facet_grid(~genero)
plot_genero


plot_genero <- clientes_limpio %>% # con puntaje
  ggplot(
    data = .,
    aes(
      x = puntaje,
      fill = genero
    )
  )
plot_genero <- plot_genero + 
  geom_histogram(
    position = 'dodge',
    bins = 15
  ) + 
  labs(
    title = '4', y = 'Frecuencia'
  )
plot_genero <- plot_genero + facet_grid(~genero) 
plot_genero



# ... EDAD ------
plot_ingreso_anual <- clientes_limpio %>% 
  ggplot(
    data = .,
    aes(
      x = edad,
      y = puntaje
    )
  )
plot_ingreso_anual  <- plot_ingreso_anual  + 
  geom_point(
    alpha = 0.5,
    aes( color= genero )
  ) +
  geom_smooth(method=lm) + 
  labs(
    title = '5'
  )
plot_ingreso_anual 


# ... INGRESO ANUAL ------
plot_ingreso_anual <- clientes_limpio %>% # la linea de tendencia no es nada clara
  ggplot(
    data = .,
    aes(
      x = ingreso_anual,
      y = edad
    )
  )
plot_ingreso_anual  <- plot_ingreso_anual  + 
  geom_point(
    alpha = 0.5,
    aes( color= genero )
  ) +
  geom_smooth(method=lm) + 
  labs(
    title = '6'
  )
# plot_ingreso_anual  <- plot_ingreso_anual  + facet_grid(~genero)
plot_ingreso_anual 


plot_ingreso_anual <- clientes_limpio %>% # la agrupacion es curiosa
  ggplot(
    data = .,
    aes(
      x = ingreso_anual,
      y = puntaje
    )
  )
plot_ingreso_anual  <- plot_ingreso_anual  + 
  geom_point(
    alpha = 0.5,
    aes( color= genero )
  ) +
  geom_smooth(method=lm) + 
  labs(
    title = '7', x = 'ingreso en dolares al año'
  )
plot_ingreso_anual 


plot_ingreso_anual <- clientes_limpio %>% # la linea de tendencia es curiosa
  ggplot(
    data = .,
    aes(
      x = ingreso_anual,
      y = id
    )
  )
plot_ingreso_anual  <- plot_ingreso_anual  + 
  geom_point(
    alpha = 0.5,
  ) +
  geom_smooth(method=lm) + 
  labs(
    title = 'Grafico curioso 1', x = 'ingreso en dolares al año', y='personas'
  )
plot_ingreso_anual 



# ... PUNTAJE ------
plot_puntaje <- clientes_limpio %>% # la dispersion es curiosa
  ggplot(
    data = .,
    aes(
      x = puntaje,
      y = id
    )
  )
plot_puntaje <- plot_puntaje  + 
  geom_point(
    alpha = 0.5,
    aes( color= genero )
  ) +
  geom_smooth(method=lm) + 
  labs(
    title = 'Grafico curioso 2', y = 'personas'
  )
plot_puntaje
