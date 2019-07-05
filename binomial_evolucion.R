#### CÓDIGO PARA HACER LA ANIMACIÓN DE LA EVOLUCIÓN DE LA VARIABILIDAD DE UNA BINOMIAL SEGÚN EL VALOR DE p #####

library(ggplot2)
library(gganimate)
library(reshape2)
library(magick)

#Vector para definir el número de etapas en la animación, se corresponde con la granularidad de p (puntos de p que recorremos entre 0 y 1)
p = seq(0, 1, by = 1/10)
#Parámetro n de la binomial (idealmente debería ser el denominador de la longitud del paso en el vector anterior)
n = 10

#Generamos el conjunto de datos con la masa de probabilidad de una binomial
#(cada columna se corresponderá con la función masa de probabilidad de una binomial de parámetro n y uno de los gránulos de p)
datos <- sapply(p, function(p, n) dbinom(0:n, n, p), n = 10)
colnames(datos) <- p
#Reestructuramos los datos para adaptarlos al tidyverse
datos <- melt(datos)
datos$k <- rep(0:10, 11)

#Primera animación: evolución de la función masa de probabilidad
p1 <- ggplot(datos, aes(x=k, y=value)) + geom_bar(stat = "identity") +  #Hacemos el diagrama de barras
  theme_minimal(base_size = 18) + 
  scale_x_continuous(breaks = 0:10) +
  labs(title = 'Función masa de probabilidad de una\nbinomial con n = 10 y p = {closest_state}',
  y = "P(X = k)",
  caption="@Picanumeros")+
  ylim(c(0,1))+
  #A partir de aquí empieza el código de gganimate
  transition_states(Var2
                    , transition_length = 1, state_length = 2
  ) +
  ease_aes('sine-in-out')

#Segunda animación: evolución de la varianza
#Hacemos un data.frame cuya primera columna sea la p y la segunda la varianza correspondiente de una binomial con esa p (y la n fijada)
datos2 <- data.frame(p = p, Varianza = n*p*(1-p))

p2 <- ggplot(datos2, aes(x = p, y = Varianza)) + geom_point(size=3) +   #Hacemos un gráfico de puntos y líneas
  geom_line() + theme_minimal(base_size = 18) + 
  labs(title = "Evolución de la varianza de una binomial\ncon n = 10",
  y = "Varianza", x = "Parámetro p de la binomial",
  caption="@Picanumeros")+
  #A partir de aquí empieza el código de gganimate
  transition_reveal(p)+   #Usamos transition_reveal para que la línea de puntos se rellene de forma acumulativa (en lugar de llevarse el punto a otro lugar)
  ease_aes('sine-in-out')

#¿Cómo juntar ambas animaciones en una sola? Usamos el paquete magick; las transformamos en objetos de image_read()
a_mgif <- image_read(animate(p1, height = 480, width = 480))
b_mgif <- image_read(animate(p2, height = 480, width = 480))

#Y después los juntamos y combinamos todos los frames
new_gif <- image_append(c(a_mgif[1], b_mgif[1]))
for(i in 2:100){
  combined <- image_append(c(a_mgif[i], b_mgif[i]))
  new_gif <- c(new_gif, combined)
}

new_gif #Listo!
