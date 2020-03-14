library(ggplot2)
library(gganimate)

#Generamos el data.frame con un total de 10.000 puntos, cuyas coordenadas X e Y se distribuyen de manera uniforme en el intervalo [-1,1]
dat <- data.frame(x = runif(10000, -1, 1), y = runif(10000, -1, 1))

#Añadimos una variable binaria que toma el valor 1 si esta dentro del circulo x^2 + y^2 <= 1, y 0 en otro caso
dat$g <- ifelse(abs(dat$x^2 + dat$y^2 <= 1), 1, 0)

#Añadimos otra variable que calcule el numero de puntos que estan dentro del circulo hasta la i-esima iteracion
dat$cumg <- cumsum(dat$g)

#Añadimos variables que indiquen la iteracion. Seran utiles para el gganimate.
dat$t <- 1:10000
#En concreto, esta variable hara que, en lugar de aparecer los puntos uno a uno, aparezcan de cien en cien, aligerando la animacion.
dat$t2 <- cut(dat$t, breaks = seq(0, 10000, by = 100), labels = 1:100)

#Hacemos un nuevo vector que nos dice cual es el valor de la suma de g dividida entre el num. de puntos en las filas multiplos de 100
#(cogemos solo las filas multiplos de 100 para aligerar la animacion)
pi4 <- dat$cumg[seq(100, 10000, by = 100)]/seq(100, 10000, by = 100)

g <- ggplot(dat, aes(x = x, y = y, col = factor(g))) + #Estas primeras ordenes son las habituales de cualquier ggplot...
      geom_point() + 
      scale_color_manual(values = c("blue","red")) + 
      theme_bw(base_size = 16) + 
      theme(legend.position = "none") + 
      labs(title = "Suma del número de puntos dentro del\ncírculo [x^2 + y^2 <= 1] dividido entre el total\nde puntos en [-1, 1], multiplicado por 4 = {round(pi4[as.integer(closest_state)]*4, 3)}", 
      caption = "@Picanumeros") + 
      transition_states(t2) +  #Como vemos, añadimos la variable t2 como la indicadora de las etapas de la animacion
      shadow_mark()            #Despues, con shadow_mark hacemos que los puntos que ya han caido no desaparezcan
      
animate(g, nframes = 200, height = 480, width = 480)
