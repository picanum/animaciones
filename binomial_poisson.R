#### CÓDIGO PARA GENERAR LA ANIMACIÓN QUE COMPARA LAS DENSIDADES DE LAS DISTRIBUCIONES BINOMIAL Y POISSON ####

library(ggplot2)
library(gganimate)

ps <- seq(0.05,0.9,by=0.025)    #Generamos el grid de valores para el parámetro p

#Realizamos las simulaciones de las binomiales con n = 30 y p = p y las Poisson con lambda = 30*p
datos<-data.frame(p=rep(ps,each=10000),
                  media_pois = rep(ps*30, each=10000))
dens <- c()
dens_pois <- c()

#Generamos (y almacenamos) 10000 resultados de una binomial con n = 30 y p = i, donde i recorre todos los valores de ps
for(i in as.numeric(levels(as.factor(datos$p)))) dens <- c(dens, rbinom(10000,30,i)) 
datos$dens <- dens

#Generamos (y almacenamos) 10000 resultados de una Poisson con lambda = i, donde i recorre todos los valores de 30*ps
for(i in as.numeric(levels(as.factor(datos$media_pois)))) dens_pois <- c(dens_pois, rpois(10000,i))
datos$dens_pois <- dens_pois

#Representaremos las funciones de densidad (se podría hacer con los histogramas cambiando geom_density por geom_histogram)
ggplot(datos, aes(x=dens)) + geom_density(alpha=.6) +
  geom_density(aes(x = dens_pois), col = "red", alpha=.6) +
  theme_minimal(base_size = 18) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  labs(title = 'Densidad de probabilidad de una binomial\ncon n = 30 y p = {closest_state} (negro)\nDensidad de probabilidad de una Poisson\ncon lambda = {as.numeric(closest_state)*30} (rojo)',
  y = "Densidad de probabilidad",
  caption="@Picanumeros")+
  #A partir de esta línea empiezan los comandos de gganimate
  transition_states(p
                    , transition_length = 3, state_length = 2
  ) + 
  view_follow() +
ease_aes('sine-in-out') 
