library(ggplot2)
library(gganimate)
library(reshape2)
library(viridis)

#Función sencilla para calcular la x-ésima raíz de x#
raiz <- function(x) x^(1/x)

#Función para calcular x^(1/x)^(x^(1/x)^... con tantos exponentes como se desee (especificándolo en el argumento num)
elevador <- function(x, num){
  vec <- raiz(x)
  for(i in 1:num) vec[i+1] <- raiz(x)^vec[i] 
  return(vec)
}

#Designamos las x (entre 1/e y e) que vamos a analizar
granulos <- seq(0.4,2.7, by = 0.1)

#Aplicamos la función "elevador" para cada x y hasta 50 exponentes
matriz <- as.data.frame(sapply(granulos, elevador, num = 50))
names(matriz) <- granulos
matriz <- melt(matriz)  #La reconstruimos para poder usarla en ggplot
matriz$exponente <- rep(0:50, length(granulos))

#ggplot donde el eje x representa el número de exponentes al que va elevado x^(1/x) e y representa el valor resultante de la operación
#Graficamos cada una de las distintas x en líneas separadas
ggplot(matriz,aes(x = exponente, y = value, col = variable)) + 
  geom_line(size = 1.1) + 
  scale_color_viridis_d(option="magma", begin = 0, end = 0.8) +             #Le damos color con el paquete viridis
  labs(title = "x^(1/x)^(x^(1/x)^(x^(1/x)^... = x, 1/e < x < e",            #Damos nombres a los ejes
       x = "Número de exponentes", y = "Valor", caption = "@Picanumeros") + 
  theme_minimal(base_size = 16) +                                           #Usaremos un tema con bajo data-ink ratio
  geom_text(aes(x = 50.6, label = variable), hjust = 0) +                   #Con este comando logramos añadir la x al final de cada línea
  transition_reveal(along = exponente) +                                    #Comando de gganimate para que las líneas se "descubran" poco a poco
  theme(legend.position = "none", panel.grid.minor.y = element_blank()) +   #Comando theme usado para quitar la leyenda y las líneas menores en Y
  scale_y_continuous(breaks = seq(0,2.7, by = 0.1))                         #Comando para designar las líneas mayores en Y
