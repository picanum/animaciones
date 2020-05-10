library(tidyverse)
library(extrafont)
library(gganimate)

animacion <- do.call(rbind.data.frame, lapply(c(100, 200, 500, 1000, 2000, 5000, 10000, 20000, 50000), 
  function(i) data.frame(y = dgeom(1:2000,1/i), x = 1:2000, prob = paste0("1 entre ", i)))) %>% 
  ggplot(aes(x = x, y = y)) + 
  geom_line(size = 1.1, linetype = "dotted") + 
  transition_states(prob) + 
  labs(title = "Función masa de probabilidad\nde una distribución geométrica para un suceso con\nuna probabilidad de ocurrencia de {closest_state}", 
    x = "Valor de X (número de fracasos)", 
    y = "Probabilidad de que fracase todas esas veces\nantes del primer éxito", 
    subtitle = "La distr. geométrica mide la probabilidad de fracasar un cierto\nnúmero de veces hasta observar la 1ª ocurrencia del suceso\nde interés", 
    caption = "@Picanumeros") + 
  theme_classic(base_size = 14) + 
  theme(text = element_text(family = "Bahnschrift"))
  
animate(animacion, width = 1280/2, height = 1080/2, res = 100)
anim_save("geometrica.gif")
