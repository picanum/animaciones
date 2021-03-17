library(tidyverse)
library(extrafont)
library(gganimate)

g <- data.frame(dens = unlist(lapply(2:8, function(i) dbeta(seq(0, 1, by = 0.01), i, 10 - i))),
           k = rep(seq(0, 1, by = 0.01), 7), 
           p = rep(2:8, each = 101),
           mediana = rep(sapply(2:8, function(i) qbeta(0.5, i, 10 - i)), each = 101),
           moda = rep(sapply(2:8, function(i) (i-1)/(i+10-i-2)), each = 101),
           media = rep(sapply(2:8, function(i) i/(i+10-i)), each = 101)) %>%
  ggplot(aes(x = k, y = dens)) + geom_line(col = "white", size = 1.05) +
  geom_vline(aes(xintercept = mediana), color='deepskyblue',linetype='dashed', size = 1.05)+
  geom_vline(aes(xintercept = media), color='red',linetype='dashed', size = 1.05)+
  geom_vline(aes(xintercept = moda), color='orange',linetype='dashed', size = 1.05)+
  transition_time(p) +
  theme_classic(base_size = 18) +
  theme(plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        axis.line = element_line(colour = "white"),
        axis.text = element_text(colour = "white"),
        axis.ticks = element_line(colour = "white"),
        plot.caption = element_text(family = "Forte", colour = "white", size = 15),
        text = element_text(family = "Leelawadee UI", colour = "white")) +
  geom_text(aes(x = moda-0.01, y = 3, label = paste0("Moda = ", round(moda,3))), 
            col = "orange", family = "Leelawadee UI", hjust = 1) +
  geom_text(aes(x = mediana-0.01, y = 2, label = paste0("Mediana = ", round(mediana,3))), 
            col = "deepskyblue", family = "Leelawadee UI", hjust = 1) +
  geom_text(aes(x = media-0.01, y = 1, label = paste0("Media = ", round(media,3))), 
            col = "red", family = "Leelawadee UI", hjust = 1) +
  labs(x = "Valor de X", y = "Frecuencia (densidad)",
       title = "¿Cómo se comportan *habitualmente* la media,\nla mediana y la moda según la simetría de los datos?",
       subtitle = "Distribución Beta de parámetros alpha = {frame_time}, beta = {10-(frame_time)}",
       caption = "Twitter/Instagram: @Picanumeros")
animate(g, nframes = 150, height = 1200, width = 1200, res = 150)
