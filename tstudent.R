library(dplyr)
library(ggplot2)
library(gganimate)

x <- rt(10000,1)

anim <- data.frame(x = 1:10000, y = sapply(1:10000, function(i) mean(x[1:i]))) %>%
    ggplot(aes(x = x, y = y)) + geom_hline(yintercept = 0, col = "red") + geom_line(size=1.05) + theme_classic(base_size = 18) + 
    labs(x = "Tamaño de muestra (n)", y = "Media de la muestra, sum(Xi)/n", 
         title = "Medias aritméticas sucesivas de una t de Student con g. l. = 1\na lo largo de 10000 simulaciones", 
         caption = "@Picanumeros") + 
         scale_x_continuous(breaks = seq(0, 10000, by = 1000)) + 
         transition_reveal(along = 1:10000)
        
animate(anim, width = 700, height = 400)

anim_save("tstudent.gif")
