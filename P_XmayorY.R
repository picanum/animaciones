library(ggplot2)
library(gganimate)
library(ggthemes)

datos_grafico <- data.frame(a = seq(0, 10, by = 0.01), b = seq(10, 0, by = -0.01))
datos_grafico$instr <- datos_grafico[,1] - datos_grafico[,2]
datos_grafico$p_XmayorY <- ifelse(datos_grafico$instr < 0, datos_grafico$a/(2*datos_grafico$b), 1 - datos_grafico$b/(2*datos_grafico$a))

ggplot(datos_grafico, aes(x = instr, y = p_XmayorY)) + geom_line() + transition_reveal(instr) + 
    labs(title = "Evolución de P(X > Y), siendo X ~ U(0, a) e\nY ~ U(0, b), a = {round((frame_along + 10)/2,2)}, b = {round(10 - (frame_along + 10)/2,2)}", 
    x = "a - b", y = "P(X > Y)", caption = "@Picanumeros") + theme_classic(base_size = 18) + scale_y_continuous(breaks = seq(0,1, by = 0.1))
