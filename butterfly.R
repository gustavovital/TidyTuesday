# script para a curva da borboleta ====
#
# Autor: gustavovital@id.uff.br

# fonte: https://pt.wikipedia.org/wiki/Curva_da_borboleta_(transcendental)

# pacotes necessários ====

library(tidyverse)
library(gganimate)

# definindo as funções: ====

butterfly_x <- 
  function(t){
    sin(t)*(exp(cos(t)) - 2*cos(4*t) - sin(t/12)^5)
  }

butterfly_y <- 
  function(t){
    cos(t)*(exp(cos(t)) - 2*cos(4*t) - sin(t/12)^5)
  }

x <- butterfly_x(seq(-12*pi,12*pi,.01))
y <- butterfly_y(seq(-12*pi,12*pi,.01))


df <-
  tibble(x = x,
         y = y,
         indice = 1:length(x))

pal <- wes_palette("Rushmore", type = "continuous")

ggplot(df, aes(y = y, x = x)) +
  geom_path(aes(colour = indice), alpha = .8, size = 1.3) +
  # geom_line(aes(colour = indice), alpha = .01, size = 1) +
  #geom_point() +
  theme_void() +
  #scale_colour_gradientn(colours = pal) + 
  
  transition_reveal(indice)

