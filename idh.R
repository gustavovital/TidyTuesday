# Script para a evolução do IDH comparado com expectativa de escolaridade
#
# Data: 10/12/2019
# Autor: gustavovital@id.uff.br

# pacotes necessários ====

library(tidytext)
library(ggplot2)
library(gganimate)

# base de dados ====

data <- read.csv('idh_gini.csv', dec = ',')
data$ANO <- rep(seq(as.Date('1990-01-01'), as.Date('2018-01-01'), 'years'), 9)

data$IDH <- data$IDH/1000

data_teste <- subset(data, ANO == '2018-01-01')

# gif ====

gif <- ggplot(data, aes(x = ESCOLA, label = PAIS)) +
  geom_point(aes(y = IDH, colour = PAIS, size = ESCOLA*IDH), alpha = .6) +
  geom_line(aes(y = IDH, colour = PAIS, size = ESCOLA*IDH), alpha = .1) +
  geom_text(aes(y = IDH), vjust =-.4) +
  
  labs(x = 'Expectativa de Escolaridade (Anos)', y = 'Índice de Desenvolvimento Humano', 
       title = 'Variação do Índice de Desenvolvimento Humano ', subtitle = 'Comparado com a Expectativa de Escolaridade (Países do MERCOSUL)',
       caption = 'Ano: {frame_time}\nFonte: ONU\n@gustavoovital') +
  
  theme_minimal() +
  
  theme(plot.title = element_text(size = 16, colour = 'tomato3'),
        plot.subtitle = element_text(size = 16, colour = 'aquamarine4'),
        legend.position =  'none',
        plot.caption = element_text(size = 12, colour = 'tomato3')) +

  transition_time(ANO) +
  shadow_wake(wake_length = .5, exclude_layer = 3) +
  ease_aes('cubic-in-out')

gif_idh <- animate(gif, fps = 10, width = 700)

magick::image_write(gif_idh, path="idh.gif")
  
