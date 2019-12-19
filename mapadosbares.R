# bagunça ggplot ====

library(tidyverse)
library(ggmap)
library(ggalt)
library(wesanderson)
library(ggExtra)
library(ggrepel)
library(patchwork)

# register_google(key = "AIzaSyCFR98MASoEpX7xOXkHHrRbyfu6FDxaG4s")

# Localizando os Campi da UFF ====

uff_valonguinho <- qmap("R. Mario Santos Braga, 30 - Centro, Niterói - RJ, 24020-140", zoom=16, source = "google", maptype="roadmap")
uff_gragoata <- qmap('R. Alexandre Moura, 8 - São Domingos, Niterói - RJ, 24210-200', zoom=17, source = "google", maptype="roadmap")
gragoata_code <- geocode('R. Alexandre Moura, 8 - São Domingos, Niterói - RJ, 24210-200')
valonguinho_code <- geocode("R. Mario Santos Braga, 30 - Centro, Niterói - RJ, 24020-140")


# Bares Gragoata \oo/ ====

MaedAgua <- 'Praça Leoni Ramos, 7 - São Domingos, Niterói - RJ, 24210-205'
AdegaDoBacalhau <- 'Praça Leoni Ramos, 13 - terreo - São Domingos, Niterói - RJ, 24210-205'
BarDoRenato <- 'Praça Leoni Ramos, 19 - São Domingos, Niterói - RJ, 24210-205'
EstacaoCantareira <- 'Praça Leoni Ramos, 9 - São Domingos, Niterói - RJ, 24210-205'
Briggs <- 'Rua Guilherme Briggs, 29 - São Domingos, Niterói - RJ, 24210-175'
TheJoint <- 'Av. Visconde do Rio Branco, 887 - São Domingos, Niterói - RJ, 24020-006'
TioCoto <- 'R. Alexandre Moura, 3 - São Domingos, Niterói - RJ, 24210-200'
VestibularDoChopp <- 'R. Gen. Osório, 02 - São Domingos, Niterói - RJ, 24210-190'
Recreio <- 'R. Gen. Osório, 13 - São Domingos, Niterói - RJ, 24210-180'
BarDoPedro <- 'R. José Bonifácio, 6 - São Domingos, Niterói - RJ, 24210-230'

gragoata_places <-
  tibble(locations = c(MaedAgua, AdegaDoBacalhau, BarDoRenato, EstacaoCantareira, Briggs, TheJoint, TioCoto, 
                       VestibularDoChopp, Recreio, BarDoPedro))

gragoata_bars <- geocode(gragoata_places$locations)
  
# Bares Valonguinho \oo/ ====

Arretado1 <- 'Av. Visconde do Rio Branco, 633, Niterói - RJ, 24020-005'
Arretado2 <- 'Av. Visconde do Rio Branco, 656 - Centro, Niterói - RJ, 24020-005'
DCE <- 'Av. Visconde do Rio Branco, 625 - Térreo - Centro, Niterói - RJ, 24020-005'
Posto <- 'Av. Visconde do Rio Branco, 756 - Centro, Niterói - RJ, 20060-080'
BarDoCanto <- 'Canto do Rio Foot-Ball Club - Av. Visconde do Rio Branco, 701 - Centro, Niterói - RJ, 24020-005'

valonguinho_places <- 
  tibble(locations = c(Arretado1, Arretado2, DCE, Posto, BarDoCanto))

valonguinho_bars <- geocode(valonguinho_places$locations)

# gragoata ====

code_gragoata <- tibble(lon = as.numeric(-43.131642),
                        lat = as.numeric(-22.898638))

uff_gragoata <- get_map(location = code_gragoata, zoom = 17, scale = 2, source = "google", maptype="roadmap")


# Mapa de Bares Gragoata ====

map_grag <- ggmap(uff_gragoata) +
  geom_point(aes(x = lon, y = lat), data = gragoata_bars, size = 25, alpha = .2, colour = 'tomato2') +
  geom_point(aes(x = lon, y = lat), data = gragoata_bars, size = 2, alpha = 1, colour = 'tomato4') +
  geom_point(aes(x = lon, y = lat), data = code_gragoata, size = 70, colour = 'dodgerblue2', alpha = 1, shape = 1) +
  geom_point(aes(x = lon, y = lat), data = code_gragoata, size = 2, colour = 'dodgerblue4', alpha = 1) +
  
  labs(x = NULL, y = NULL, subtitle = 'Gragoatá') +
  
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        plot.subtitle = element_text(colour = 'tomato4', size = 16)) 

# Valonguinho ====

code_valonguinho <- tibble(lon = as.numeric(-43.125544),
                           lat = as.numeric(-22.896408))

uff_valonguinho <- get_map(location = code_valonguinho, zoom = 17, scale = 2, source = "google", maptype="roadmap")
 
# Mapa de Bares Valonguinho ====

map_valong <- ggmap(uff_valonguinho) +
  geom_point(aes(x = lon, y = lat), data = valonguinho_bars, size = 25, alpha = .2, colour = 'orchid2') +
  geom_point(aes(x = lon, y = lat), data = valonguinho_bars, size = 2, alpha = 1, colour = 'orchid4') +
  geom_point(aes(x = lon, y = lat), data = code_valonguinho, size = 70, colour = 'dodgerblue2', alpha = 1, shape = 1) +
  geom_point(aes(x = lon, y = lat), data = code_valonguinho, size = 2, colour = 'dodgerblue4', alpha = 1) +
  
  labs(x = NULL, y = NULL, subtitle = 'Valonguinho',
       caption = 'Fonte: googlemaps\nElaboração: @gustavoovital') +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        plot.subtitle = element_text(colour = 'orchid4',
                                     size = 16))

# mapa de referencia ====

referencia <- qmap('Av. Visconde do Rio Branco, 755 - Centro, Niterói - RJ, 24020-006', zoom = 17, source = "google", maptype="roadmap")

mapa_ref <- referencia +
  geom_point(aes(x = lon, y = lat), data = valonguinho_bars, size = 2, alpha = 1, colour = 'orchid4') +
  geom_point(aes(x = lon, y = lat), data = gragoata_bars, size = 2, alpha = 1, colour = 'tomato4') +
  
  geom_encircle(aes(x=lon, y=lat), data = valonguinho_bars, size = 2, fill = "orchid2", alpha = .2, s_shape=.05, expand=.05) +
  geom_encircle(aes(x=lon, y=lat), data = gragoata_bars, size = 2, fill = "tomato2", alpha = .2, s_shape=.5, expand=.05) +
  
  geom_point(aes(x = lon, y = lat), data = code_valonguinho, size = 70, colour = 'dodgerblue2', alpha = 1, shape = 1) +

  labs(x = NULL, y = NULL, title = 'Mapa de Bares Próximos aos Campi do Valonguinho e do Gragoatá', caption = 'Fonte: googlemaps\nElaboração: @gustavoovital') +
  theme(plot.title = element_text(colour = 'seagreen4', size = 16))

# Avaliação de ambientes e numero de avaliações ====

data <- 
  tribble(
    ~Local, ~Nota, ~avaliacoes, ~Preço,
    'Mae d\'Agua', 4.2, 197, 'Indisponível',
    'Adega do Bacalhau', 4, 116, '$',
    'Bar do Renato', 3.7, 113, '$',
    'Estação Cantareira', 4.1, 65, 'Indisponível',
    'Briggs Gourmet', 4.5, 67, 'Indisponível',
    'The Joint Music Bar', 4.3, 27, 'Indisponível',
    'Bar Tio Coto', 4, 1524, '$$',
    'Vestibular do Chopp', 4.1, 980, '$',
    'Recreio bar e Sinuca', 4.5, 25, 'Indisponível',
    'Bar do Pedro', 3.4, 5, 'Indisponível',
    'Arretado', 4.7, 608, 'Indisponível',
    'DCE', 3.5, 19, 'Indisponível',
    'Posto Petrobras', 3.9, 428, 'Indisponível', 
    'Bar do Canto', 4.3, 167, '$$'  
  )

# notas medias ====

graf_media <- ggplot(data, aes(x = reorder(Local, Nota), y = Nota, label = Nota)) +
  geom_col(aes(fill = Preço), alpha = .8) +
  coord_flip() +
  scale_fill_manual(values = wes_palette("Darjeeling2", n = 3)) +
  geom_label(position=position_stack(vjust = .9), vjust=0.5, size = 3, colour = 'black') +
  labs(y = 'Nota Média Avaliada', x = NULL, title = 'Notas Médias dos Bares próximos aos Campi do Valonguinho e do Gragoatá', subtitle = 'Por Preço e Bar') +
  theme_minimal() +
  theme(legend.position = 'bottom',
        plot.title = element_text(size = 16, colour = 'dodgerblue3'),
        plot.subtitle = element_text(colour = 'dodgerblue4', size = 12))

# nota avaliacao ====

graf_nota <- ggplot(data, aes(x = Nota, y = avaliacoes, label = Local)) +
  
  geom_point(aes(size = Preço, colour = Preço), alpha = .5) +
  geom_hline(yintercept = mean(data$avaliacoes), linetype="dashed", color = "grey50") +
  geom_vline(xintercept = mean(data$Nota), linetype="dashed", color = "grey50") +
  geom_label_repel(size = 3) +
  
  scale_colour_manual(values = wes_palette("Darjeeling1", n = 3)) +
  scale_x_continuous(limits = c(3, 5)) +
  
  labs(x = 'Nota Média Avaliada', y = 'Número de Avaliações',
       title = 'Comparação do Número de Avaliações com a Nota Média Avaliada', subtitle = 'Por Preço e Bar',
       caption = 'Fonte: googlemaps\nElaboração: @gustavoovital') +
  
  theme_minimal() +
  theme(legend.position = 'bottom',
        plot.title = element_text(size = 16, colour = 'tomato3'),
        plot.subtitle = element_text(colour = 'tomato4', size = 12))

ggMarginal(g, type = "boxplot", fill="transparent", bins = 20, size = 25)


# plots ====

mapa_ref 

graf_media + graf_nota

map_grag + map_valong + plot_annotation(title = 'Proximidade dos Campi do Gragoatá e Valonguinho dos Bares')
