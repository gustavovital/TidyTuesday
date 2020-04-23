# just some fun with some fun
#
# autor: gustavovital@id.uff.br

library(tidyverse)
library(ggalt)
library(grid)
library(RColorBrewer)

cannabis <- read_csv('cannabis.csv')

cannabis_clean <- cannabis %>% 
  mutate(Efeito = word(str_replace_all(Effects, '[[:punct:]]', ' '), 1)) %>% 
  mutate(Sabor = word(str_replace_all(Flavor, '[[:punct:]]', ' '), 1)) %>%
  select(Strain, Type, Rating, Efeito, Sabor) %>% 
  na.omit() %>% 
  filter(Sabor != 'None')

rm(cannabis)

theme_green <- theme(panel.background = element_rect(fill = 'lemonchiffon1'),
                     plot.background = element_rect(fill = 'lemonchiffon2'),
                     panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lemonchiffon2"),
                     panel.grid.minor = element_line(size = 0.5, linetype = 'solid', colour = "lemonchiffon2"),
                     panel.border = element_rect(colour = 'lemonchiffon1', fill=NA, size=1),
                     
                     axis.text.y   = element_text(size=11, colour = 'seagreen4'),
                     axis.text.x   = element_text(size=11, colour = 'seagreen4'),
                     axis.title.y  = element_text(size=14, colour = 'seagreen4'),
                     axis.title.x  = element_text(size=14, colour = 'seagreen4'),
                     plot.title = element_text(size = 18, colour = 'seagreen4'),
                     plot.subtitle = element_text(size = 14, colour = 'seagreen4'),
                     plot.caption = element_text(size = 12, colour = 'seagreen4'),
                     
                     legend.background = element_rect(fill = 'lemonchiffon2'),
                     legend.position = 'bottom',
                     legend.text = element_text(size=11, colour = 'seagreen4'))

# brincadeira com os dados ----

cannabis_clean %>% head()

# Boxplot by flavor ----

cannabis_clean %>% 
  ggplot(aes(x = reorder(Sabor, Rating), y = Rating)) +
  geom_boxplot(colour = 'black', fill = 'darkgreen', alpha = .8, size = .2) +
  coord_flip() +
  labs(x = 'Flavors', y = 'Ratings', title = 'Boxplots of Ratings by flavors of Cannabis',
       subtitle = 'From 0 to 5', caption = 'Elaboração: @gustavoovital') +
  theme_minimal() +
  theme_green

# Histogram of best cannabis types ----

cannabis_clean %>% 
  mutate(media = mean(Rating)) %>% 
  mutate(ranking = ifelse(Rating >= media, 'Above', 'Below')) %>% 
  group_by(Efeito, Type) %>% 
  summarise(media = mean(Rating)) %>% 
  filter(Efeito != 'Dry', Efeito != 'None') %>% 
  
  ggplot(aes(reorder(Efeito, media), media, fill = Type)) +
  geom_col(position = 'dodge', alpha = .7) +
  scale_fill_manual(values = c('black', 'darkgreen', 'yellow3'), name = '') +
  labs(x = 'Effects', y = 'Average Rating', title = 'Average Rating by Cannabis Effects', caption = 'Elaboração: @gustavoovital') +
  coord_polar() +
  theme_green

# plot normalized ----

cannabis_point <- cannabis_clean %>% 
  group_by(Type, Rating) %>% 
  filter(Efeito == 'Happy' |
           Efeito == 'Sleepy' |
           Efeito == 'Hungry' |
           Efeito == 'Relaxed') %>% 
  group_by(Type, Efeito, Rating) %>% 
  count(Sabor) %>% 
  arrange(desc(n)) 

cannabis_point$Rating_norm <-
  round((cannabis_point$Rating - mean(cannabis_point$Rating))/sd(cannabis_point$Rating), 2)

cannabis_point %>% 
  group_by(Sabor) %>% 
  summarise(n = mean(Rating_norm)) %>% 
  arrange(desc(n)) %>% 
  mutate(class = ifelse(n >= 0, 'Da Boa', 'Michada')) %>% 
  ggplot(aes(x = reorder(Sabor, n), y = n, label = round(n, 2), colour = class)) +
  geom_point(stat = 'identity', size=7, alpha = 1) +
  geom_segment(aes(y = 0, 
                   x = Sabor, 
                   yend = n, 
                   xend = Sabor), size = 3, alpha = .2) + 
  geom_text(colour = 'white', size = 2) +
  scale_colour_manual(values = c('yellow4', 'black'), name = NULL) +
  labs(title = 'Diverging Dot Plot by Flavors', subtitle = 'Normalized Flavors from Dataset', x = NULL, y = NULL, caption = 'Elaboração: @gustavoovital') +
  coord_flip() +
  theme_green +
  theme(legend.position = 'none') +
  annotation_custom(grobTree(textGrob(" ''If you’re a fan of mint flavors in your cannabis and you want 
the most powerful strain you can get, Thin Mint is what you’re 
looking for.'' (marijuanabreak)
                                      
''Must have been Minty-Greeny-Green-Afghani-Widow-Goo.'' 
(anonymous) ", 
                                      x=0.06,  y=0.5, hjust=0,
                                      gp=gpar(col="black", fontsize=12, fontface="italic"))))

# cannabis that make us happy :) ----


cannabis_clean %>% 
  group_by(Type, Efeito, Sabor) %>% 
  summarise(media = mean(Rating)) %>% 
  filter(media > 3 & Efeito != 'None' & Efeito != 'Dry') %>% 

  ggplot(aes(reorder(Efeito, media), media)) +
  geom_point(size = 7, colour = 'yellow4', alpha = .1) +
  geom_boxplot(fill = 'darkgreen', colour = 'darkgreen', alpha = .8) +
    
  labs(title = 'Boxplot of Rating by Effects', x = NULL, y = 'Rating', subtitle = 'Without "Dry" Effect', caption = 'Elaboração: @gustavoovital') +
  
  #coord_flip() +
  #geom_text( position=position_stack(vjust = 0.5), vjust=0.5, size = 2.5) +
  theme_green +
  theme(axis.text.x = element_text(angle = 45, vjust=0.5))
