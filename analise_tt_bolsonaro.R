# Análise do twitter do presidente bolsonaro durante o ano de 2019
# script para a criação de um corpus

# Autor: gustavovital@id.uff.br
# data: 28/12/2019

setwd("/media/ragnar/Seagate Expansion Drive/tidyverse/twitter_bolsonaro")

# Pacotes Necessários e stopwords ====

library(tidyverse)
library(rio)
library(rtweet)
library(qdap)
library(tm)
library(ggthemes)
library(wordcloud)
library(RColorBrewer)
library(ggwordcloud)
library(ggimage)
library(patchwork)
library(lubridate)
library(ggwordcloud)

stop_words <- as.character(read.table('stopwords.txt')$V1)

# pegando os tweets do presidente ====

bolsonaro <- 
  get_timeline('jairbolsonaro', n = 3200)

bolsonaro <- bolsonaro %>% # tweets do ano de 2019 
  slice(1:2568)
saveRDS(bolsonaro, 'bolsonarott.rds')

bolsonaro <- bolsonaro %>% 
  select(created_at, text, favorite_count, retweet_count)

# começando uma alálise textual ====

bolsonaro %>%  glimpse()

# Criando um Corpus ====

bolso_Vector <- VectorSource(bolsonaro$text)
bolso_corpus <- VCorpus(bolso_Vector) 

meta(bolso_corpus)

# limpando o corpus ====

words_to_discart <- c('bolsonaro', 'milhares', 'milhões', 'bilhões', 'dia', 'dias', 'semana',
                      'semanas', 'mês', 'meses', 'ano', 'anos', 'via', 'vamos', 'jair', 'presidente',
                      'período', 'parabens', 'abraço', 'jairbolsonaro', 'https')

clean_corpus <-
  function(corpus){
    
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removeWords, words = c(stop_words, words_to_discart))
    corpus <- tm_map(corpus, stripWhitespace)
    return(corpus)
  }

bolso_clean <- clean_corpus(bolso_corpus)

# Análise Textual ====

bolso_tdm <- TermDocumentMatrix(bolso_clean)
bolso_matrix <- as.matrix(bolso_tdm)

term_frequency <- rowSums(bolso_matrix)
term_frequency <- sort(term_frequency, decreasing = TRUE)

head(term_frequency, n = 50)

saveRDS(term_frequency, 'term_frequency.rds')
rm(list = ls())

library(tidytext)

# base de dados ====

bolsonaro <- readRDS('bolsonarott.rds')
bolsonaro <- bolsonaro %>% 
  select(created_at, text, favorite_count, retweet_count)

# criando bigrams ====

words_to_discart <- c('bolsonaro', 'milhares', 'milhões', 'bilhões', 'dia', 'dias', 'semana',
                      'semanas', 'mês', 'meses', 'ano', 'anos', 'via', 'vamos', 'jair', 'presidente',
                      'período', 'parabens', 'abraço', 'jairbolsonaro', 'https', '8', '2018')
stop_words <- as.character(read.table('stopwords.txt')$V1)

termos_bolsonaro <- bolsonaro %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  count(bigram, sort = TRUE) %>% 
  separate(bigram, c('termo1', 'termo2'), sep = ' ') %>% 
  filter(!termo1 %in% c(stopwords('pt'), c(stop_words, words_to_discart))) %>% 
  filter(!termo2 %in% c(stopwords('pt'), c(stop_words, words_to_discart))) 

termos_bolsonaro <- na.omit(termos_bolsonaro)
saveRDS(termos_bolsonaro, 'termos_bolsonaro.rds')

# script para visualização dos tts do pr bolsonaro

# autor: gustavovital@id.uff.br
# data: 28/12/2019

options(scipen = 999)

setwd("/media/ragnar/Seagate Expansion Drive/tidyverse/twitter_bolsonaro")

# pacotes ====

library(tidyverse)
library(ggthemes)
library(wordcloud)
library(RColorBrewer)
library(ggwordcloud)
library(qdap)
library(ggimage)
library(patchwork)
library(lubridate)

# base de dados ====

df_frequency <-
  tibble(palavra = names(readRDS('term_frequency.rds')),
         n = readRDS('term_frequency.rds'))

df_frequency_bar <- df_frequency %>% 
  slice(1:30)

termos_bolsonaro <- readRDS('termos_bolsonaro.rds')
termos_bolsonaro <- termos_bolsonaro %>% 
  filter(n >= 10)

bolsonaro <- readRDS('bolsonarott.rds')
stop_words <- as.character(read.table('stopwords.txt')$V1)
words_to_discart <- c('bolsonaro', 'milhares', 'milhões', 'bilhões', 'dia', 'dias', 'semana',
                      'semanas', 'mês', 'meses', 'ano', 'anos', 'via', 'vamos', 'jair', 'presidente',
                      'período', 'parabens', 'abraço', 'jairbolsonaro', 'https', '8', '2018')
# grafico de barras ====

# palavras ====

pal <- ggplot(df_frequency_bar, aes(reorder(palavra, n), n)) +
  geom_col(fill = 'tomato3', colour = 'white', alpha = .75) +
  coord_flip() +
  labs(title = 'Palavras que mais apareceram no Twitter do Bolsonaro',
       subtitle = 'Durante o ano de 2019',
       x = NULL, y  = 'Número de ocorrências',
       caption = 'Fonte: @jairbolsonaro\nElaboração: @gustavoovital') +
  scale_y_continuous(breaks = seq(0, 600, 100)) +
  theme_minimal()

# termos ====

termos_bolsonaro <- termos_bolsonaro %>% 
  mutate(termos = paste(termo1, termo2))

ter <- ggplot(termos_bolsonaro, aes(reorder(termos, n), n)) +
  geom_col(fill = 'dodgerblue3', colour = 'white', alpha = .75) +
  coord_flip() +
  labs(title = 'Termos que mais apareceram no Twitter do Bolsonaro',
       subtitle = 'Durante o ano de 2019',
       x = NULL, y  = 'Número de ocorrências') +
  scale_y_continuous(breaks = seq(0, 30, 5)) +
  theme_minimal()


ter+pal
# nuves de palavras ====

tt_nuvem <- df_frequency %>%
  slice(1:800) %>% 
  mutate(angle = 45 * sample(-2:2, n(), replace = TRUE, prob = c(1, 1, 4, 1, 1)))

ggplot(tt_nuvem, aes(label = palavra, size = n,
                     color = factor(sample.int(3, nrow(tt_nuvem), replace = TRUE)))) +
  geom_text_wordcloud_area(
    mask = png::readPNG("twitter_logo.png")
  ) +
  scale_size_area(max_size = 20) +
  scale_color_manual(values=c("dodgerblue2", "dodgerblue3", "dodgerblue4")) +
  labs(title = 'Núvem de palavras do twitter do Bolsonaro',
       subtitle = 'Durante o ano de 2019',
       caption = 'Fonte: @jairbolsonaro\nElaboração: @gustavoovital') +
  theme_minimal()

# rts e curtidas ====

bolso <- bolsonaro %>% 
  select(created_at, favorite_count, retweet_count)

bolso %>% 
  mutate(mes = month(created_at)) %>% 
  group_by(mes) %>% 
  summarise(likes = mean(favorite_count),
            rts = mean(retweet_count)) -> bolso

bolso$mes <- seq(as.Date('2019-01-01'), length.out = 12, by = 'm')

bolso %>% 
  pivot_longer(-mes) -> bolso

ggplot(bolso, aes(x = mes, fill = name, label = value)) +
  geom_vline(xintercept = bolso$mes, linetype = 2, colour = 'gray30', alpha = .5) +
  geom_area(aes(y = value), alpha = .6) +
  scale_fill_manual(values = c('likes'='tomato2',
                               'rts'='dodgerblue4'),
                    labels = c('Likes', 'Retweets'),
                    name = NULL) +
  labs(title = 'Média de likes e RTs por mês, do twitter do Bolsonaro',
       subtitle = 'Durante o ano de 2019',
       x = NULL, y = 'Valores') +
  theme_minimal() +
  theme(legend.position = 'bottom')
