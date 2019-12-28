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
