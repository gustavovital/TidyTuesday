# Script para analise com base em TM sobre a mudança que se teve na pesquisa da CAPES em 20 anos
# de teoria economica
# 
# Data: 26/01/2019
# Autor: @gustavoovital

library(tidyverse)
library(readxl)
library(stringr)
library(tidytext)
library(ggwordcloud)

# base de dados de 1998 - 2018 ----

data_1998_2012 <- read_excel(paste('Data/', 'dados_1998.xlsx', sep = ""))
data_2013_2018 <- read_excel(paste('Data/', 'dados_2013.xlsx', sep = ""))

for( i in 2:(length(list.files('Data')) - 1)) {
  
  if( i <= 15 ) {
    
    support <- read_excel(paste('Data/', list.files('Data/')[i], sep = ""))
    data_1998_2012 <- rbind(data_1998_2012, support)
  } else {
    
    support <- read_excel(paste('Data/', list.files('Data/')[i + 1], sep = ""))
    data_2013_2018 <- rbind(data_2013_2018, support)
  }
  
}

# Manipulação dos dados ----

# contagem dos anos para verificar se falta ou nao alguma observação ou variavel

# data_1998_2012 %>% 
#   count(AnoBase)
# 
# data_2013_2018 %>% 
#   count(AN_BASE)
#
# colnames(data_1998_2012)
# colnames(data_2013_2018)
#
# Foi optado entao pelas seguintes variáveis :
#
#   Ano do corte - data_1998_2012$AnoBase, data_2013_2018$AN_BASE
#   Sigla da instituição - data_1998_2012$SiglaIes, data_2013_2018$SG_ENTIDADE_ENSINO
#   palavra chave -
#   nome orientador - ata_1998_2012$Orientador_1, data_2013_2018$NM_ORIENTADOR 
#   grande area de conhecimento - data_1998_2012$GrandeAreaDescricao, data_2013_2018$NM_GRANDE_AREA_CONHECIMENTO
#   curso - ata_1998_2012$NomePrograma, data_2013_2018$NM_PROGRAMA


data <-
  tibble(
    ANO = c(data_1998_2012$AnoBase, data_2013_2018$AN_BASE),
    GRANDE_AREA = c(
      data_1998_2012$GrandeAreaDescricao,
      data_2013_2018$NM_GRANDE_AREA_CONHECIMENTO
    ),
    ORIENTADOR = tolower(stringi::stri_trans_general(
      c(data_1998_2012$Orientador_1, data_2013_2018$NM_ORIENTADOR),
      id = "Latin-ASCII"
    )),
    IES = c(data_1998_2012$SiglaIes, data_2013_2018$SG_ENTIDADE_ENSINO),
    CURSO = c(data_1998_2012$NomePrograma, data_2013_2018$NM_PROGRAMA),
    PESQUISA = str_trim(gsub(".*-", "", tolower(
      c(
        data_1998_2012$LinhaPesquisa,
        data_2013_2018$NM_LINHA_PESQUISA
      )
    ))),
    PCHAVE = tolower(tm::removeNumbers(
      c(
        data_1998_2012$PalavrasChave,
        data_2013_2018$DS_PALAVRA_CHAVE
      )
    ))
  )

data_eco <- data %>% 
  filter(CURSO == 'ECONOMIA') 

eco_pchaves <- data_eco %>%
  na.omit() %>%
  group_by(ANO) %>%
  tidytext::unnest_tokens(bigram, PCHAVE, token = str_split, pattern = ',') %>%
  group_by(ANO) %>%
  count(bigram) %>%
  group_by(ANO) %>%
  tidytext::unnest_tokens(bigram, bigram, token = str_split, pattern = ';') %>%
  group_by(ANO) %>%
  count(bigram) %>%
  group_by(ANO) %>%
  tidytext::unnest_tokens(bigram, bigram, token = str_split, pattern = '-') 


eco_pchaves$bigram <- tm::removePunctuation(eco_pchaves$bigram) %>%
  str_trim()

eco_pchaves %>%
  filter(bigram != "") %>% 
  arrange(desc(n)) -> eco_pchaves
  
# analise de palavras chaves ----

eco_pchaves %>% 
  filter(ANO != 1998) %>% 
  group_by(ANO) %>% 
  top_n(4) %>% 
  ggplot(aes(label = bigram, colour = as.factor(ANO))) +
  labs(title = expression(bold('Evolução dos Temas de Pesquisa nas Teses de Mestrado e Doutorado em Economia')~'(CAPES)'),
       subtitle = 'Com base nas palavras chaves das teses - de 1999 à 2018 - Brasil', caption = 'Fonte: CAPES\nElaboração: @gustavoovital') +
  geom_text_wordcloud(size = 3.6, family = 'Bookman', show.legend = FALSE, scales = 'free') +
  scale_colour_manual(values = viridis::inferno(80, direction = 1)) +
  facet_wrap(~ANO) +
  theme_minimal() +
  theme(plot.title = element_text(size = 30, colour = 'gray25', family = 'Bookman'),
        plot.subtitle = element_text(size = 25, colour = 'gray30', family = 'Bookman'),
        plot.caption = element_text(size = 20, colour = 'gray30', family = 'Bookman'),
        strip.text.x = element_text(colour = 'gray30', size = 15, family = 'Bookman', hjust = 0))
        

# analise de teses por universidades ----

data_eco %>%  
#  group_by(ANO) %>% 
  count(IES) %>% 
  arrange(desc(n)) %>% 
  top_n(12) -> top_10_pub

data_eco %>% 
  filter(IES %in% top_10_pub$IES) %>% 
  group_by(ANO) %>% 
  count(IES) %>% 
  ggplot(aes(ANO, n, colour = as.factor(IES))) +
  geom_line(size = 3.5, alpha = .6, show.legend = FALSE) +
  scale_color_manual(values = viridis::inferno(30)) +
  labs(title = expression(bold('Evolução no Número de Teses de Mestrado e Doutorado em Economia')~'(CAPES)'),
       subtitle = 'Com base nas faculdades com maior número de defesas - de 1999 à 2018 - Brasil',
       caption = 'Fonte: CAPES\nElaboração: @gustavoovital',
       y = 'Número de Teses de Mestrado e Doutorado', x = NULL)  +
  facet_wrap(~IES, scales = 'free_y') +
  theme_minimal() +
  theme(plot.title = element_text(size = 30, colour = 'gray25', family = 'Bookman'),
        plot.subtitle = element_text(size = 25, colour = 'gray30', family = 'Bookman'),
        plot.caption = element_text(size = 20, colour = 'gray30', family = 'Bookman'),
        strip.text.x = element_text(colour = 'gray30', size = 15, family = 'Bookman', hjust = 0),
        axis.text = element_text(colour = 'gray30', size = 12, family = 'Bookman'),
        axis.title.y = element_text(colour = 'gray30', size = 20, family = 'Bookman'))

# temas mais recorrentes ----

data_eco %>% 
  filter(ANO > 2008,
         PESQUISA != 'o comércio externo do nordeste brasileiro: perspectivas na nova ordem econômica internacional') %>% 
  count(PESQUISA) %>% 
  na.omit() %>% 
  filter(n > 30) %>% 
  ggplot(aes(reorder(tools::toTitleCase(PESQUISA), n), n, fill = as.factor(n)))  +
  geom_hline(yintercept = c(0,  100, 200, 300, 400), alpha = .1, colour = 'gray70', size = 1) +
  geom_col(show.legend = FALSE, alpha = .6) +
  labs(title = expression(bold('Temas de Pesquisa mais Recorrentes')~'(CAPES)'),
       subtitle = 'Economia. 2008-2018', x = NULL, y = NULL,
       caption = 'Fonte: CAPES\nElaboração: @gustavoovital') +
  coord_flip() +
  scale_fill_manual(values = viridis::inferno(100)) +
  theme_minimal() +
  theme(plot.title = element_text(size = 30, colour = 'gray25', family = 'Bookman', hjust = 1.2),
        plot.subtitle = element_text(size = 25, colour = 'gray30', family = 'Bookman', hjust = -2.43),
        plot.caption = element_text(size = 20, colour = 'gray30', family = 'Bookman'),
        strip.text.x = element_text(colour = 'gray30', size = 15, family = 'Bookman', hjust = 0),
        axis.text = element_text(colour = 'gray30', size = 12, family = 'Bookman'),
        axis.title.y = element_text(colour = 'gray30', size = 20, family = 'Bookman'),
        plot.background = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank())
  
  
