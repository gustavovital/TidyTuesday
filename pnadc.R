# Script para estatisticas descritivas utilizando 
# a PNAD-Contínua.
# 
# Autor: gustavovital@id.uff.br

# considerações iniciais de acordo com o dicionário da PNAC ====

# VARIÁVEL SIGNIFICADO
# UF       UNIDADE FEDERATIVA
# VD4020   RENDIMENTO MENSAL MEDIO
# V2007    HOMEM OU MULHER
# V2009    IDADE
# V2010    COR


# pacotes necessários: ====

library(PNADcIBGE)
library(survey)
library(ggrepel)
library(wesanderson)
library(tidyverse)

# pegando a PNAD ====

pnad.2019 <- get_pnadc(year = 2019, quarter = 3) # aqui ja baixa a pnad em formato survey, o que acaba
# facilitando a manipulação dos dados quando necessário
# ver dicionario da pnad para os próximos códigos

# manipulando a PNAD ====

svy.pnad <-
  update(pnad.2019,
         categorias_idade = factor( 1 + findInterval( V2009, seq(0, 80, 5))))

# estatisticas descritivas ====

# salario medio por estado e genero ====

tabela.salario <- 
  svyby( ~ VD4020 , ~ UF + V2007, svy.pnad , svymean , na.rm = TRUE )



# salario medio por estado ====

tabela.salario.medio <- 
  svyby( ~ VD4020 , ~ UF, svy.pnad , svymean , na.rm = TRUE )

# cat_idade, salario e uf ====

tabela.salario.medio <- 
  svyby( ~ VD4020 , ~ UF + categorias_idade, svy.pnad , svymean , na.rm = TRUE )

# cor, salario e uf ====

tabela.salario.cor <- 
  svyby( ~ VD4020 , ~ UF + V2010, svy.pnad , svymean , na.rm = TRUE )

tabela.salario.cor %>% 
  filter(V2010 != 'Ignorado') -> tabela.salario.cor

# sobre faixa de idade ====

tabela.idade <- 
  svyby( ~ categorias_idade , ~ V2007, svy.pnad, svytotal, na.rm = TRUE )

# svytotal( ~ categorias_idade , svy.pnad, na.rm = TRUE )


tabela.idade <- tabela.idade[, 1:18]

colnames(tabela.idade) <- c('genero','0-4 Anos', '5-9 Anos', '10-14 Anos', '15-19 Anos', '20-24 Anos', '25-29 Anos', 
                            '30-34 Anos', '35-39 Anos', '40-44 Anos', '45-49 Anos', '50-54 Anos', '55-59 Anos', 
                            '60-64 Anos', '65-69 Anos', '70-74 Anos', '75-79 Anos', '+80 Anos')

tabela.idade %>%
  pivot_longer(-genero, names_to = "faixa_de_idade", values_to = "pop") -> tabela.idade

tabela.idade$faixa_de_idade <- 
  factor(tabela.idade$faixa_de_idade, levels = c('0-4 Anos', '5-9 Anos', '10-14 Anos', '15-19 Anos', '20-24 Anos', '25-29 Anos',
                                                 '30-34 Anos', '35-39 Anos', '40-44 Anos', '45-49 Anos', '50-54 Anos', 
                                                 '55-59 Anos', '60-64 Anos', '65-69 Anos', '70-74 Anos', '75-79 Anos', '+80 Anos'))

tabela.idade$pop <- with(tabela.idade, ifelse(genero == "Homem", -pop, pop))

tabela.idade %>% 
  mutate(freq = abs(pop)/sum(abs(pop))) -> tabela.idade

tabela.idade$freq <- with(tabela.idade, ifelse(genero == "Homem", -freq, freq))

# visualização ====

tabela.salario <- readRDS('tabela.salario.rds')
tabela.salario.medio <- readRDS('tabela.salario.medio.rds')
tabela.salario.cor <- readRDS('tabela.salario.cor.rds')
tabela.idade <- readRDS('tabela.idade.rds')

# salario por genero e uf ====

ggplot(tabela.salario, aes(x = UF, y = VD4020, fill = V2007)) +
  geom_bar(alpha = 1, stat="identity", position = "dodge") +
  coord_flip() +
  labs(x = NULL, y = NULL, title = 'Rendimentos Salariais Médios por Estados',
       subtitle = 'em reais (R$)',
       caption = 'Fonte: PNADC. Terceiro Trimestre de 2019\nElaboração: @gustavoovital') +
  theme_minimal() +
  scale_fill_manual(values = wes_palette("GrandBudapest1", n = 2)) +
  theme(legend.position = 'bottom',
        legend.title = element_blank())

# estados com maiores salarios ====

ggplot(tabela.salario.medio, aes(x = reorder(UF, VD4020), y = VD4020, label = round(VD4020, 2))) +
  geom_bar(alpha = .4, stat="identity", colour = 'dodgerblue4', fill = 'dodgerblue2') +
  geom_text_repel(size = 3, colour = 'dodgerblue4') +
  labs(x = NULL, y = NULL, title = 'Rendimentos Salariais Médios por Estados', subtitle = 'em reais (R$)',
       caption = 'Fonte: PNADC. Terceiro Trimestre de 2019\nElaboração: @gustavoovital') +
  coord_polar() +
  theme_minimal() +
  theme(axis.text = element_text(size=8))

# diferença salarial por cor ====

ggplot(tabela.salario.cor, aes(x = UF, y = VD4020, fill = V2010)) +
  geom_bar(alpha = 1, stat="identity") +
  geom_text(aes(label = round(VD4020,2)), position=position_stack(vjust = 0.5), vjust=0.5, size = 2.5) +
  scale_fill_manual(values = wes_palette("Royal2", n = 5)) +
  coord_flip() +
  labs(x = NULL, y = NULL, title = 'Rendimentos Salariais Médios por Estados e Cor',
       subtitle = 'em reais (R$)', caption = 'Fonte: PNADC. Terceiro Trimestre de 2019\nElaboração: @gustavoovital') +
  theme_minimal() +
  theme(legend.position = 'bottom',
        legend.title = element_blank())
  
# Piramide Etária Brasileira ====

ggplot(data = tabela.idade,  aes(x = faixa_de_idade, y = freq, fill = genero)) +
  geom_col(alpha = .7) +
  coord_flip() +
  scale_y_continuous(limits = c(-0.05, 0.05),
                     breaks = seq(-0.05, 0.05, .01),
                     labels = paste(abs(seq(-5, 5, 1)), '%')) +
  geom_text(aes(label = paste(100*(abs(round(freq,4))), '%')), position=position_stack(vjust = 0.5), vjust=0.5, size = 2.5) +
  scale_fill_manual(values = wes_palette("Royal1", n = 2)) +
    labs(x = NULL, y = NULL, title = 'Pirâmide Etária Brasileira', subtitle = 'Participação Percentual Frente ao Total da População em Destaque',
         caption = 'Fonte: PNADC. Terceiro Trimestre de 2019\nElaboração: @gustavoovital') +
  theme_minimal() +
  theme(legend.position = 'bottom',
        legend.title = element_blank())
