# Script para uma analise sobre religioes ;p

# Autor: gustavovital@id.uff.br
# Data: 14/01/2020

library(sidrar)
library(tidyverse)
library(geobr)
library(ggrepel)
library(ggthemes)
library(treemapify)

# base de daos ----
brasil <- read_state(code_state="all", year=2010)

relig_pcnt <- get_sidra(api = '/t/137/n1/all/n3/all/v/1000093/p/last%202/c133/0,2824,2828,2829,12884,12885,95263,95267,95269,95275,95277/d/v1000093%202')[, c(4,8,10,13)]

# manipulação dos dados ----

relig_pcnt$`Brasil e Unidade da Federação` <- tolower(relig_pcnt$`Brasil e Unidade da Federação`)
brasil$name_state <- tolower(brasil$name_state)
brasil$name_state[18] <- 'espírito santo'

brasil_relig_pcnt <- left_join(relig_pcnt, brasil, by = c('Brasil e Unidade da Federação'='name_state'))

# graficos ----

brasil_relig_pcnt_geral <- brasil_relig_pcnt %>% 
  filter(`Brasil e Unidade da Federação` == 'brasil',
         Religião == 'Católica Apostólica Romana' |
           Religião == 'Evangélicas') %>% 
  select(Ano, Religião, Valor) %>% 
  pivot_wider(names_from = Ano, values_from = Valor) %>% 
  mutate(left_label = paste(Religião, round(`2000`, 2), '%', sep=" "), 
         right_label = paste(Religião, round(`2010`, 2), '%', sep=" "))

brasil_relig_pcnt_geral %>% 
  ggplot() +
  geom_segment(aes(x=1, xend=2, y=`2000`, yend=`2010`, colour = Religião), size = 4, alpha = .7) +
  geom_vline(xintercept=1, linetype="dashed", size=.3) + 
  geom_vline(xintercept=2, linetype="dashed", size=.3) +
  scale_color_manual(values = c("Católica Apostólica Romana"="tomato3", "Evangélicas"="dodgerblue3")) +
  labs(x="", y="Porcentagem Populacional pertencente a Religião", title = 'Religião Declarada de Acordo com a População - Brasil',
       subtitle = 'Valores Percentuais - Anos de 2000 e 2010', caption = 'Fonte: IBGE - 2000/2010\nElaboração: @gustavoovital') +  
  
  geom_text(aes(label=left_label, y= `2000`), x=rep(1, NROW(brasil_relig_pcnt_geral)), hjust=1.1, size=4.5, colour = 'gray30') +
  geom_text(aes(label=right_label, y= `2010`), x=rep(2, NROW(brasil_relig_pcnt_geral)), hjust=-0.1, size=4.5, colour = 'gray30') +
  
  geom_text(label="2000",aes( x=1, y=1.1*(max(`2000`, `2010`))), hjust=1.4, size=9) +
  geom_text(label="2010",aes( x=2, y=1.1*(max(`2000`, `2010`))), hjust=-.3, size=9) +

  xlim(0.75, 2.25) + 
  ylim(0,85) +
  theme_classic() +
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text = element_text(size = 16, colour = 'gray30'),
        plot.title = element_text(size=28, colour = 'gray30'),
        plot.subtitle = element_text(size=26, colour = 'gray30'),
        axis.title.y = element_text(size = 14, colour = 'gray30'),
        panel.border = element_blank(),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.caption = element_text(size = 14, colour = 'gray30'),
        legend.position = 'none')
  

brasil_relig_pcnt_geral_outras <- brasil_relig_pcnt %>% 
  filter(`Brasil e Unidade da Federação` == 'brasil',
         Religião != 'Católica Apostólica Romana' &
           Religião != 'Evangélicas' &
           Religião != 'Sem religião - Sem religião' &
           Religião != 'Sem religião - Ateu' &
           Religião != 'Total') %>% 
  select(Ano, Religião, Valor) %>% 
  pivot_wider(names_from = Ano, values_from = Valor) %>% 
  mutate(left_label = paste(Religião, round(`2000`, 2), '%', sep=" "), 
         right_label = paste(Religião, round(`2010`, 2), '%', sep=" "))



brasil_relig_pcnt_geral_outras %>% 
  ggplot() +
  geom_segment(aes(x=1, xend=2, y=`2000`, yend=`2010`, colour = Religião), size = 2, alpha = .7) +
  geom_vline(xintercept=1, linetype="dashed", size=.3) + 
  geom_vline(xintercept=2, linetype="dashed", size=.3) +
  scale_color_manual(values = viridis::inferno(8)) +
  labs(x="", y="Porcentagem Populacional pertencente a Religião", title = 'Religião Declarada de Acordo com a População - Brasil',
       subtitle = 'Valores Percentuais - Anos de 2000 e 2010', caption = 'Fonte: IBGE - 2000/2010\nElaboração: @gustavoovital') +  
  
  geom_text(aes(label=left_label, y= `2000`), x=rep(1, NROW(brasil_relig_pcnt_geral_outras)), hjust=1.1, size=5, colour = 'gray30') +
  geom_text(aes(label=right_label, y= `2010`), x=rep(2, NROW(brasil_relig_pcnt_geral_outras)), hjust=-0.1, size=5, colour = 'gray30') +
  
  geom_text(label="2000",aes( x=1, y=1.1*(max(`2000`, `2010`))), hjust=1.4, size=9) +
  geom_text(label="2010",aes( x=2, y=1.1*(max(`2000`, `2010`))), hjust=-.3, size=9) +
  
  
  xlim(0.75, 2.25) + 
  ylim(0,0.85) +
  theme_classic() +
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text = element_text(size = 16, colour = 'gray30'),
        plot.title = element_text(size=28, colour = 'gray30'),
        plot.subtitle = element_text(size=26, colour = 'gray30'),
        axis.title.y = element_text(size = 14, colour = 'gray30'),
        panel.border = element_blank(),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.caption = element_text(size = 14, colour = 'gray30'),
        legend.position = 'none')

# geobr ----



  
# criação de outra tabela para evangelicos ----
  
evangelicos <-
    get_sidra(api = '/t/137/n3/all/v/1000093/p/all/c133/2804,2812,95277/d/v1000093%202')[, c(4,8,10,13)]
  
  
evangelicos_clean <- evangelicos %>% 
  pivot_wider(names_from = Religião, values_from = Valor)
  
evangelicos_clean$total <- rowSums(cbind(evangelicos_clean$`Evangélica tradicional`, evangelicos_clean$Evangélicas, evangelicos_clean$`Evangélica pentecostal`), na.rm=TRUE)

evangelicos_clean <- evangelicos_clean %>% 
  select(`Unidade da Federação`, Ano, total)

evangelicos_clean$`Unidade da Federação` <- tolower(evangelicos_clean$`Unidade da Federação`)
brasil$name_state <- tolower(brasil$name_state)
brasil$name_state[18] <- 'espírito santo'

evangelicos_brasil <- left_join(evangelicos_clean, brasil, by = c('Unidade da Federação'='name_state'))


evangelicos_brasil %>% 
  ggplot() + 
  geom_sf(aes(geometry = geometry, fill=total), color="white", size=.1, show.legend = TRUE, alpha = 1) + 
  scale_fill_viridis_c(option = 'inferno', direction = -1) +
  labs(title = 'Evolução da Religião Evangélica no Brasil (1991-2010)',
       subtitle = 'Valores percentuais da população', caption = 'Fonte: IBGE\nElaboração: @gustavoovital') +
  theme_minimal() +
  facet_wrap(~Ano) +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_blank(),
        strip.text = element_text(size=18),
        plot.title = element_text(size=28, colour = 'gray30'),
        plot.subtitle = element_text(size=26, colour = 'gray30'),
        plot.caption = element_text(size=16, colour = 'gray30'),
        legend.key.size = unit(1, "cm"))

#  instrução ----

inst <- 
  get_sidra(api = '/t/3457/n1/all/v/allxp/p/all/c1568/0,9493,9494,9495,99713/c2/0/c133/0,2826,2827,2836,95263,95277/d/v1643%200')[, c(10,14,17)]

inst <- inst %>% 
  pivot_wider(names_from = `Nível de instrução`, values_from = Valor) %>% 
  mutate(`Sem Instrução` = `Sem instrução e fundamental incompleto`/Total * 100,
         `Fundamental Completo` = `Fundamental completo e médio incompleto`/Total * 100,
         `Médio Completo` = `Médio completo e superior incompleto`/Total * 100,
         `Superior Completo` = `Superior completo`/Total * 100) %>%  
  select(Religião, `Sem Instrução`, `Fundamental Completo`, `Médio Completo`, `Superior Completo`) %>% 
  pivot_longer(-Religião, names_to = 'Escolaridade', values_to = 'Valor')  
  
inst$Escolaridade <- factor(inst$Escolaridade, levels = c('Sem Instrução', 'Fundamental Completo', 'Médio Completo', 'Superior Completo'))

inst %>% 
  ggplot(aes(Escolaridade, Valor, fill = Religião, label = paste(round(Valor, 1), '%'))) +
  geom_col(position = 'dodge', colour = 'black', size = .3, alpha = .7) +
  geom_text(position = position_dodge(width = .9), alpha = 1, vjust = 3, colour = 'white', size = 5, family = 'bold') +
  scale_fill_manual(values = viridis::plasma(8),
                    labels = c('Total'='População Brasileira')) +
  scale_y_continuous(limits = c(0, 55),
                     breaks = seq(0,55,5)) +
  labs(title = 'Comparação de Escolaridade: Religiões VS População Brasileira - 2010',
       subtitle = 'Valores Percentuais', caption = 'Fonte: IBGE\nElaboração: @gustavoovital') +
  #facet_wrap(~Escolaridade) +
  theme_minimal() +
  theme(axis.title=element_blank(),
        axis.text=element_text(size = 16, colour = 'gray30'),
        #axis.ticks=element_blank(),
        #panel.grid.major = element_blank(), 
        #panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_blank(),
        strip.text = element_text(size=18),
        plot.title = element_text(size=28, colour = 'gray30'),
        plot.subtitle = element_text(size=26, colour = 'gray30'),
        plot.caption = element_text(size=16, colour = 'gray30'),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size=16, colour = 'gray30')
        #axis.text.x = element_blank()
        )
  

# rendimento medio ----

rendimento <-
  get_sidra(api = '/t/3459/n1/all/v/allxp/p/all/c133/2826,2827,2836,95263,95277/c2/0/c11913/0/c11915/0,96179,96180,96181,96182,99824,99825,99826,99827,99828,100434/d/v916%200')[, c(10,16,19)]

rendimento_pct <- rendimento %>% 
  pivot_wider(names_from = `Classes de rendimento nominal mensal de todos os trabalhos`, values_from = Valor) %>% 
  mutate(`Até 1/2 salário mínimo` = `Até 1/2 salário mínimo`/Total * 100,
         `Mais de 1 a 2 salários mínimos` = `Mais de 1 a 2 salários mínimos`/Total * 100,
         `Mais de 2 a 3 salários mínimos` = `Mais de 2 a 3 salários mínimos`/Total * 100,
         `Mais de 3 a 5 salários mínimos` = `Mais de 3 a 5 salários mínimos`/Total * 100,
         `Mais de 5 a 10 salários mínimos` = `Mais de 5 a 10 salários mínimos`/Total * 100,
         `Mais de 1/2 a 1 salário mínimo` = `Mais de 1/2 a 1 salário mínimo`/Total * 100,
         `Mais de 10 a 15 salários mínimos` = `Mais de 10 a 15 salários mínimos`/Total * 100,
         `Mais de 20 a 30 salários mínimos` = `Mais de 20 a 30 salários mínimos`/Total * 100,
         `Mais de 30 salários mínimos` = `Mais de 30 salários mínimos`/Total * 100,
         `Mais de 15 a 20 salários mínimos` = `Mais de 15 a 20 salários mínimos`/Total * 100) %>% 
  select(-Total) %>% 
  pivot_longer(-Religião, names_to = 'Rendimento nominal mensal de todos os trabalhos', values_to = 'Valor')

  
rendimento_pct$`Rendimento nominal mensal de todos os trabalhos` <-
  factor(rendimento_pct$`Rendimento nominal mensal de todos os trabalhos`, levels = c('Até 1/2 salário mínimo',
                                                                                      'Mais de 1/2 a 1 salário mínimo',
                                                                                      'Mais de 1 a 2 salários mínimos',
                                                                                      'Mais de 2 a 3 salários mínimos',
                                                                                      'Mais de 3 a 5 salários mínimos',
                                                                                      'Mais de 5 a 10 salários mínimos',
                                                                                      'Mais de 10 a 15 salários mínimos',
                                                                                      'Mais de 15 a 20 salários mínimos',
                                                                                      'Mais de 20 a 30 salários mínimos',
                                                                                      'Mais de 30 salários mínimos'))

rendimento_pct %>% 
  ggplot(aes(`Rendimento nominal mensal de todos os trabalhos`, Valor, fill = Religião)) +
  geom_col(position = 'dodge', colour = 'black', size = 1.2, alpha = .65) +
  scale_fill_manual(values = viridis::viridis(7), name = '') +
  facet_wrap(~`Rendimento nominal mensal de todos os trabalhos`, scales ='free') +
  labs(x = NULL, y = '(%)', title = 'Rendimento nominal mensal de todos os trabalhos - 2010',
       subtitle = 'Por Religião - Valores percentuais', caption = 'Fonte: IBGE\nElaboração: @gustavoovital') +
  theme_minimal() +
  theme(legend.position = 'bottom',
        legend.justification = c(0, 0),
        legend.key.size = unit(2, "cm"),
        legend.text = element_text(size=16, colour = 'gray30'),
        strip.text = element_text(size=18, colour = 'gray30'),
        plot.title = element_text(size=28, colour = 'gray30'),
        plot.subtitle = element_text(size=26, colour = 'gray30'),
        plot.caption = element_text(size=16, colour = 'gray30'),
        axis.text.x = element_blank(),
        axis.text=element_text(size = 16, colour = 'gray30'),
        axis.text.y = element_text(size = 16, colour = 'gray30'))

rendimento_pct %>% 
  filter(Religião == 'Espírita') 
  

rendimento %>% 
  filter(Religião == 'Evangélicas' &
           `Classes de rendimento nominal mensal de todos os trabalhos` != 'Total') %>% 
  summarise(sum(Valor))
# idade ====

idade <- 
  get_sidra(api = '/t/2103/n1/all/v/allxp/p/last%201/c1/0/c2/0/c58/0,1143,1144,1145,3299,6798,100402,104895/c133/2826,2827,2836,95263,95277/d/v93%200')[, c(14, 16, 19)]

idade_pct <- idade %>% 
  pivot_wider(names_from = `Grupo de idade`, values_from = Valor) %>%
  mutate(`15 a 19 anos` = `15 a 19 anos`/Total * 100,
         `20 a 24 anos` = `20 a 24 anos`/Total * 100,
         `25 a 29 anos` = `25 a 29 anos`/Total * 100,
         `30 a 39 anos` = `30 a 39 anos`/Total * 100,
         `65 anos ou mais` = `65 anos ou mais`/Total * 100,
         `0 a 14 anos` = `0 a 14 anos`/Total * 100,
         `40 a 64 anos` = `40 a 64 anos`/Total * 100) %>% 
  select(-Total) %>% 
  pivot_longer(-Religião, names_to = 'Grupo de Idade', values_to = 'Valor')

idade_pct$`Grupo de Idade` <- factor(idade_pct$`Grupo de Idade`, levels = c('0 a 14 anos', '15 a 19 anos', '20 a 24 anos', '25 a 29 anos', '30 a 39 anos', '40 a 64 anos', '65 anos ou mais'))
  
idade_pct %>% 
  ggplot(aes(`Grupo de Idade`, Valor, fill = Valor, label = paste(round(Valor, 2), '%'))) +
  geom_col(colour = 'black') +
  coord_polar() +
  geom_label_repel(fill = 'white', alpha = 1, vjust = 1, colour = 'gray30', size = 5, family = 'bold' ) +
  scale_fill_viridis_c(option = 'inferno', direction = -1) +
  labs(title = 'Religiões de Acordo com a Idade - 2010', subtitle = 'Valores Percentuais', y = NULL, x = NULL,
       caption = 'Fonte: IBGE\nElaboração: @gustavoovital') +
  facet_wrap(~Religião) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_blank(),
        legend.text = element_text(size=16, colour = 'gray30'),
        strip.text = element_text(size=18, colour = 'gray30'),
        plot.title = element_text(size=28, colour = 'gray30'),
        plot.subtitle = element_text(size=26, colour = 'gray30'),
        plot.caption = element_text(size=16, colour = 'gray30'),
        legend.position = 'none')

# comparação igrejas ----

igrejas <-
  read.csv('igrejas.csv')

igrejas %>% 
  head()

soma <- sum(igrejas$Valor)  
igrejas$Valor <- igrejas$Valor/soma

igrejas %>% 
  ggplot(aes(area = Valor, fill = Tipo, label = Nome, subgroup = Nome)) +
  geom_treemap(alpha = .4, size = 2, colour = 'gray25') +
  geom_treemap_subgroup_border(colour = 'gray30', size = 1) +
  geom_treemap_text(colour = "gray12", place = "centre", grow = TRUE) + 
  labs(title = 'Composição do Cenário de Igrejas Evangélicas no Brasil - 2010',
       subtitle = 'Valores percentuais', caption = 'Fonte: IBGE\nElaboração: @gustavoovital') +
  scale_fill_manual(values = viridis::viridis(4), name = NULL) +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_blank(),
        legend.key.size = unit(2, "cm"),
        legend.text = element_text(size=16, colour = 'gray30'),
        strip.text = element_text(size=18, colour = 'gray30'),
        plot.title = element_text(size=28, colour = 'gray30'),
        plot.subtitle = element_text(size=26, colour = 'gray30'),
        plot.caption = element_text(size=16, colour = 'gray30'),
        legend.position = 'bottom')

igrejas %>% 
  ggplot(aes(reorder(Nome, Valor), Valor, label = paste(round(Valor, 3) * 100, '%'), fill = Tipo)) +
  geom_col(alpha = .73) +
  geom_text(position = position_dodge(width = .9), alpha = 1, vjust = 0.5, hjust = -.1,  colour = 'gray20', size = 5, family = 'bold') +
  scale_fill_manual(values = viridis::inferno(4), name = NULL) +
  labs(title = 'Composição do Cenário de Igrejas Evangélicas no Brasil - 2010',
       subtitle = 'Valores percentuais', caption = 'Fonte: IBGE\nElaboração: @gustavoovital', x = NULL, y = NULL) +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 14),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_line(size = 4),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size=16, colour = 'gray30'),
        strip.text = element_text(size=18, colour = 'gray30'),
        plot.title = element_text(size=28, colour = 'gray30'),
        plot.subtitle = element_text(size=26, colour = 'gray30'),
        plot.caption = element_text(size=16, colour = 'gray30'),
        legend.position = 'bottom')
