# script para analise de dados em segurança publica no rj
#
# autor: gustavovital@id.uff.br

# wd ====

setwd("/media/ragnar/Seagate Expansion Drive/tidyverse/Segurança Pública")

# pacotes ====

library(tidyverse)
library(geobr)
library(ggthemes)
library(patchwork)
library(wesanderson)
library(lubridate)
library(gganimate)
library(viridis)

# base de dados ====

cisp <- read_csv2('BaseDPEvolucaoMensalCisp.csv', locale = locale(encoding = "latin1"))
pop <- read_csv('munic_pop.csv')
pop <- pop %>% 
  pivot_longer(-munic, names_to = 'data', values_to = 'pop')

pop$data <- as.Date(pop$data)

# manipulação de dados ====

# região do Grande RJ ====

colnames(cisp)

geral <- cisp %>% 
  filter(munic == 'Belford Roxo' |
           munic == 'Duque de Caxias' |
           munic == 'Guapimirim' |
           munic == 'Itaboraí' |
           munic == 'Japeri' |
           munic == 'Magé' |
           munic == 'Maricá' |
           munic == 'Mesquita' |
           munic == 'Nilópolis' |
           munic == 'Niterói' |
           munic == 'Nova Iguaçu' |
           munic == 'Queimados' |
           munic == 'Rio de Janeiro' |
           munic == 'São Gonçalo' |
           munic == 'São João de Meriti' |
           munic == 'Tanguá') %>% 
  mutate(data = as.Date(paste(vano, mes, '01', sep='-'))) %>% 
  mutate(policiais_mortos = pol_militares_mortos_serv + pol_civis_mortos_serv) %>% 
  group_by(data, munic) %>% 
  summarise(n_doloso = sum(hom_doloso),
            n_latrocinio = sum(latrocinio),
            n_estupro = sum(estupro),
            n_policiais = sum(policiais_mortos),
            n_roubos = sum(total_roubos),
            n_posse = sum(apreensao_drogas)) %>% 
  mutate(prefeito = ifelse(data < as.Date('2007-01-01'), 'Rosinha Garotinho', 
                           ifelse(data >= as.Date('2007-01-01') & data < as.Date('2014-04-04'), 'Sérgio Cabral',
                                  ifelse(data >= as.Date('2014-04-04') & data < as.Date('2019-01-01'), 'Fernando Pezão',
                                         'Wilson Witzel'))))


geral$prefeito <- factor (geral$prefeito, levels = c('Rosinha Garotinho', 'Sérgio Cabral','Fernando Pezão', 'Wilson Witzel'))


geral <- left_join(geral, pop, by = c('munic' = 'munic', 'data' = 'data'))


# PRIMEIRO QUADRO  - RIO DE JANEIRO ====

# DOT PLOT ====
# HOMICIDIOS DOLOSOS ====

rj_h_d <- geral %>% 
  filter(munic == 'Rio de Janeiro') %>% 
  ggplot(aes(x = data, y = n_doloso, colour = prefeito)) +
  geom_smooth(se = FALSE, size = .7) +
  scale_colour_manual(values = wes_palette("GrandBudapest1", n = 4),
                      name = NULL) +
  geom_point(size = 5, alpha = .55) +
  scale_y_continuous(limits = c(70, 260),
                     breaks = seq(70, 260, 20)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%b-%y") +
  labs(subtitle = 'Número de Homicídios Dolosos por Mês',
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(legend.position = 'top',
        plot.subtitle = element_text(size = 14, colour = 'gray50'),
        axis.text.x=element_text(angle = 30, hjust = 1, colour = 'gray50'),
        axis.text.y = element_text(colour = 'gray50'))

# ESTUPROS ====

rj_e_d <- geral %>% 
  filter(munic == 'Rio de Janeiro') %>% 
  ggplot(aes(x = data, y = n_estupro, colour = prefeito)) +
  geom_smooth(se = FALSE, size = .7) +
  scale_colour_manual(values = wes_palette("GrandBudapest1", n = 4),
                      name = NULL) +
  geom_point(size = 2, alpha = .55) +
  scale_y_continuous(limits = c(50, 240),
                     breaks = seq(50, 250, 20)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%b-%y") +
  labs(subtitle = 'Número de Estupros Reportados por Mês',
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(legend.position = 'none',
        plot.subtitle = element_text(size = 14, colour = 'gray50'),
        axis.text.x=element_text(angle = 30, hjust = 1, colour = 'gray50'),
        axis.text.y = element_text(colour = 'gray50'))

# APREENSAO ====

rj_d_d <- 
  geral %>% 
  filter(munic == 'Rio de Janeiro') %>% 
  ggplot(aes(x = data, y = n_roubos, colour = prefeito)) +
  geom_smooth(se = FALSE, size = .7) +
  scale_colour_manual(values = wes_palette("GrandBudapest1", n = 4),
                      name = NULL) +
  geom_point(size = 2, alpha = .55) +
  scale_y_continuous(limits = c(4000, 13000),
                     breaks = seq(4000, 13000, 1000)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%b-%y") +
  labs(subtitle = 'Número Total de Roubos por Mês',
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(legend.position = 'none',
        plot.subtitle = element_text(size = 14, colour = 'gray50'),
        axis.text.x=element_text(angle = 30, hjust = 1, colour = 'gray50'),
        axis.text.y = element_text(colour = 'gray50'))


# BOX PLOT ====


# HOMICIDIOS DOLOSOS ====

rj_h_b <- geral %>% 
  filter(munic == 'Rio de Janeiro') %>% 
  ggplot(aes(x = prefeito, y = n_doloso, fill = prefeito)) +
  geom_violin(size = .5, alpha = .4) +
  scale_fill_manual(values = wes_palette("GrandBudapest1", n = 4),
                    name = NULL) +
  scale_y_continuous(limits = c(70, 260),
                     breaks = seq(70, 260, 20)) +
  labs(subtitle = 'Número de Homicídios Dolosos por Mês',
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(legend.position = 'none',
        plot.subtitle = element_text(size = 14, colour = 'gray50'),
        axis.text.x=element_text(colour = 'gray50', size = 10),
        axis.text.y = element_text(colour = 'gray50'))

# ESTUPROS ====

rj_e_b <- geral %>% 
  filter(munic == 'Rio de Janeiro') %>% 
  ggplot(aes(x = prefeito, y = n_estupro, fill = prefeito)) +
  geom_violin(size = .5, alpha = .4) +
  scale_fill_manual(values = wes_palette("GrandBudapest1", n = 4),
                    name = NULL) +
  scale_y_continuous(limits = c(50, 240),
                     breaks = seq(50, 240, 20)) +
  labs(subtitle = 'Número de Estupros Reportados por Mês',
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(legend.position = 'none',
        plot.subtitle = element_text(size = 14, colour = 'gray50'),
        axis.text.x=element_text(colour = 'gray50', size = 10),
        axis.text.y = element_text(colour = 'gray50'))


# APREENSAO ====

rj_d_b <- 
  geral %>% 
  filter(munic == 'Rio de Janeiro') %>% 
  ggplot(aes(x = prefeito, y = n_roubos, fill = prefeito)) +
  geom_violin(size = .5, alpha = .4) +
  scale_fill_manual(values = wes_palette("GrandBudapest1", n = 4),
                    name = NULL) +
  scale_y_continuous(limits = c(4000, 13000),
                     breaks = seq(4000, 13000, 1000)) +
  labs(subtitle = 'Número Total de Roubos por Mês',
       x = NULL, y = NULL) + 
  theme_minimal() +
  theme(legend.position = 'none',
        plot.subtitle = element_text(size = 14, colour = 'gray50'),
        axis.text.x=element_text(colour = 'gray50', size = 10),
        axis.text.y = element_text(colour = 'gray50'))

# plot RJ ====

patch <- rj_h_d + rj_e_d + rj_d_d + rj_h_b + rj_e_b + rj_d_b 

patch + plot_layout(design = 'AAB
                              AAC
                              DEF')  +  
  plot_annotation(title = 'Resultados/Indicadores de criminalidade na cidade do Rio de Janeiro',
                  subtitle = 'Dados para o período 2003-2019. Tipos de crime selecionados pelo autor',
                  caption = 'Fonte:ISP-RJ.\nElaboração: @gustavoovital') & 
  theme(text = element_text(size = 15, 'bold'))

# NITEROI ====


# HOMICIDIOS DOLOSOS ====

nit_h_d <- geral %>% 
  filter(munic == 'Niterói') %>% 
  ggplot(aes(x = data, y = n_doloso, colour = prefeito)) +
  geom_smooth(se = FALSE, size = .7) +
  scale_colour_manual(values = wes_palette("Rushmore1", n = 4),
                      name = NULL) +
  geom_point(size = 5, alpha = .55) +
  scale_y_continuous(limits = c(0, 35),
                     breaks = seq(0, 35, 5)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%b-%y") +
  labs(subtitle = 'Número de Homicídios Dolosos por Mês',
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(legend.position = 'top',
        plot.subtitle = element_text(size = 14, colour = 'gray50'),
        axis.text.x=element_text(angle = 30, hjust = 1, colour = 'gray50'),
        axis.text.y = element_text(colour = 'gray50'))

# ESTUPROS ====

nit_e_d <- geral %>% 
  filter(munic == 'Niterói') %>% 
  ggplot(aes(x = data, y = n_estupro, colour = prefeito)) +
  geom_smooth(se = FALSE, size = .7) +
  scale_colour_manual(values = wes_palette("Rushmore1", n = 4),
                      name = NULL) +
  geom_point(size = 2, alpha = .55) +
  scale_y_continuous(limits = c(0, 25),
                     breaks = seq(0, 25, 5)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%b-%y") +
  labs(subtitle = 'Número de Estupros Reportados por Mês',
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(legend.position = 'none',
        plot.subtitle = element_text(size = 14, colour = 'gray50'),
        axis.text.x=element_text(angle = 30, hjust = 1, colour = 'gray50'),
        axis.text.y = element_text(colour = 'gray50'))

# APREENSAO ====

nit_d_d <- 
  geral %>% 
  filter(munic == 'Niterói') %>% 
  ggplot(aes(x = data, y = n_roubos, colour = prefeito)) +
  geom_smooth(se = FALSE, size = .7) +
  scale_colour_manual(values = wes_palette("Rushmore1", n = 4),
                      name = NULL) +
  geom_point(size = 2, alpha = .55) +
  scale_y_continuous(limits = c(200, 1000),
                     breaks = seq(200, 1000, 100)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%b-%y") +
  labs(subtitle = 'Número Total de Roubos por Mês',
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(legend.position = 'none',
        plot.subtitle = element_text(size = 14, colour = 'gray50'),
        axis.text.x=element_text(angle = 30, hjust = 1, colour = 'gray50'),
        axis.text.y = element_text(colour = 'gray50'))


# BOX PLOT ====


# HOMICIDIOS DOLOSOS ====

nit_h_b <- 
  geral %>% 
  filter(munic == 'Niterói') %>% 
  ggplot(aes(x = prefeito, y = n_doloso, fill = prefeito)) +
  geom_violin(size = .5, alpha = .4) +
  scale_fill_manual(values = wes_palette("Rushmore1", n = 4),
                    name = NULL) +
  scale_y_continuous(limits = c(0, 35),
                     breaks = seq(0, 35, 5)) +
  labs(subtitle = 'Número de Homicídios Dolosos por Mês',
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(legend.position = 'none',
        plot.subtitle = element_text(size = 14, colour = 'gray50'),
        axis.text.x=element_text(colour = 'gray50', size = 10),
        axis.text.y = element_text(colour = 'gray50'))

# ESTUPROS ====

nit_e_b <- geral %>% 
  filter(munic == 'Niterói') %>% 
  ggplot(aes(x = prefeito, y = n_estupro, fill = prefeito)) +
  geom_violin(size = .5, alpha = .4) +
  scale_fill_manual(values = wes_palette("Rushmore1", n = 4),
                    name = NULL) +
  scale_y_continuous(limits = c(0, 25),
                     breaks = seq(0, 25, 5)) +
  labs(subtitle = 'Número de Estupros Reportados por Mês',
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(legend.position = 'none',
        plot.subtitle = element_text(size = 14, colour = 'gray50'),
        axis.text.x=element_text(colour = 'gray50', size = 10),
        axis.text.y = element_text(colour = 'gray50'))


# APREENSAO ====

nit_d_b <- geral %>% 
  filter(munic == 'Niterói') %>% 
  ggplot(aes(x = prefeito, y = n_roubos, fill = prefeito)) +
  geom_violin(size = .5, alpha = .4) +
  scale_fill_manual(values = wes_palette("Rushmore1", n = 4),
                    name = NULL) +
  scale_y_continuous(limits = c(200, 1000),
                     breaks = seq(200, 1000, 100)) +
  labs(subtitle = 'Número Total de Roubos por Mês',
       x = NULL, y = NULL) + 
  theme_minimal() +
  theme(legend.position = 'none',
        plot.subtitle = element_text(size = 14, colour = 'gray50'),
        axis.text.x=element_text(colour = 'gray50', size = 10),
        axis.text.y = element_text(colour = 'gray50'))

# plot nit ====

patch <- nit_h_d + nit_e_d + nit_d_d + nit_h_b + nit_e_b + nit_d_b 

patch + plot_layout(design = 'AAB
                              AAC
                              DEF')  +  
  plot_annotation(title = 'Resultados/Indicadores de criminalidade na cidade de Niterói',
                  subtitle = 'Dados para o período 2003-2019. Tipos de crime selecionados pelo autor',
                  caption = 'Fonte:ISP-nit.\nElaboração: @gustavoovital') & 
  theme(text = element_text(size = 15, 'bold'))

# SÃO GONÇALO ====


# HOMICIDIOS DOLOSOS ====

sg_h_d <- 
  geral %>% 
  filter(munic == 'São Gonçalo') %>% 
  ggplot(aes(x = data, y = n_doloso, colour = prefeito)) +
  geom_smooth(se = FALSE, size = .7) +
  scale_colour_manual(values = wes_palette("Cavalcanti1", n = 4),
                      name = NULL) +
  geom_point(size = 5, alpha = .55) +
  scale_y_continuous(limits = c(5, 65),
                     breaks = seq(5, 65, 10)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%b-%y") +
  labs(subtitle = 'Número de Homicídios Dolosos por Mês',
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(legend.position = 'top',
        plot.subtitle = element_text(size = 14, colour = 'gray50'),
        axis.text.x=element_text(angle = 30, hjust = 1, colour = 'gray50'),
        axis.text.y = element_text(colour = 'gray50'))

# ESTUPROS ====

sg_e_d <- 
  geral %>% 
  filter(munic == 'São Gonçalo') %>% 
  ggplot(aes(x = data, y = n_estupro, colour = prefeito)) +
  geom_smooth(se = FALSE, size = .7) +
  scale_colour_manual(values = wes_palette("Cavalcanti1", n = 4),
                      name = NULL) +
  geom_point(size = 2, alpha = .55) +
  scale_y_continuous(limits = c(0, 45),
                     breaks = seq(0, 45, 5)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%b-%y") +
  labs(subtitle = 'Número de Estupros Reportados por Mês',
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(legend.position = 'none',
        plot.subtitle = element_text(size = 14, colour = 'gray50'),
        axis.text.x=element_text(angle = 30, hjust = 1, colour = 'gray50'),
        axis.text.y = element_text(colour = 'gray50'))

# APREENSAO ====

sg_d_d <- 
  geral %>% 
  filter(munic == 'São Gonçalo') %>% 
  ggplot(aes(x = data, y = n_roubos, colour = prefeito)) +
  geom_smooth(se = FALSE, size = .7) +
  scale_colour_manual(values = wes_palette("Cavalcanti1", n = 4),
                      name = NULL) +
  geom_point(size = 2, alpha = .55) +
  scale_y_continuous(limits = c(250, 2500),
                     breaks = seq(250, 2500, 250)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%b-%y") +
  labs(subtitle = 'Número Total de Roubos por Mês',
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(legend.position = 'none',
        plot.subtitle = element_text(size = 14, colour = 'gray50'),
        axis.text.x=element_text(angle = 30, hjust = 1, colour = 'gray50'),
        axis.text.y = element_text(colour = 'gray50'))


# BOX PLOT ====


# HOMICIDIOS DOLOSOS ====

sg_h_b <- 
  geral %>% 
  filter(munic == 'São Gonçalo') %>% 
  ggplot(aes(x = prefeito, y = n_doloso, fill = prefeito)) +
  geom_violin(size = .5, alpha = .4) +
  scale_fill_manual(values = wes_palette("Cavalcanti1", n = 4),
                    name = NULL) +
  scale_y_continuous(limits = c(5, 65),
                     breaks = seq(5, 65, 10)) +
  labs(subtitle = 'Número de Homicídios Dolosos por Mês',
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(legend.position = 'none',
        plot.subtitle = element_text(size = 14, colour = 'gray50'),
        axis.text.x=element_text(colour = 'gray50', size = 10),
        axis.text.y = element_text(colour = 'gray50'))

# ESTUPROS ====

sg_e_b <- geral %>% 
  filter(munic == 'São Gonçalo') %>% 
  ggplot(aes(x = prefeito, y = n_estupro, fill = prefeito)) +
  geom_violin(size = .5, alpha = .4) +
  scale_fill_manual(values = wes_palette("Cavalcanti1", n = 4),
                    name = NULL) +
  scale_y_continuous(limits = c(0, 45),
                     breaks = seq(0, 45, 5)) +
  labs(subtitle = 'Número de Estupros Reportados por Mês',
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(legend.position = 'none',
        plot.subtitle = element_text(size = 14, colour = 'gray50'),
        axis.text.x=element_text(colour = 'gray50', size = 10),
        axis.text.y = element_text(colour = 'gray50'))


# APREENSAO ====

sg_d_b <- geral %>% 
  filter(munic == 'São Gonçalo') %>% 
  ggplot(aes(x = prefeito, y = n_roubos, fill = prefeito)) +
  geom_violin(size = .5, alpha = .4) +
  scale_fill_manual(values = wes_palette("Cavalcanti1", n = 4),
                    name = NULL) +
  scale_y_continuous(limits = c(250, 2500),
                     breaks = seq(250, 2500, 250)) +
  labs(subtitle = 'Número Total de Roubos por Mês',
       x = NULL, y = NULL) + 
  theme_minimal() +
  theme(legend.position = 'none',
        plot.subtitle = element_text(size = 14, colour = 'gray50'),
        axis.text.x=element_text(colour = 'gray50', size = 10),
        axis.text.y = element_text(colour = 'gray50'))

# plot sg ====

patch <- sg_h_d + sg_e_d + sg_d_d + sg_h_b + sg_e_b + sg_d_b 

patch + plot_layout(design = 'AAB
                              AAC
                              DEF')  +  
  plot_annotation(title = 'Resultados/Indicadores de criminalidade na cidade de São Gonçalo',
                  subtitle = 'Dados para o período 2003-2019. Tipos de crime selecionados pelo autor',
                  caption = 'Fonte:ISP-sg.\nElaboração: @gustavoovital') & 
  theme(text = element_text(size = 15, 'bold'))
# Graficos gerais ====

graficos_gerais <-
  geral %>% 
  mutate(ano = year(data)) %>% 
  group_by(munic, ano, pop) %>%  
  summarise(estupro = sum(n_estupro),
            roubos = sum(n_roubos),
            latrocinio = sum(n_latrocinio),
            homicidio = sum(n_doloso)) %>% 
  mutate(estupro_ponderado = estupro*100000/pop,
         roubos_ponderado = roubos*100000/pop,
         latrocinio_ponderado = latrocinio*100000/pop,
         homicidio_ponderado = homicidio*100000/pop)

# estupro ====

graficos_gerais %>% 
  ggplot(aes(ano, estupro_ponderado, fill = munic)) +
  geom_col(size = 8, alpha = .6) +
  geom_smooth(method = 'lm', se = FALSE, colour = 'black', linetype = 'dashed', size = .5) +
  scale_x_continuous(limits = c(2002,2020),
                     breaks = seq(2002, 2022, 2)) +
  labs(title = 'Número de Estupros Reportados a cada 100.000 Habitantes',
       subtitle = 'Dados para o período 2003-2019. Microrregião do Rio de Janeiro',
       x = NULL, y = NULL, caption = 'Fonte:ISP-sg.\nElaboração: @gustavoovital') +
  scale_fill_manual(values = inferno(30)[1:16], name = '') +
  facet_wrap(~munic) +
  theme_minimal() +
  theme(legend.position = 'none',
        plot.subtitle = element_text(size = 16, colour = 'gray40'),
        plot.title = element_text(size = 20, colour = 'gray40'),
        axis.text.x=element_text(colour = 'gray40', size = 10),
        axis.text.y = element_text(colour = 'gray40'),
        strip.text.x = element_text(size = 16, color = "gray40"),
        plot.caption = element_text(size = 16, color = "gray40"))

# homicidios ===

graficos_gerais %>% 
  ggplot(aes(ano, homicidio_ponderado, fill = munic)) +
  geom_col(size = 8, alpha = .6) +
  geom_smooth(method = 'lm', se = FALSE, colour = 'black', linetype = 'dashed', size = .5) +
  scale_x_continuous(limits = c(2002,2020),
                     breaks = seq(2002, 2022, 2)) +
  labs(title = 'Número de Homicídios Dolosos a cada 100.000 Habitantes',
       subtitle = 'Dados para o período 2003-2019. Microrregião do Rio de Janeiro',
       x = NULL, y = NULL, caption = 'Fonte:ISP-sg.\nElaboração: @gustavoovital') +
  scale_fill_manual(values = inferno(50)[1:16], name = '') +
  facet_wrap(~munic) +
  theme_minimal() +
  theme(legend.position = 'none',
        plot.subtitle = element_text(size = 16, colour = 'gray40'),
        plot.title = element_text(size = 20, colour = 'gray40'),
        axis.text.x=element_text(colour = 'gray40', size = 10),
        axis.text.y = element_text(colour = 'gray40'),
        strip.text.x = element_text(size = 16, color = "gray40"),
        plot.caption = element_text(size = 16, color = "gray40"))

graficos_gerais %>% 
  ggplot(aes(ano, roubos_ponderado, fill = munic)) +
  geom_col(size = 8, alpha = .6) +
  geom_smooth(method = 'lm', se = FALSE, colour = 'black', linetype = 'dashed', size = .5) +
  scale_x_continuous(limits = c(2002,2020),
                     breaks = seq(2002, 2022, 2)) +
  labs(title = 'Número de Roubos Totais a cada 100.000 Habitantes',
       subtitle = 'Dados para o período 2003-2019. Microrregião do Rio de Janeiro',
       x = NULL, y = NULL, caption = 'Fonte:ISP-sg.\nElaboração: @gustavoovital') +
  scale_fill_manual(values = inferno(30)[1:16], name = '') +
  facet_wrap(~munic) +
  theme_minimal() +
  theme(legend.position = 'none',
        plot.subtitle = element_text(size = 16, colour = 'gray40'),
        plot.title = element_text(size = 20, colour = 'gray40'),
        axis.text.x=element_text(colour = 'gray40', size = 10),
        axis.text.y = element_text(colour = 'gray40'),
        strip.text.x = element_text(size = 16, color = "gray40"),
        plot.caption = element_text(size = 16, color = "gray40"))

# latrocinio 

graficos_gerais %>% 
  ggplot(aes(ano, latrocinio_ponderado, fill = munic)) +
  geom_col(size = 8, alpha = .6) +
  geom_smooth(method = 'lm', se = FALSE, colour = 'black', linetype = 'dashed', size = .5) +
  scale_x_continuous(limits = c(2002,2020),
                     breaks = seq(2002, 2022, 2)) +
  labs(title = 'Número de Latrocínios a cada 100.000 Habitantes',
       subtitle = 'Dados para o período 2003-2019. Microrregião do Rio de Janeiro',
       x = NULL, y = NULL, caption = 'Fonte:ISP-sg.\nElaboração: @gustavoovital') +
  scale_fill_manual(values = inferno(30)[1:16], name = '') +
  facet_wrap(~munic) +
  theme_minimal() +
  theme(legend.position = 'none',
        plot.subtitle = element_text(size = 16, colour = 'gray40'),
        plot.title = element_text(size = 20, colour = 'gray40'),
        axis.text.x=element_text(colour = 'gray40', size = 10),
        axis.text.y = element_text(colour = 'gray40'),
        strip.text.x = element_text(size = 16, color = "gray40"),
        plot.caption = element_text(size = 16, color = "gray40"))
