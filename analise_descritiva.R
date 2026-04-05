########################################################################
## Description: Análise Exploratória de Dados - Buscando conhecimento -
## Parte II
##
## Maintainer: UNIVALI / EP / Núcleo de Disciplinas da EP
## Author: Rodrigo Sant'Ana
## Created: ter abr  1 15:38:13 2025 (-0300)
## Version: 0.0.1
##
## URL:
## Doc URL:
##
## Database info:
##
### Commentary:
##
### Code:
########################################################################

########################################################################
######@> 01. Instalando e Carregando pacotes / bibliotecas R...

######@> Instalação de novos pacotes / bibliotecas [Instalação de
######@> Pacotes precisa se executada apenas uma vez]...
install.packages(c("dplyr", "tidyr", "lubridate", "readxl", "ggplot2",
                   "patchwork", "sf", "ggcorrplot", "ggmap", "leaflet",
                   "Hmisc", "imputeTS", "stringr", "DataExplorer",
                   "explore"),
                 dependencies = TRUE)

######@> Carregando os pacotes para uso [Carregamento dos pacotes
######@> precisa ser feito sempre que quiser usar o mesmo]...
library(Hmisc)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(DataExplorer)
library(readxl)
library(ggplot2)
library(patchwork)
library(sf)
library(ggcorrplot)
library(ggmap)
library(leaflet)
library(imputeTS)
library(explore)

########################################################################
######@> 02. Importando a base de dados...

######@> Base de dados...
dados <- read_excel("Rio_Itajai_Estuario_ver00.xlsx",
                    sheet = 1)

########################################################################
######@> 03. Faxinando os dados...

######@> Corrigindo os problemas na variável "Local"...

#####@> Verificando os casos únicos...
table(dados$Local)

#####@> Corrigindo...
dados$Local <- ifelse(test = dados$Local %in%
                          c("Estuário", "estuario", "estuário"),
                      yes = "Estuário",
                      no = "Adjacência")

#####@> Verificando novamente os casos únicos para ver se resolvemos os
#####@> problemas...
table(dados$Local)

######@> Corrigindo os problemas na variável "Mês"...

#####@> Verificando os casos únicos...
table(dados$Mês)

#####@> Corrigindo...
dados$Mês <- str_to_title(dados$Mês)

#####@> Verificando novamente os casos únicos para ver se resolvemos os
#####@> problemas...
table(dados$Mês)

######@> Corrigindo os problemas na variável "Draga"...

#####@> Verificando os casos únicos...
table(dados$Draga)

#####@> Corrigindo...
dados$Draga <- str_to_title(dados$Draga)

#####@> Verificando novamente os casos únicos para ver se resolvemos os
#####@> problemas...
table(dados$Draga)

#####@> Corrigindo 02...
dados$Draga <- ifelse(test = dados$Draga == "Nao",
                      yes = "Não",
                      no = dados$Draga)

#####@> Verificando novamente os casos únicos para ver se resolvemos os
#####@> problemas...
table(dados$Draga)

########################################################################
######@> 04. Explorando a base de dados...

######@> Avaliando as variáveis físico-químicas individualmente...

#####@> Temperatura...

####@> média, desvio padrão e quantis da distribuição...
c("Média" = mean(dados$Temp), "Desvio padrão" = sd(dados$Temp),
  quantile(dados$Temp))

####@> histograma...
ggplot(data = dados, aes(x = Temp)) +
    geom_histogram(binwidth =  0.5, boundary = 0.5, closed = "right",
                   fill = "white", colour = "black") +
    scale_x_continuous(breaks = seq(17, 26, 0.5), expand = c(0, 0),
                       limits = c(16.5, 26.5)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 30)) +
    labs(x = "Temperatura (ºC)", y = "Frequência absoluta (n)") +
    theme_gray(base_size = 14)

####@> histograma + densidade. Como a densidade probabilidade, neste
####@> caso, não está na mesma escala do número de amostras por classe,
####@> necessitamos ajustar para ficar visível e comparável na figura...
####@> Esta f(x) after_stat(..density..) * nrow(dados) * 0.5 ajusta a
####@> densidade multiplicando: pela contagem total (nrow(dados)) pelo
####@> binwidth = 0.5, para transformar densidade em frequência absoluta
####@> por classe...
ggplot(data = dados, aes(x = Temp)) +
    geom_histogram(binwidth =  0.5, boundary = 0.5, closed = "right",
                   fill = "white", colour = "black") +
    geom_density(aes(y = after_stat(..density..) * nrow(dados) * 0.5),
                 colour = "blue", fill = "blue", alpha = 0.2) +
    scale_x_continuous(breaks = seq(17, 26, 0.5), expand = c(0, 0),
                       limits = c(16.5, 26.5)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 30)) +
    labs(x = "Temperatura (ºC)", y = "Frequência absoluta (n)") +
    theme_gray(base_size = 14)

######@>----------------------------------------------------------------
######@> Repita o processo para as demais variáveis...
######@>----------------------------------------------------------------

#####@> pH....

####@> média, desvio padrão e quantis da distribuição...
c("Média" = mean(dados$pH), "Desvio padrão" = sd(dados$pH),
  quantile(dados$pH))

ggplot(data = dados, aes(x = pH)) +
  geom_histogram(binwidth = 0.5, boundary = 0.5, closed = "right",
                fill = "white", colour = "black") +
  geom_density(aes(y = after_stat(..density..) * nrow(dados) * 0.5),
                colour = "blue", fill = "blue", alpha = 0.2) +
  labs(x = "pH", y = "Frequência absoluta (n)") +
  theme_gray(base_size = 14)
  
#####@> Salinidade....

####@> média, desvio padrão e quantis da distribuição...
c("Média" = mean(dados$Sal), "Desvio padrão" = sd(dados$Sal),
  quantile(dados$Sal))

ggplot(data = dados, aes(x = Sal)) +
  geom_histogram(fill = "white", colour = "black") +
  geom_density(aes(y = after_stat(..density..) * nrow(dados) * 0.5),
                colour = "blue", fill = "blue", alpha = 0.2) +
  labs(x = "Salinidade", y = "Frequência absoluta (n)") +
  theme_gray(base_size = 14)

#####@> Granulometria fina

####@> média, desvio padrão e quantis da distribuição...
c("Média" = mean(dados$Fino), "Desvio padrão" = sd(dados$Fino),
  quantile(dados$Fino))

ggplot(data = dados, aes(x = Fino)) +
    geom_histogram(fill = "white", colour = "black") +
    geom_density(aes(y = after_stat(..density..) * nrow(dados) * 0.5),
                 colour = "blue", fill = "blue", alpha = 0.2) +
    labs(x = "Granulometria Fina", y = "Frequência absoluta (n)") +
    theme_gray(base_size = 14)

#####@> Granulometria grossa

####@> média, desvio padrão e quantis da distribuição...
c("Média" = mean(dados$Grosso), "Desvio padrão" = sd(dados$Grosso),
  quantile(dados$Grosso))

ggplot(data = dados, aes(x = Grosso)) +
    geom_histogram(fill = "white", colour = "black") +
    geom_density(aes(y = after_stat(..density..) * nrow(dados) * 0.5),
                 colour = "blue", fill = "blue", alpha = 0.2) +
    labs(x = "Granulometria Grossa", y = "Frequência absoluta (n)") +
    theme_gray(base_size = 14)

#####@> Matéria orgânica

####@> média, desvio padrão e quantis da distribuição...
c("Média" = mean(dados$MO), "Desvio padrão" = sd(dados$MO),
  quantile(dados$MO))

ggplot(data = dados, aes(x = MO)) +
    geom_histogram(binwidth =  0.5, boundary = 0.5, closed = "right",
                   fill = "white", colour = "black") +
    geom_density(aes(y = after_stat(..density..) * nrow(dados) * 0.5),
                 colour = "blue", fill = "blue", alpha = 0.2) +
    scale_x_continuous(breaks = seq(0, 18, 3), expand = c(0, 0),
                       limits = c(0, 18)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 10)) +
    labs(x = "Matéria orgânica", y = "Frequência absoluta (n)") +
    theme_gray(base_size = 14)

#####@> Carbono

####@> média, desvio padrão e quantis da distribuição...
c("Média" = mean(dados$Carb), "Desvio padrão" = sd(dados$Carb),
  quantile(dados$Carb))

ggplot(data = dados, aes(x = Carb)) +
    geom_histogram(binwidth =  0.5, boundary = 0.5, closed = "right",
                   fill = "white", colour = "black") +
    geom_density(aes(y = after_stat(..density..) * nrow(dados) * 0.5),
                 colour = "blue", fill = "blue", alpha = 0.2) 
    labs(x = "Matéria orgânica", y = "Frequência absoluta (n)") +
    theme_gray(base_size = 14)

#####@> Cádmio

####@> média, desvio padrão e quantis da distribuição...
c("Média" = mean(dados$Cd), "Desvio padrão" = sd(dados$Cd),
  quantile(dados$Cd))

ggplot(data = dados, aes(x = Cd)) +
    geom_histogram(binwidth =  0.5, boundary = 0.5, closed = "right",
                   fill = "white", colour = "black") +
    geom_density(aes(y = after_stat(..density..) * nrow(dados) * 0.5),
                 colour = "blue", fill = "blue", alpha = 0.2) +
    labs(x = "Cádmio", y = "Frequência absoluta (n)") +
    theme_gray(base_size = 14)


######@> Avaliando as variáveis físico-químicas em função de outras
######@> variáveis...

#####@> Temperatura...

####@> média, desvio padrão e quantis da distribuição por Local...
dados %>%
    group_by(Local) %>%
    summarise(Media = mean(Temp, na.rm = TRUE),
              Desvio =  sd(Temp, na.rm = TRUE),
              q2.5 = quantile(Temp, 0.025),
              q50 = quantile(Temp, 0.5),
              q97.5 = quantile(Temp, 0.975))

####@> boxplot por Local...
ggplot(data = dados, aes(x = Local, y = Temp)) +
    geom_boxplot(fill = "white", colour = "black") +
    scale_y_continuous(limits = c(0, 30)) +
    labs(x = "Local", y = "Temperatura (ºC)") +
    theme_gray(base_size = 14)

####@> média, desvio padrão e quantis da distribuição por Local e Dragagem...
dados %>%
    group_by(Local, Draga) %>%
    summarise(Media = mean(Temp, na.rm = TRUE),
              Desvio =  sd(Temp, na.rm = TRUE),
              q2.5 = quantile(Temp, 0.025),
              q50 = quantile(Temp, 0.5),
              q97.5 = quantile(Temp, 0.975))

####@> boxplot por Local...
ggplot(data = dados, aes(x = Local, y = Temp)) +
    geom_boxplot(fill = "white", colour = "black") +
    scale_y_continuous(limits = c(0, 30)) +
    facet_wrap(~Draga) +
    labs(x = "Local", y = "Temperatura (ºC)") +
    theme_gray(base_size = 14)

########################################################################
######@> 05. Explorando a base de dados - Utilizando novas
######@> possibilidades...

######@> Vamos brincar com um pacote que otimiza nosso trabalho...
explore(dados)

######@> Podemos fazer um report completo da nossa base de dados...
dados %>% report(output_file = "report.html",
                 output_dir = tempdir())

######@> Podemos avaliar, por exemplo, a influência dos dados como um
######@> todo sobre uma variável alvo...

#####@> Explorando os dados em função do alvo...
dados %>%
    report(output_file = "report.html",
           output_dir = tempdir(),
           target = Draga)

######@> Podemos buscar exploração de variáveis individuais...
dados %>%
    explore(Zn)

######@> Podemos buscar exploração de variáveis de forma comparativa...
dados %>%
    explore(Zn, target = Draga)

######@> Podemos buscar compreender o grau de associação entre variáveis
######@> numéricas...
dados %>%
    explore(Zn, target = Cd)


######@> Podemos buscar compreender o grau de associação entre variáveis
######@> numéricas em multiplas relações...
dados %>%
    select(Cd, Zn, Ni, Cr) %>%
    explore_all(target = Cd)

######@> Podemos buscar compreender o grau de associação entre variáveis
######@> numéricas em multiplas relações, contra Local...
dados %>%
    select(Local, Cd, Zn, Ni, Cr) %>%
    explore_all(target = Local)

######@> Descrevendo a base a partir de resumos estatísticos
######@> completos...
dados %>%
    describe() %>%
    as.data.frame()

########################################################################
##
##                  Creative Commons License 4.0
##                       (CC BY-NC-SA 4.0)
##
##  This is a humam-readable summary of (and not a substitute for) the
##  license (https://creativecommons.org/licenses/by-nc-nd/4.0/legalcode)
##
##  You are free to:
##
##  Share - copy and redistribute the material in any medium or format.
##
##  The licensor cannot revoke these freedoms as long as you follow the
##  license terms.
##
##  Under the following terms:
##
##  Attribution - You must give appropriate credit, provide a link to
##  license, and indicate if changes were made. You may do so in any
##  reasonable manner, but not in any way that suggests the licensor
##  endorses you or your use.
##
##  NonCommercial - You may not use the material for commercial
##  purposes.
##
##  ShareAlike - If you remix, transform, or build upon the material,
##  you must distributive your contributions under the same license
##  as the  original.
##
##  No additional restrictions — You may not apply legal terms or
##  technological measures that legally restrict others from doing
##  anything the license permits.
##
########################################################################
