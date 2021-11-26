#Publicado em: 25 de Novembro de 2021

#Modelo para previsão de bilheteria de filmes



library(quantmod)
library(dplyr)
library(VIM)
library(missMDA)
library(Amelia)
library(mice)
library(missForest)
library(FactoMineR)
library(readxl)
library(Hmisc)
library(stringr)
library(magrittr)
library(caret)
library(tidyverse)
library(ggplot2)
library(dummies)
library(fastDummies)
library(vcd)
library(corrplot)
library(PerformanceAnalytics)
library(caret)
library(klaR)
library(naivebayes)
library(e1071)
library(ggrepel)
require(caTools)
library(naniar)
library(reshape)
library(lubridate)
library(data.table)
library(rvest)
library(RCurl)
library(ggplot2)
library(igraph)
library(stringr)
library(Amelia)
library(lubridate)
library(reshape)
library(corrplot)
library(knitr)
library(gt)
library(rhandsontable)
library(gdata)
library(dplyr)
library(readr)
library(naniar)
library(UpSetR)
library(qdapTools)
library(gtsummary)
library(rlang)
library(lifecycle)
library(pillar)
library(glue)
library(rlang)
library(ggplot2)
library(hrbrthemes)
library(scales)
library(cowplot)
library(corrplot)
library(leaps)
library(olsrr)
library(shiny)
library(DT)
library(ggplot2)

######### ZERANDO O AMBIENTE #########
rm(list=ls())

######### CARREGANDO OS ARQUIVOS ######### 

IMDB_movies <- read.csv(file="C:/Users/arthu/OneDrive/Área de Trabalho/TCC/IMDb movies.csv", header =TRUE, sep = ",", encoding = "UTF-8", stringsAsFactors = FALSE)
TMDB_movies <- read.csv2(file="C:/Users/arthu/OneDrive/Área de Trabalho/TCC/AllMoviesDetailsCleaned.csv", encoding = "UTF-8", stringsAsFactors = FALSE) 
casting <- read.csv2("C:/Users/arthu/OneDrive/Área de Trabalho/TCC/AllMoviesCastingRaw.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
awards <- read.csv(file="C:/Users/arthu/OneDrive/Área de Trabalho/TCC/220k_awards_by_directors.csv", header =TRUE, sep = ",", encoding = "UTF-8", stringsAsFactors = FALSE)

summary(TMDB_movies)

summary(TMDB_movies)

summary(as.numeric(TMDB_movies$popularity))

summary(as.Date(parse_date_time(TMDB_movies$release_date,"dmy")))

summary(as.numeric(TMDB_movies$vote_average))

summary(as.numeric(convert(IMDB_movies$budget)))

summary(as.numeric(convert(IMDB_movies$worlwide_gross_income)))

summary(as.numeric(convert(IMDB_movies$year)))

summary(as.Date(IMDB_movies$date_published))

summary(awards$year)

unique(awards$outcome)

awards <- mutate(awards, resultado_tipo = ifelse(awards$outcome == "Won", 4, 
                                                    ifelse(awards$outcome == "Nominated", 3,
                                                     ifelse(awards$outcome == "2nd place", 2,
                                                     ifelse(awards$outcome == "3rd place", 1,0)))))
                                       
######### RESUMO DAS TABELAS ######### 
sumario <- data.frame(Tabela = c("IMDB_movies", "title_principals", "TMDB_movies", "casting", "awards"),
                      "Qtd_Registros" = c(nrow(IMDB_movies), nrow(names), nrow(title_principals), nrow(TMDB_movies), nrow(casting), nrow(awards)))

rhandsontable(sumario, width = 550, height = 300) %>% 
  hot_col("Qtd_Registros")

colnames(TMDB_movies)[1] <- "id"
str(TMDB_movies)


######### EXCLUINDO COLUNAS #########


temp <- TMDB_movies %>% dplyr::select(imdb_id, 
                                      id,
                                      genres, 
                                      original_language, 
                                      original_title, 
                                      overview,
                                      production_companies,
                                      production_countries,
                                      release_date,
                                      revenue,
                                      spoken_languages,
                                      status,
                                      tagline)


######### FAZENDO UM JOIN ENTRE AS DUAS TABELAS ######### 
final_movies <- left_join(IMDB_movies, 
                          temp,
                          by = c("imdb_title_id" = "imdb_id"))


######### EXCLUINDO COLUNAS ######### 
final_movies <- final_movies %>% dplyr::select(-c("genres", "spoken_languages", "production_countries", "genres", "production_companies", "metascore", "reviews_from_users", "reviews_from_critics", "original_title.x", "original_title.y", "release_date", "year", "revenue", "original_language"))


######### TRANSFORMANDO VALORES DAS COLUNAS ######### 
#Criei uma funÃ§Ã£o para converter uma coluna para valores numericos

convert = function(column) {
  as.numeric(
    format(gsub("\\D+", "", column), 
           big.mark = ".", small.mark = ","))
}


final_movies$usa_gross_income <- convert(final_movies$usa_gross_income)
final_movies$worlwide_gross_income <- convert(final_movies$worlwide_gross_income)
final_movies$date_published <- as.Date(final_movies$date_published)


######### CONVERTENDO TUDO P/ DÃLAR ######### 

#Criando coluna de moeda na tabela
final_movies <- final_movies %>% mutate(moeda = gdata::trim(gsub('[0-9]+', '', final_movies$budget)))

lista_moedas <- gdata::trim(unique(final_movies$moeda))

final_movies["moeda"][final_movies["moeda"] == "$"] <- "USD"

#Extraindo a cotaÃ§Ã£o atual das moedas para dÃ³lar americano
from <- lista_moedas
to <- c("USD")

t <- quantmod::getQuote(paste0(from, to, "=X"), )
t <- cbind(moeda = rownames(t), t, stringsAsFactors = FALSE)
rownames(t) <- 1:nrow(t)
t$moeda <- substr(t$moeda, 1, 3)

t <- t %>% dplyr::select(c("moeda", "Last"))

#Cruzando as duas tabelas e obtendo o resultado

final_movies <- left_join(final_movies, 
                          t,
                          by = c("moeda" = "moeda"))

final_movies$budget <- convert(final_movies$budget)

final_movies <- final_movies %>% mutate(orcamento_ajustado = budget*Last)



######### EXCLUINDO COLUNAS ######### 
df <- final_movies


######### LIMPANDO COLUNAS ######### 

#Removendo espaÃ§os em branco
df$language <- gdata::trim(df$language)
df$genre <- gdata::trim(df$genre)
df$director <- gdata::trim(df$director)
df$writer <- gdata::trim(df$writer)
df$production_company <- gdata::trim(df$production_company)
df$actors <- gdata::trim(df$actors)
df$description <- gdata::trim(df$description)
df$overview <- gdata::trim(df$overview)
df$status <- gdata::trim(df$status)
df$tagline <- gdata::trim(df$tagline)

#Transformando valores em branco
df$language[df$language == "none" | df$language == "" | df$language == "None" | df$language == "No Language"] <- NA
df$genre[df$genre == "none" | df$genre == "" ] <- NA
df$director[df$director == "none" | df$director== "" ] <- NA
df$writer[df$writer== "none" | df$writer== "" ] <- NA
df$production_company[df$production_company== "none" | df$production_company== "" ] <- NA
df$actors[df$actors== "none" | df$actors == "" ] <- NA
df$description[df$description== "none" | df$description == "" ] <- NA
df$overview[df$overview== "none" | df$overview== "" ] <- NA
df$status[df$status== "none" | df$status == "" ] <- NA
df$tagline[df$tagline== "none" | df$tagline == "" ]  <- NA


######### CRIANDO COLUNAS ######### 

#Coluna: ano de lanÃ§amento
df <- df %>% mutate(year = substr(df$date_published,1,4))
df$year <- as.numeric(df$year)

#bkp <- df
#df <- bkp

#Coluna: % de mulheres no filme

c <- casting %>% dplyr::select(c("id", "actor1_gender", "actor2_gender", "actor3_gender", "actor4_gender", "actor5_gender"))
c <- c %>% mutate(female = rowSums(c == "1")/5)
c <- c %>% dplyr::select(c("id", "female"))      

df <- left_join(df, 
                c,
                by = c("id" = "id"))

#bkp <- df


######### AJUSTE INFLACIONÃRIO ######### 

#Webscrapping
webpage <- read_html("https://www.usinflationcalculator.com/monthly-us-inflation-rates-1913-present/")

html_nodes(webpage, "table")%>%.[1]%>% 
  html_table(fill = TRUE)

inflation <- webpage %>% 
  html_nodes("table") %>%
  .[1] %>% 
  html_table(fill = TRUE) %>%
  .[[1]] %>%
  .[-1,]

colnames(inflation) <- c("Year", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
 
inflation <- melt(inflation, id = c("Year"))

colnames(inflation) <- c("Year", "Month", "Value")

inflation$date <- as.Date(with(inflation, paste(Year, Month, 1, sep = "-")),"%Y-%m-%d")

inflation$Value <- as.numeric(format(inflation$Value, 
                                     big.mark = ".", small.mark = ","))


inflation$base_date <- floor_date(as.Date(Sys.Date()), "month") - months(1)

inflation <- na.omit(inflation[order(inflation$date, decreasing = FALSE),])

inflation$cuminflation <- rev(cumprod(inflation$Value/100+1))


df <- df %>% mutate(month_begin = cut(df$date_published, "month"))
df$month_begin <- as.Date(df$month_begin)

inflation <- inflation %>% dplyr:: select(c("date","cuminflation"))

df <- left_join(df, 
                inflation,
                by = c("month_begin" = "date"))


df <- df %>% mutate(usa_gross_adjusted = usa_gross_income*cuminflation)


######### CRIANDO COLUNAS ######### 

#Coluna: mes de lancamento
df <- df %>% mutate(month = month(df$date_published))

#Coluna: dia da semana de lancamento
df <- df %>% mutate(weekday = wday(df$date_published))

#Coluna: qtd de premios dos diretores ate 1 ano antes do filme ser lancado

awards_filtered <- 
  awards[,-c(6)] %>% 
  filter (ceremony %in% 
            c("Golden Globes, USA", 
              "Golden Globes, Italy", 
              "Golden Goblets, Italy", 
              "Golden Globes, Portugal", 
              "Academy Awards, USA", 
              "BAFTA Awards", 
              "BAFTA Awards, Scotland", 
              "BAFTA Awards, Wales", 
              "Berlin International Film Festival", 
              "Cannes Film Festival"), 
          outcome == "Won") %>% 
  group_by(director_name, year) %>% 
  summarise(qtd = n())

unique(awards_filtered$category)

awards_filtered <- arrange(awards_filtered, director_name, year)
awards_filtered <- awards_filtered %>% group_by(director_name) %>% mutate(qtd_awards = cumsum(qtd))
awards_filtered <- awards_filtered %>% dplyr::select(c("director_name", "year", "qtd_awards"))

c <- df %>% dplyr::select(c("imdb_title_id", "year", "director"))

c <- c %>% separate(director, c("dir1", "dir2"), ",")

c$dir1 <- gdata::trim(c$dir1)
c$dir2 <- gdata::trim(c$dir2)

c <- reshape2::melt(c, id.vars=c("imdb_title_id", "year"))
c <- c %>% dplyr:: select(-c("variable"))
c$value <- as.character(c$value)

c <-left_join(c, 
              awards_filtered,
              by = c("value" = "director_name"))

#test <- c %>% select(c("value")) %>% filter(is.na(c$qtd_awards))

#unique(test$value)


#dir <- "C:\\Users\\arthu\\OneDrive\\Ãrea de Trabalho\\TCC\\test.csv"

#write.csv(test
#          , dir, quote = T, fileEncoding = "UTF-16LE")

c <- c %>% mutate(awards = ifelse(year.x > year.y, qtd_awards, 0))

c <- c %>%
  group_by(imdb_title_id) %>% 
  summarise(awards_total = sum(awards))

df <- left_join(df, 
                c,
                by = c("imdb_title_id" = "imdb_title_id"))


bkp <- df

######### CRIANDO COLUNAS DUMMY ######### 

bkp <- df
#df <- bkp

#Coluna: diretora feminina
c <- casting %>% dplyr::select(c("id", "director_gender"))
#c <- c %>% mutate(female_director = ifelse(director_gender == "1", 1, 0))
c <- c %>% mutate(female_director = director_gender)
c <- c %>% dplyr::select(c("id", "female_director"))

df <- left_join(df, 
                c,
                by = c("id" = "id"))



######### EXCLUINDO COLUNAS ######### 
df <- df %>% dplyr::select(-c("title", "date_published", "Last", "moeda", "actors", "description", "avg_vote", "votes", "overview","tagline", "month_begin", "cuminflation", "id"))
df <- df %>% dplyr::select(-c("worlwide_gross_income", "writer", "budget"))

#bkp <- df
#df <- bkp


######### RENOMEANDO COLUNAS ######### 

colnames(df) <- c('imdb_titulo_id',
                  'genero',
                  'duracao',
                  'pais',
                  'linguagem',
                  'diretor',
                  'produtora',
                  'usa_bilheteria_domestica',
                  'status',
                  'orcamento_ajustado',
                  'ano',
                  'rep_mulheres',
                  'usa_bilheteria_domestica_ajustada',
                  'mes',
                  'dia da semana',
                  'premios_total',
                  'diretor_feminino')


c <- dplyr::select(IMDB_movies, c("imdb_title_id", "title"))

t <- left_join(df, 
               c,
               by = c("imdb_titulo_id" = "imdb_title_id"))


#RESUMO
print(paste0("Ao total temos: ", nrow(df)," filmes disponiveis para analise"))

#test <- df %>% filter(!is.na(ano))
bkp <- df

######### SUCESSO PREVIO FINANCEIRO #########

#### SUCESSO DE ATORES ####

t <- left_join(TMDB_movies, 
               casting,
               by = c("id" = "id"))

t <- t %>% select(c("id", "imdb_id", "actor1_name", "actor2_name", "actor3_name", "actor4_name", "actor5_name", "director_name"))

t <- left_join(df,
               t,
               by = c("imdb_titulo_id" = "imdb_id"))

t <- t %>% select(c("id", "imdb_titulo_id", "actor1_name", "actor2_name", "actor3_name", "actor4_name", "actor5_name", "director_name", "ano", "usa_bilheteria_domestica_ajustada"))

t1 <- t %>% select(c("imdb_titulo_id", "actor1_name", "actor2_name", "actor3_name", "actor4_name", "actor5_name", "ano", "usa_bilheteria_domestica_ajustada"))

t1 <- reshape2::melt(t1, id.vars=c("imdb_titulo_id", "ano", "usa_bilheteria_domestica_ajustada"))

total_1 <- t1 %>% select(c("ano", "usa_bilheteria_domestica_ajustada", "value")) %>%
          group_by(value, ano) %>%
          summarise(total = sum(usa_bilheteria_domestica_ajustada))

t1 <- left_join(t1,
          total_1,
          by = c("value" = "value"))

t1 <- t1 %>% mutate(sucesso_previo_atores = ifelse(ano.x > ano.y, total , 0))

t1 <- t1 %>% 
  select(c("imdb_titulo_id", "sucesso_previo_atores")) %>% 
  group_by(imdb_titulo_id) %>% 
  summarise(total = sum(sucesso_previo_atores))

#### SUCESSO DE DIRETORES ####

t2 <- t %>% select(c("imdb_titulo_id", "director_name", "ano", "usa_bilheteria_domestica_ajustada"))

t2 <- reshape2::melt(t2, id.vars=c("imdb_titulo_id", "ano", "usa_bilheteria_domestica_ajustada"))

total_2 <- t2 %>% select(c("ano", "usa_bilheteria_domestica_ajustada", "value")) %>%
  group_by(value, ano) %>%
  summarise(total = sum(usa_bilheteria_domestica_ajustada))

t2 <- left_join(t2,
                total_2,
                by = c("value" = "value"))

t2 <- t2 %>% mutate(sucesso_previo_diretor = ifelse(ano.x > ano.y, total , 0))

t2 <- t2 %>% 
  select(c("imdb_titulo_id", "sucesso_previo_diretor")) %>% 
  group_by(imdb_titulo_id) %>% 
  summarise(total = sum(sucesso_previo_diretor))

#bkp <- t

t <- left_join(t1,
               t2,
               by = c("imdb_titulo_id" = "imdb_titulo_id"))


colnames(t) <- c("imdb_titulo_id", "sucesso_previo_atores", "sucesso_previo_diretor")

#bkp <- df

#head(t)

df <- left_join(df,
                       t,
                       by = c("imdb_titulo_id" = "imdb_titulo_id"))



######### SUCESSO PREVIO POR NOTAS #########


#### SUCESSO DE ATORES ####

f <- left_join(TMDB_movies, 
               casting,
               by = c("id" = "id"))


f <- f %>% select(c("id", "imdb_id", "actor1_name", "actor2_name", "actor3_name", "actor4_name", "actor5_name", "director_name"))

f <- left_join(IMDB_movies,
               f,
               by = c("imdb_title_id" = "imdb_id"))

f <- f %>% select(c("id", "imdb_title_id", "actor1_name", "actor2_name", "actor3_name", "actor4_name", "actor5_name", "director_name", "year", "avg_vote"))


f1 <- f %>% select(c("imdb_title_id", "actor1_name", "actor2_name", "actor3_name", "actor4_name", "actor5_name", "year", "avg_vote"))

f1 <- reshape2::melt(f1, id.vars=c("imdb_title_id", "year", "avg_vote"))


ftotal_1 <- f1 %>% select(c("year", "avg_vote", "value")) %>%
  group_by(value, year) %>%
  summarise(total = mean(avg_vote))


f1 <- left_join(f1,
                ftotal_1,
                by = c("value" = "value"))

f1 <- f1 %>% mutate(notas_previas_atores = ifelse(year.x > year.y, total , 0))

f1 <- f1 %>% filter(notas_previas_atores > 0)

f1 <- f1 %>% 
  select(c("imdb_title_id", "notas_previas_atores")) %>% 
  group_by(imdb_title_id) %>% 
  summarise(total = mean(notas_previas_atores))



#### SUCESSO DE DIRETORES ####

f2 <- f %>% select(c("imdb_title_id", "director_name", "year", "avg_vote"))

f2 <- reshape2::melt(f2, id.vars=c("imdb_title_id", "year", "avg_vote"))


ftotal_2 <- f2 %>% select(c("year", "avg_vote", "value")) %>%
  group_by(value, year) %>%
  summarise(total = mean(avg_vote))


f2 <- left_join(f2,
                ftotal_1,
                by = c("value" = "value"))

f2 <- f2 %>% mutate(notas_previas_diretores = ifelse(year.x > year.y, total , 0))

f2 <- f2 %>% filter(notas_previas_diretores > 0)

f2 <- f2 %>% 
  select(c("imdb_title_id", "notas_previas_diretores")) %>% 
  group_by(imdb_title_id) %>% 
  summarise(total = mean(notas_previas_diretores))



f <- left_join(f1,
               f2,
               by = c("imdb_title_id" = "imdb_title_id"))


colnames(f) <- c("imdb_titulo_id", "notas_previas_atores", "notas_previas_diretores")

df <- left_join(df,
                f,
                by = c("imdb_titulo_id" = "imdb_titulo_id"))




df <- df %>% filter(status == "Released")




################ ANALISE EXPLORATORIA ##################

#install.packages("hrbrthemes")


#### COMEÇANDO PELO BÁSICO: DISTRIBUIÇÃO ####

p <- ggplot(df, aes(x=usa_bilheteria_domestica_ajustada)) + 
  geom_histogram(binwidth=1000000, fill="#69b3a2", color="#e9ecef", alpha=0.9)


#### 1 - BILHETERIA POR ANO ####

ggplot(df, aes(x=ano, y=usa_bilheteria_domestica_ajustada)) + 
  geom_bar(stat = "identity",  fill=rgb(0.1,0.4,0.5,0.7))


#### 2 - BILHETERIA POR REPRESENTATIVIDADE FEMININA ####

graphic2 <- 
  df %>% 
  group_by(as.factor(rep_mulheres)) %>% 
  summarise(mean = mean(usa_bilheteria_domestica_ajustada, na.rm = T), n = n())

ggplot(graphic2, aes(x=graphic2$`as.factor(rep_mulheres)`, y=mean)) + 
  geom_bar(stat = "identity",  fill=rgb(0.1,0.4,0.5,0.7))+
labs(y = "Bilheteria Domestica Ajustada (EUA)", x = "Representatividade Feminina (%)")+
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6))+
  geom_text(aes(label = format(mean/1000000, digits = 3, big.mark=","), vjust = -0.2))

ggplot(df, aes(x=as.factor(rep_mulheres), y=usa_bilheteria_domestica_ajustada)) + 
  geom_point(
    color="black",
    fill="#69b3a2",
    shape=22,
    alpha=0.5,
    size=6,
    stroke = 1
  ) +
  theme_ipsum()

graphic2
#### 3 - BILHETERIA POR GENERO DO DIRETOR ####
#1: mulher
#2: homem
#0: nao definido, substituído por NA

df$diretor_feminino[df$diretor_feminino == 0] <- NA

graphic3 <- 
  df %>% 
  group_by(as.factor(diretor_feminino)) %>% 
  summarise(mean = mean(usa_bilheteria_domestica_ajustada, na.rm = T), n = n())

ggplot(graphic3, aes(x=graphic3$`as.factor(diretor_feminino)`, y=mean)) + 
  geom_bar(stat = "identity",  fill=rgb(0.1,0.4,0.5,0.7))+
  labs(y = "Bilheteria Domestica Ajustada (EUA)", x = "Gênero do diretor")+
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6))+
  geom_text(aes(label = format(mean/1000000, digits = 3, big.mark=","), vjust = -0.2))

ggplot(df, aes(x=as.factor(diretor_feminino), y=usa_bilheteria_domestica_ajustada)) + 
  geom_point(
    color="black",
    fill="#69b3a2",
    shape=22,
    alpha=0.5,
    size=6,
    stroke = 1
  ) +
  theme_ipsum()

graphic3



#### 4 - BILHETERIA POR GENERO CINEMATOGRAFICO ####

graphic4 <-
  cbind(df %>% select(c("genero", "usa_bilheteria_domestica_ajustada")),mtabulate(gdata::trim(strsplit(as.character(df$genero), ','))))

graphic4 <- graphic4 %>% select(-c("genero"))

colnames(graphic4) <- c('usa_bilheteria_domestica_ajustada',
                  'Acao',
                  'Adulto',
                  'Aventura',
                  'Animacao',
                  'Biografia',
                  'Comedia',
                  'Crime',
                  'Documentario',
                  'Drama',
                  'Familia',
                  'Fantasia',
                  'Noir',
                  'Game_Show',
                  'Historia',
                  'Terror',
                  'Musica',
                  'Musical',
                  'Misterio',
                  'Noticia',
                  'Reality_TV',
                  'Romance',
                  'Sci_Fi',
                  'Esporte',
                  'Thriller',
                  'Guerra',
                  'Velho_Oeste')



#df <- df %>% dplyr::select(-c("worlwide_gross_income", "genre", "status", "genre", "writer", "budget"))

graphic4 <- reshape2::melt(graphic4, id.vars=c("usa_bilheteria_domestica_ajustada"))

graphic4 <- graphic4 %>% filter(value == 1)

graphic4 <- 
  graphic4 %>% 
  group_by(variable) %>% 
  summarise(mean = mean(usa_bilheteria_domestica_ajustada, na.rm = T), n = n())


ggplot(graphic4, aes(x= reorder(variable, mean), y=mean)) + 
  geom_bar(stat = "identity",  fill=rgb(0.1,0.4,0.5,0.7))+
  labs(y = "Bilheteria Domestica Ajustada (EUA)", x = "Gênero do filme")+
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6))+
  geom_text(aes(label = format(mean/1000000, digits = 3, big.mark=","), hjust = -0.1))+
  theme(axis.text.x = element_text(angle = 90)) +
  coord_flip()


graphic4 <-
  cbind(df %>% select(c("genero", "orcamento_ajustado")),mtabulate(gdata::trim(strsplit(as.character(df$genero), ','))))

graphic4 <- graphic4 %>% select(-c("genero"))

colnames(graphic4) <- c('orcamento_ajustado',
                        'Acao',
                        'Adulto',
                        'Aventura',
                        'Animacao',
                        'Biografia',
                        'Comedia',
                        'Crime',
                        'Documentario',
                        'Drama',
                        'Familia',
                        'Fantasia',
                        'Noir',
                        'Game_Show',
                        'Historia',
                        'Terror',
                        'Musica',
                        'Musical',
                        'Misterio',
                        'Noticia',
                        'Reality_TV',
                        'Romance',
                        'Sci_Fi',
                        'Esporte',
                        'Thriller',
                        'Guerra',
                        'Velho_Oeste')

graphic4 <- reshape2::melt(graphic4, id.vars=c("orcamento_ajustado"))

graphic4 <- graphic4 %>% filter(value == 1)

graphic4 <- 
  graphic4 %>% 
  group_by(variable) %>% 
  summarise(mean = mean(orcamento_ajustado, na.rm = T), n = n())


ggplot(graphic4, aes(x= reorder(variable, mean), y=mean)) + 
  geom_bar(stat = "identity",  fill=rgb(0.1,0.4,0.5,0.7))+
  labs(y = "Orçamento Ajustado", x = "Gênero do filme")+
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6))+
  geom_text(aes(label = format(mean/1000000, digits = 1, big.mark=","), hjust = -0.1))+
  theme(axis.text.x = element_text(angle = 90)) +
  coord_flip()


#### 5 - BILHETERIA POR IDIOMA ####


graphic5 <-
  cbind(df %>% select(c("linguagem", "usa_bilheteria_domestica_ajustada")),mtabulate(gdata::trim(strsplit(as.character(df$linguagem), ','))))

graphic5 <- graphic5 %>% select(-c("linguagem"))

#df <- df %>% dplyr::select(-c("worlwide_gross_income", "genre", "status", "genre", "writer", "budget"))

graphic5 <- reshape2::melt(graphic5, id.vars=c("usa_bilheteria_domestica_ajustada"))

graphic5 <- graphic5 %>% filter(value == 1)

graphic5 <- 
  graphic5 %>% 
  group_by(variable) %>% 
  summarise(mean = mean(usa_bilheteria_domestica_ajustada, na.rm = T), n = n())

graphic5 <- graphic5[order(-graphic5$mean),]

graphic5 <- graphic5 %>% filter(n > 1000)

datatable(graphic5,  options = list(dom = 't'))


ggplot(graphic5, aes(x= reorder(variable, mean), y=mean)) + 
  geom_bar(stat = "identity",  fill=rgb(0.1,0.4,0.5,0.7))+
  labs(y = "Bilheteria Domestica Ajustada (EUA)", x = "Linguagem do filme")+
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6))+
  geom_text(aes(label = format(mean/1000000, digits = 3, big.mark=","), hjust = -0.1))+
  theme(axis.text.x = element_text(angle = 90)) +
  coord_flip()

## CENÁRIO MONOLINGUÍSTICO E MULTILINGUÍSTICO
graphic5.1 <- df %>% select(c("linguagem", "usa_bilheteria_domestica_ajustada")) %>% 
  mutate(linguagens = str_count(df$linguagem, ",")+1)

graphic5.1 <-
  cbind(graphic5.1 %>% select(c("linguagem", "linguagens", "usa_bilheteria_domestica_ajustada")),mtabulate(gdata::trim(strsplit(as.character(graphic5.1$linguagem), ','))))

graphic5.1 <- reshape2::melt(graphic5.1, id.vars=c("usa_bilheteria_domestica_ajustada", "linguagens","linguagem"))

graphic5.1 <- graphic5.1 %>% filter(value == 1)

graphic5.1 <- graphic5.1 %>% select(-c("linguagem"))

#graphic5.1 <- 
#  graphic5.1 %>% 
#  group_by(as.factor(variable), linguagens) %>% 
#  summarise(mean = mean(usa_bilheteria_domestica_ajustada, na.rm = T), n = n())

graphic5.1 <- graphic5.1 %>% mutate(categoria = ifelse(linguagens == 1, "Mono", "Multi"))
  
colnames(graphic5.1) <- c("bilheteria", "num_linguagens", "linguagem", "value", "categoria")

graphic5.1 <- graphic5.1 %>% select(-c("value", "num_linguagens"))

#MONOLINGUÍSTICO
#install.packages("cowplot")

graphic5.1.1 <- graphic5.1 %>% filter(categoria == "Mono")

graphic5.1.1 <- 
    graphic5.1.1 %>% 
    group_by(linguagem) %>% 
    summarise(mean = mean(bilheteria, na.rm = T), n = n())
  
  
graphic5.1.1 <- graphic5.1.1[order(-graphic5.1.1$mean),]

mean(graphic5.1.1$n)

graphic5.1.1 <- graphic5.1.1 %>% filter(n > 500)

datatable(graphic5.1.1 %>% select(c("linguagem","mean")),  options = list(dom = 't'))

mono <- 
  ggplot(graphic5.1.1, aes(x= reorder(linguagem, mean), y=mean)) + 
  geom_bar(stat = "identity",  fill=rgb(0.1,0.4,0.5,0.7))+
  labs(y = "Bilheteria Domestica Ajustada (EUA)", x = "Linguagem do filme")+
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6))+
  geom_text(aes(label = format(mean/1000000, digits = 1, big.mark=".", decimal.mark = ","), hjust = -0.1), size = 4)+
  theme(axis.text.x = element_text(angle = 90)) +
  coord_flip()

#MULTILINGUÍSTICO

graphic5.1.2 <- graphic5.1 %>% filter(categoria == "Multi")

graphic5.1.2 <- 
  graphic5.1.2 %>% 
  group_by(linguagem) %>% 
  summarise(mean = mean(bilheteria, na.rm = T), n = n())

graphic5.1.2 <- graphic5.1.2[order(-graphic5.1.2$mean),]

mean(graphic5.1.2$n)

graphic5.1.2 <- graphic5.1.2 %>% filter(n > 100)

datatable(graphic5.1.2 %>% select(c("linguagem","mean")),  options = list(dom = 't'))

multi <- 
  ggplot(graphic5.1.2, aes(x= reorder(linguagem, mean), y=mean)) + 
  geom_bar(stat = "identity",  fill=rgb(0.1,0.4,0.5,0.7))+
  labs(y = "Bilheteria Domestica Ajustada (EUA)", x = "Linguagem do filme")+
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6))+
  geom_text(aes(label = format(mean/1000000, digits = 2, big.mark=".", decimal.mark = ","), hjust = -0.1), size = 3)+
  theme(axis.text.x = element_text(angle = 90)) +
  coord_flip()

mono
multi

#### 6 - BILHETERIA X ORÇAMENTO ####


ggplot(df, aes(x=orcamento_ajustado, y=usa_bilheteria_domestica_ajustada)) + 
  geom_point() +
  theme_ipsum()+
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE)+
  labs(y = "Bilheteria Domestica Ajustada (EUA)", x = "Orçamento Ajustado")+
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6))+
  scale_x_continuous(labels = label_number(suffix = " M", scale = 1e-6))+
  xlim(NA, 3e+8)

#### 7 - BILHETERIA POR MES ####


graphic7 <- 
  df %>% 
  group_by(as.factor(mes)) %>% 
  summarise(mean = mean(usa_bilheteria_domestica_ajustada, na.rm = T), n = n())


colnames(graphic7) <- c("mes", "mean","n")

levels(graphic7$mes) <- c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago","Set","Out","Nov","Dez")

ggplot(graphic7, aes(x=mes, y=mean)) + 
  geom_bar(stat = "identity",  fill=rgb(0.1,0.4,0.5,0.7))+
  labs(y = "Bilheteria Domestica Ajustada (EUA)", x = "Mês de Lançamento")+
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6))+
  geom_text(aes(label = format(mean/1000000, digits = 3, big.mark=","), vjust = -0.2))


#### 8 - BILHETERIA POR DIA DA SEMANA ####

#1: domingo
#7: sabado

graphic8 <- 
  df %>% 
  group_by(as.factor(`dia da semana`)) %>% 
  summarise(mean = mean(usa_bilheteria_domestica_ajustada, na.rm = T), n = n())

colnames(graphic8) <- c("dia_da_semana", "mean","n")

levels(graphic8$dia_da_semana) <- c("Domingo", "Segunda", "Terca", "Quarta", "Quinta", "Sexta", "Sabado")

ggplot(graphic8, aes(x=dia_da_semana, y=mean)) + 
  geom_bar(stat = "identity",  fill=rgb(0.1,0.4,0.5,0.7))+
  labs(y = "Bilheteria Domestica Ajustada (EUA)", x = "Dia da Semana")+
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6))+
  geom_text(aes(label = format(mean/1000000, digits = 3, big.mark=","), vjust = -0.2))

#### 9 - BILHETERIA POR SUCESSO PREVIO ATORES ####


ggplot(df, aes(x=sucesso_previo_atores, y=usa_bilheteria_domestica_ajustada)) + 
  geom_point() +
  theme_ipsum()+
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE)+
  labs(y = "Bilheteria Domestica Ajustada (EUA)", x = "Sucesso Previo de Atores")+
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6))+
  scale_x_continuous(labels = label_number(suffix = " M", scale = 1e-6))


graphic9 <- df %>% select(c("sucesso_previo_atores", "usa_bilheteria_domestica_ajustada"))

graphic9 <- 
  graphic9 %>% mutate(sucesso_grupo = cut(sucesso_previo_atores, breaks=seq(1,3000000000,100000000)))

graphic9 <- graphic9 %>% select(-c("sucesso_previo_atores")) %>% 
  group_by(sucesso_grupo) %>% 
  summarise(mean = mean(usa_bilheteria_domestica_ajustada, na.rm = T), n = n())


ggplot(graphic9, aes(x=sucesso_grupo, y=mean)) + 
  geom_bar(stat = "identity",  fill=rgb(0.1,0.4,0.5,0.7))+
  labs(y = "Bilheteria Domestica Ajustada (EUA)", x = "Nota Previa dos Atores")+
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6))+
  geom_text(aes(label = format(mean/1000000, digits = 3, big.mark=","), vjust = -0.2))

#### 10 - BILHETERIA POR SUCESSO PREVIO DIRETORES ####


ggplot(df, aes(x=sucesso_previo_diretor, y=usa_bilheteria_domestica_ajustada)) + 
  geom_point() +
  theme_ipsum()+
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE)+
  labs(y = "Bilheteria Domestica Ajustada (EUA)", x = "Sucesso Previo do Diretor")+
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6))+
  scale_x_continuous(labels = label_number(suffix = " M", scale = 1e-6))


#### 11 - BILHETERIA POR NOTA PREVIA ATORES ####

ggplot(df, aes(x=notas_previas_atores, y=usa_bilheteria_domestica_ajustada)) + 
  geom_point() +
  theme_ipsum()+
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE)+
  labs(y = "Bilheteria Domestica Ajustada (EUA)", x = "Nota Previa de Atores")+
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6))


graphic11 <- df %>% select(c("notas_previas_atores", "usa_bilheteria_domestica_ajustada"))

graphic11 <- 
  graphic11 %>% mutate(notas_grupo = cut(notas_previas_atores, breaks=seq(1,10,1)))

graphic11 <- graphic11 %>% select(-c("notas_previas_atores")) %>% 
  group_by(notas_grupo) %>% 
  summarise(mean = mean(usa_bilheteria_domestica_ajustada, na.rm = T), n = n())


ggplot(graphic11, aes(x=notas_grupo, y=mean)) + 
  geom_bar(stat = "identity",  fill=rgb(0.1,0.4,0.5,0.7))+
  labs(y = "Bilheteria Domestica Ajustada (EUA)", x = "Nota Previa dos Atores")+
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6))+
  geom_text(aes(label = format(mean/1000000, digits = 3, big.mark=","), vjust = -0.2))


#### 12 - BILHETERIA POR NOTA PREVIA DIRETORES ####

ggplot(df, aes(x=notas_previas_diretores, y=usa_bilheteria_domestica_ajustada)) + 
  geom_point() +
  theme_ipsum()+
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE)+
  labs(y = "Bilheteria Domestica Ajustada (EUA)", x = "Nota Previa do Diretor")+
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6))

graphic12 <- df %>% select(c("notas_previas_diretores", "usa_bilheteria_domestica_ajustada"))

graphic12 <- 
  graphic12 %>% mutate(notas_grupo = cut(notas_previas_diretores, breaks=seq(1,10,1)))

graphic12 <- graphic12 %>% select(-c("notas_previas_diretores")) %>% 
  group_by(notas_grupo) %>% 
  summarise(mean = mean(usa_bilheteria_domestica_ajustada, na.rm = T), n = n())


ggplot(graphic12, aes(x=notas_grupo, y=mean)) + 
  geom_bar(stat = "identity",  fill=rgb(0.1,0.4,0.5,0.7))+
  labs(y = "Bilheteria Domestica Ajustada (EUA)", x = "Nota Previa do Diretor")+
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6))+
  geom_text(aes(label = format(mean/1000000, digits = 3, big.mark=","), vjust = -0.2))





#### 13 - BILHETERIA POR QUANTIDADE DE PREMIOS PREVIOS ####


graphic13 <- 
  df %>% 
  group_by(premios_total) %>% 
  summarise(mean = mean(usa_bilheteria_domestica_ajustada, na.rm = T), n = n())

ggplot(df, aes(x=premios_total, y=usa_bilheteria_domestica_ajustada)) + 
  geom_point() +
  theme_ipsum()+
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE)+
  labs(y = "Bilheteria Domestica Ajustada (EUA)", x = "Prêmios e indicações")+
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6))


#### 14 - BILHETERIA POR DURACAO #####

ggplot(df, aes(x=duracao, y=usa_bilheteria_domestica_ajustada)) + 
  geom_point() +
  theme_ipsum()+
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE)+
  labs(y = "Bilheteria Domestica Ajustada (EUA)", x = "Duração (em minutos)")+
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6))+
  xlim(NA, 750)

graphic14 <- df %>% select(c("duracao", "usa_bilheteria_domestica_ajustada"))

summary(graphic14)

graphic14 <- 
  graphic14 %>% mutate(duracao_grupo = cut(duracao, breaks=seq(40,4000,10)))


graphic14 <- graphic14 %>% select(-c("duracao")) %>% 
  group_by(duracao_grupo) %>% 
  summarise(mean = mean(usa_bilheteria_domestica_ajustada, na.rm = T), n = n())



#### 15 - BILHETERIA POR PAÍS PRODUTOR #####

graphic15 <-
  cbind(df %>% select(c("pais", "usa_bilheteria_domestica_ajustada")),mtabulate(gdata::trim(strsplit(as.character(df$pais), ','))))

graphic15 <- graphic15 %>% select(-c("pais"))


#df <- df %>% dplyr::select(-c("worlwide_gross_income", "genre", "status", "genre", "writer", "budget"))

graphic15 <- reshape2::melt(graphic15, id.vars=c("usa_bilheteria_domestica_ajustada"))

graphic15 <- graphic15 %>% filter(value == 1)

graphic15 <- 
  graphic15 %>% 
  group_by(variable) %>% 
  summarise(mean = mean(usa_bilheteria_domestica_ajustada, na.rm = T), n = n())

graphic15 <- graphic15 %>% filter(!is.na(mean))

mean(graphic15$n)

graphic15 <- graphic15 %>% filter(n >= 500)

graphic15 <- graphic15[order(-graphic15$mean),]

colnames(graphic15) <- c("pais", "media_de_bilheteria","qtd")

#graphic15$`Média de Bilheteria` <- format(graphic15$`Média de Bilheteria`, nsmall = 1, big.mark = ".", decimal.mark = ",")

datatable(graphic15 %>% select(-c("Qtd")),  options = list(dom = 't'))


ggplot(graphic15, aes(x= reorder(pais, media_de_bilheteria), y=media_de_bilheteria)) + 
  geom_bar(stat = "identity",  fill=rgb(0.1,0.4,0.5,0.7))+
  labs(y = "Bilheteria doméstica ajustada (EUA)", x = "País produtor")+
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6))+
  geom_text(aes(label = format(media_de_bilheteria/1000000, digits = 1, big.mark=".", decimal.mark = ","), hjust = -0.1))+
  theme(axis.text.x = element_text(angle = 90)) +
  coord_flip()








bkp <- df
#df <- bkp
df <- df %>% select(-c("diretor", 
                      "status", "usa_bilheteria_domestica"))

####### TRANSFORMANDO A TABELA






######### CRIANDO COLUNAS DUMMY ######### 

#ABERTURA POR GÊNERO CINEMATOGRÁFICO
df <- cbind(df[1:18],mtabulate(gdata::trim(strsplit(as.character(df$genero), ','))))
#df <- df %>% dplyr::select(-c("worlwide_gross_income", "genre", "status", "genre", "writer", "budget"))

#FILMES MULTILINGUAS OU NAO
df <- df %>%
    mutate(lingua_grupo = ifelse(str_count(df$linguagem, ",")+1 == 1, 1,0))

#Coluna: pais (SE EH "USA" OU NAO)
df <- df %>% mutate(usa_production = ifelse(grepl("USA", df$pais), 1, 0))

#Coluna: produtora (SE EH UMA DAS BIG SIX OU NAO)
df <- df %>% mutate(big_six = ifelse(produtora %in% c('Universal Pictures',
                                                               'Paramount Pictures',
                                                               'Warner Bros.',
                                                               'Fox Film Corporation',
                                                               'Twentieth Century Fox',
                                                               'Walt Disney Productions',
                                                               'Universal International Pictures (UI)',
                                                               'Walt Disney Pictures',
                                                               'Walt Disney Feature Animation',
                                                               '21st Century Film Corporation',
                                                               'Disney Television Animation',
                                                               'Warner Bros. Animation',
                                                               '21st Century Films',
                                                               'Sony New Technologies',
                                                               'Fox 2000 Pictures',
                                                               'Warner Bros. Family Entertainment',
                                                               'Walt Disney Feature Animation Florida',
                                                               'Sony Pictures Classics',
                                                               'Paramount Classics',
                                                               'Twentieth Century Fox Animation',
                                                               'Warner Independent Pictures (WIP)',
                                                               'Walt Disney Animation Studios',
                                                               'Paramount Vantage',
                                                               'Universal Pictures International (UPI)',
                                                               'Warner Bros. Pictures',
                                                               'Paramount Animation',
                                                               'Warner Bros. Digital Distribution',
                                                               'Sony Pictures Entertainment (SPE)',
                                                               'Sony Pictures Animation',
                                                               'Warner Animation Group',
                                                               'Paramount Players',
                                                               'Sony Pictures Worldwide Acquisitions (SPWA)'), 1, 0))

#bkp <- df
#Coluna: lingua inglesa

df$linguagem[df$linguagem == ""] <- NA

df <- df %>% mutate(english = ifelse(grepl("English", df$linguagem), 1, ifelse(is.na(df$linguagem), NA,0)))

df <- df %>% select(-c("genero", "pais", "linguagem", "produtora"))

colnames(df) <- c('imdb_titulo_id',
                  'duracao',
                  'orcamento_ajustado',
                  'ano',
                  'rep_mulheres',
                  'usa_bilheteria_domestica_ajustada',
                  'mes',
                  'diasemana',
                  'premios_total',
                  'genero_diretor',
                  'sucesso_previo_atores',
                  'sucesso_previo_diretor',
                  'notas_previas_atores',
                  'notas_previas_diretores',
                  'Acao',
                  'Adulto',
                  'Aventura',
                  'Animacao',
                  'Biografia',
                  'Comedia',
                  'Crime',
                  'Documentario',
                  'Drama',
                  'Familia',
                  'Fantasia',
                  'Noir',
                  'Game_Show',
                  'Historia',
                  'Terror',
                  'Musica',
                  'Musical',
                  'Misterio',
                  'Reality_TV',
                  'Romance',
                  'Sci_Fi',
                  'Esporte',
                  'Thriller',
                  'Guerra',
                  'Velho_Oeste',
                  'idioma_grupo',
                  'usa_producao',
                  'produtoras_b6',
                  'idioma_ingles'     
                  )

######### TRATANDO VALORES EM BRANCO #########

#Quantos valores em branco eu tenho?

missing.values <- df %>%
  tidyr::gather(key = "key", value = "val") %>%
  mutate(isna = is.na(val)) %>%
  group_by(key) %>%
  mutate(total = n()) %>%
  group_by(key, total, isna) %>%
  summarise(num.isna = n()) %>%
  mutate(pct = num.isna / total * 100)


levels <-
  (missing.values  %>% filter(isna == T) %>% arrange(desc(pct)))$key


percentage.plot <- missing.values %>%
  ggplot() +
  geom_bar(aes(x = reorder(key, desc(pct)), 
               y = pct, fill=isna), 
           stat = 'identity', alpha=0.8) +
  scale_x_discrete(limits = levels) +
  scale_fill_manual(name = "", 
                    values = c('grey', 'steelblue'), labels = c("Presentes", "Faltantes")) +
  coord_flip() +
  labs(x =
         'Variavel', y = "% de valores faltantes")




percentage.plot


t <- filter(missing.values, isna == T)
t <- t %>% dplyr::select(c("key", "pct"))
t <- t[order(-t$pct),]
t <- t[2:3]
colnames(t) <- c("Variavel", "Percentual_Faltante")

t <- t %>%
mutate(Percentual = percent(Percentual_Faltante/100, accuracy = 0.2, decimal.mark = "."))

tb <- datatable(t %>% select(-c("Percentual_Faltante")),  options = list(dom = 't'))

tb


#Vamos entao excluir os filmes que nao possuem bilheteria, e ver como fica...
#bkp <- df


df <- df %>% filter(!is.na(usa_bilheteria_domestica_ajustada))


missing.values <- df %>%
  tidyr::gather(key = "key", value = "val") %>%
  mutate(isna = is.na(val)) %>%
  group_by(key) %>%
  mutate(total = n()) %>%
  group_by(key, total, isna) %>%
  summarise(num.isna = n()) %>%
  mutate(pct = num.isna / total * 100)


levels <-
  (missing.values  %>% filter(isna == T) %>% arrange(desc(pct)))$key


t <- filter(missing.values, isna == T)
t <- t %>% dplyr::select(c("key", "pct"))
t <- t[order(-t$pct),]
t <- t[2:3]
colnames(t) <- c("Variavel", "Percentual_Faltante")

t <- t %>%
  mutate(Percentual = percent(Percentual_Faltante/100, accuracy = 0.2, decimal.mark = "."))

datatable(t %>% select(-c("Percentual_Faltante")),  options = list(dom = 't'))


df %>% group_by(idioma_grupo) %>% summarise(n = n())


bkp <- df
#df <- bkp
df <- df %>% select(-c("premios_total", "sucesso_previo_atores", 
                       "notas_previas_diretores", "sucesso_previo_diretor",
                       "orcamento_ajustado"))
#SUBSTITUINDO VALORES

df <- df %>% filter(!is.na(idioma_grupo))
df <- df %>% filter(!is.na(genero_diretor))

df$notas_previas_atores[is.na(df$notas_previas_atores)] <- mean(df$notas_previas_atores, na.rm = T) 

nrow(df)
# MUDANDO TIPO DAS COLUNAS

#df$usa_bilheteria_domestica_ajustada <- as.factor(df$usa_bilheteria_domestica_ajustada)
#df[, 10:38] <- lapply(df[, 10:38], as.factor)


#### DIRETORES E ATORES PRODUZEM GERALMENTE APENAS UM FILME ####

b <-
casting %>% select(c("director_name", "id")) %>%
  group_by(director_name) %>%
  summarise(total = n())

b <- b %>% count(total)

b <- b %>% mutate(percent = percent(n/sum(n), accuracy = 0.1, decimal.mark = ","))

colnames(b) <- c("Qtd filmes", "Qtd diretores", "Percentual")

datatable(b,  options = list(dom = 'b'))

#b %>% group_by(total) %>% summarise(total_2 = count(total))


t <- casting %>% select(c("id", "actor1_name", "actor2_name", "actor3_name", "actor4_name", "actor5_name"))

t <- reshape2::melt(t, id.vars=c("id"))

t <-
  t %>% select(c("value", "id")) %>%
  group_by(value) %>%
  summarise(total = n())

t <- t %>% count(total)

t <- t %>% mutate(percent = percent(n/sum(n), accuracy = 0.1, decimal.mark = ","))

colnames(t) <- c("Qtd filmes", "Qtd atores", "Percentual")

datatable(t,  options = list(dom = 't'))


######### CORRELACAO ENTRE VARIAVEIS ######### 

bkp <- df
df <- df %>% select(-c("imdb_titulo_id"))

correlacao <- cor(df)
correlacao <- as.data.frame(correlacao)
correlacao <- tibble::rownames_to_column(correlacao, "variavel")

correlacao <- reshape2::melt(correlacao)

correlacao %>% filter(value > 0.5)

######### ANALISE PREDITIVA ######### 
install.packages("janitor")
library(janitor)

set.seed(123)

bkp <- df
#df <- bkp
df <- df %>% mutate (id = row_number())
df <- df %>% filter(!is.na(usa_bilheteria_domestica_ajustada))
df <- as.data.frame(df)

#names(df)[names(df) == 'Game-Show'] <- "Game_Show"
#names(df)[names(df) == 'Reality-TV'] <- "Reality_TV"
#names(df)[names(df) == 'Sci-Fi'] <- "Sci_Fi"

df <- df %>% select(-c("Adulto", "Documentario", "Game_Show", "Reality_TV"))
#df <- df %>% select(-c("orcamento_ajustado")) 


#df <- bkp
#df <- df %>% filter(ano > 2000)

#set.seed(123)

index <- createDataPartition(df$id, p = .8, list = FALSE, times = 1)

df <- df %>% select(-c("id"))

train <- df[index, ]
test  <- df[-index, ]

paste0("Então temos um total de ", nrow(test), " registros para o conjunto teste e ", nrow(train), " registros para o conjunto treino")


ctrlspecs <- trainControl (method = "cv", number = 10, savePredictions = "all")

#### PRIMEIRO MODELO ####

paste(names(df), collapse = " + ")


model_1 = train(usa_bilheteria_domestica_ajustada ~ ., data=train,
                method = "lm",
                trControl = ctrlspecs)

print(model_1)
summary(model_1)

model_1$finalModel

s <- summary(model_1)
s <- s$coefficients
s <- as.data.frame(s)
s <- s[order(-s$Estimate),]
s <- s %>% select(c("Estimate", "Pr(>|t|)")) 

s

result_1 <- predict(model_1, newdata = test)

ggplot(test, aes(x=predict(model_1, newdata = test), y= usa_bilheteria_domestica_ajustada)) +
  geom_point() +
  geom_abline(intercept=0, slope=1, color="red", fill="#69b3a2") +
#  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE)+
  labs(x='Valores preditos', y='Valores empíricos')


cor.test(test$usa_bilheteria_domestica_ajustada, result_1)


#### SEGUNDO MODELO ####
#install.packages("lmvar")
library(lmvar)



model_2 = train(usa_bilheteria_domestica_ajustada ~ ., data=train,
                method = "leapBackward",
                trControl = ctrlspecs,
                tuneGrid = data.frame(nvmax = 1:32))

model_2 <- lm(usa_bilheteria_domestica_ajustada ~ ., data=train)
summary(model_2)

model_2.1 <- ols_step_backward_p(model_2, prem = 0.5, details = TRUE)

model_2.2 <- lm(usa_bilheteria_domestica_ajustada ~ duracao + ano + rep_mulheres + mes + 
                  genero_diretor + notas_previas_atores + Acao + Aventura + Animacao + Crime + 
                  Drama + Familia + Fantasia + Noir + Historia + Terror + Musical + Romance + Sci_Fi + Esporte + 
                  Thriller + Guerra + Velho_Oeste + idioma_grupo + usa_producao + produtoras_b6 + idioma_ingles, data=train)


model_2.2.1 <- ols_step_backward_p(model_2.2, prem = 0.5, details = TRUE)

model_2.2 <- train(usa_bilheteria_domestica_ajustada ~ duracao + ano + rep_mulheres + mes + 
                     genero_diretor + notas_previas_atores + Acao + Aventura + Animacao + Crime + 
                     Drama + Familia + Fantasia + Noir + Historia + Terror + Musical + Romance + Sci_Fi + Esporte + 
                     Thriller + Guerra + Velho_Oeste + idioma_grupo + usa_producao + produtoras_b6 + idioma_ingles, data=train,
                   method = "lm",
                   trControl = ctrlspecs)

print(model_2.2)
summary(model_2.2)

result_2 <- predict(model_2.2, newdata = test)

ggplot(test, aes(x=predict(model_2.2, newdata = test), y= usa_bilheteria_domestica_ajustada)) +
  geom_point() +
  geom_abline(intercept=0, slope=1, color="red", fill="#69b3a2") +
  #  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE)+
  labs(x='Valores preditos', y='Valores empíricos')


cor.test(test$usa_bilheteria_domestica_ajustada, result_2)
cor.test(test$usa_bilheteria_domestica_ajustada, result_1)
