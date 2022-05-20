# Atividade 1 - Luiza Vieira

setwd("C:/Users/luiza/Documents/Open_Science")
getwd()

library(readr)
library(tidyverse)
library(taxize)
install.packages("DataCombine")
library(DataCombine)

#baixando os csv

Atividade1_LuizaVieira = read.csv("./atividade1_LuizaVieira.csv", header=T, sep = ";")

Atividade1_VictorLupinacci = read.csv("./atividade1_VictorLupinacci.csv", header=T, sep = ";")

Atividade1_AnaClara = read.csv("./atividade1_AnaClara.csv", header=T, sep = ",")

Atividade1_MarinaMega = read.csv("./atividade1_MarinaMega.csv", header=T, sep = ";")

Atividade1_NataliaMelo = read.csv("./atividade1_NataliaMelo.csv", header=T, sep = ";")

Atividade1_LeticiaEvangelista = read.csv("./atividade1_LeticiaEvangelista.csv", header=T, sep = ";")

Atividade1_MarianaFaitanin = read.csv("./atividade1_MarianaFaitanin.csv", header=T, sep = ";")

#igualando as colunas

##victor
head(Atividade1_VictorLupinacci)

Atividade1_VictorLupinacci = rename(Atividade1_VictorLupinacci, Amostra2 = AmostraFicha, Amostra = AmostraArq, Sepal_length_cm = SepLength, Sepal_width_cm = SepWidth, Petal_length_cm = PetLength, Petal_width_cm = PetWidth, Species = Especie, Data = Dia)

Atividade1_VictorLupinacci$Latitude <-sapply(Atividade1_VictorLupinacci$Latitude, gsub, pattern = "\\.", replacement= "")
Atividade1_VictorLupinacci$Latitude <- as.numeric(Atividade1_VictorLupinacci$Latitude)

Atividade1_VictorLupinacci$Longitude <-sapply(Atividade1_VictorLupinacci$Longitude, gsub, pattern = "\\.", replacement= "")
Atividade1_VictorLupinacci$Longitude <- as.numeric(Atividade1_VictorLupinacci$Longitude)


Atividade1_VictorLupinacci$Latitude <- format(Atividade1_VictorLupinacci$Latitude, digits=8,
                                              big.mark=".",small.mark=".", big.interval=6)

Atividade1_VictorLupinacci$Longitude <- format(Atividade1_VictorLupinacci$Longitude, digits=8,
                                               big.mark=".",small.mark=".", big.interval=6)

Atividade1_VictorLupinacci$Species <- sapply(Atividade1_VictorLupinacci$Species, gsub, pattern = "_", replacement= " ")

Atividade1_VictorLupinacci$Site <- as.numeric(Atividade1_VictorLupinacci$Site)

##ana
head(Atividade1_AnaClara)

Atividade1_AnaClara = rename(Atividade1_AnaClara, Amostra2 = amostra, Amostra = amostra_cod, Sepal_length_cm = sepal.lenght..cm., Sepal_width_cm = sepal.width..cm., Petal_length_cm = petal.length..cm., Petal_width_cm = petal.width..cm., Species = espÃ.cie, Data = data, Longitude = longitude, Latitude = latitude, Site = site)

Atividade1_AnaClara$Latitude <- sapply(Atividade1_AnaClara$Latitude, gsub, pattern = "\\.", replacement= "")
Atividade1_AnaClara$Latitude <- as.numeric(Atividade1_AnaClara$Latitude)

Atividade1_AnaClara$Longitude <- sapply(Atividade1_AnaClara$Longitude, gsub, pattern = "\\.", replacement= "")
Atividade1_AnaClara$Longitude <- as.numeric(Atividade1_AnaClara$Longitude)


Atividade1_AnaClara$Latitude <- format(Atividade1_AnaClara$Latitude, digits=8,
                                              big.mark=".",small.mark=".", big.interval=6)

Atividade1_AnaClara$Longitude <- format(Atividade1_AnaClara$Longitude, digits=8,
                                               big.mark=".",small.mark=".", big.interval=6)


Atividade1_AnaClara$Data <- factor(Atividade1_AnaClara$Data) %>% 
  as.Date(format = "%Y-%m-%d") %>% 
  format("%d/%m/%Y")

Atividade1_AnaClara$Site <- as.numeric(Atividade1_AnaClara$Site)

##leticia
head(Atividade1_LeticiaEvangelista)

Atividade1_LeticiaEvangelista = rename(Atividade1_LeticiaEvangelista, Amostra2 = Amostra, Amostra = Amostra.arq., Sepal_length_cm = Sepal.length..cm., Sepal_width_cm = Sepal.width..cm., Petal_length_cm = Petal.length..cm., Petal_width_cm = Petal.width..cm., Species = Espécies)

Atividade1_LeticiaEvangelista$Latitude <- sapply(Atividade1_LeticiaEvangelista$Latitude, gsub, pattern = "\\.", replacement= "")
Atividade1_LeticiaEvangelista$Latitude <- as.numeric(Atividade1_LeticiaEvangelista$Latitude)

Atividade1_LeticiaEvangelista$Longitude <- sapply(Atividade1_LeticiaEvangelista$Longitude, gsub, pattern = "\\.", replacement= "")
Atividade1_LeticiaEvangelista$Longitude <- as.numeric(Atividade1_LeticiaEvangelista$Longitude)

Atividade1_LeticiaEvangelista$Latitude <- format(Atividade1_LeticiaEvangelista$Latitude, digits=8,
                                       big.mark=".",small.mark=".", big.interval=6)

Atividade1_LeticiaEvangelista$Longitude <- format(Atividade1_LeticiaEvangelista$Longitude, digits=8,
                                        big.mark=".",small.mark=".", big.interval=6)

Atividade1_LeticiaEvangelista$Site <- as.numeric(Atividade1_LeticiaEvangelista$Site)


##luiza
head(Atividade1_LuizaVieira)

Atividade1_LuizaVieira = rename(Atividade1_LuizaVieira, Species = Especie, Amostra = ï..Amostra)

Atividade1_LuizaVieira$Latitude <- as.numeric(Atividade1_LuizaVieira$Latitude)
Atividade1_LuizaVieira$Longitude <- as.numeric(Atividade1_LuizaVieira$Longitude)
Atividade1_LuizaVieira$Site <- as.numeric(Atividade1_LuizaVieira$Site)


##mariana
head(Atividade1_MarianaFaitanin)

Atividade1_MarianaFaitanin = rename(Atividade1_MarianaFaitanin, Amostra = Nome.do.arquivo, Data = date, Amostra2 = Amostra, Species = Espécie, Site = Área, Sepal_length_cm = Sepal.length..cm., Sepal_width_cm = Sepal.width..cm., Petal_length_cm = Petal.length..cm., Petal_width_cm = Petal.width..cm. )

Atividade1_MarianaFaitanin$Site <- gsub("Site", "", as.character(Atividade1_MarianaFaitanin$Site))

Atividade1_MarianaFaitanin$Latitude <- as.numeric(Atividade1_MarianaFaitanin$Latitude)
Atividade1_MarianaFaitanin$Longitude <- as.numeric(Atividade1_MarianaFaitanin$Longitude)
Atividade1_MarianaFaitanin$Site <- as.numeric(Atividade1_MarianaFaitanin$Site)


#marina
head(Atividade1_MarinaMega)

Atividade1_MarinaMega = rename(Atividade1_MarinaMega, Amostra = sample_pdf, Data = date, Amostra2 = sample_card, Species = specie, Site = site, Sepal_length_cm = sepal_lenght_cm, Sepal_width_cm = sepal_widht_cm, Petal_length_cm = petal_lenght_cm, Petal_width_cm = petal_width_cm, Latitude = latitude, Longitude = longitude)

Atividade1_MarinaMega$Latitude <- sapply(Atividade1_MarinaMega$Latitude, gsub, pattern = "\\.", replacement= "")
Atividade1_MarinaMega$Longitude <- sapply(Atividade1_MarinaMega$Longitude, gsub, pattern = "\\.", replacement= "")

Atividade1_MarinaMega$Longitude <- as.numeric(Atividade1_MarinaMega$Longitude)
Atividade1_MarinaMega$Latitude <- as.numeric(Atividade1_MarinaMega$Latitude)

Atividade1_MarinaMega$Latitude <- format(Atividade1_MarinaMega$Latitude, digits=8,
                                                 big.mark=".",small.mark=".", big.interval=6)

Atividade1_MarinaMega$Longitude <- format(Atividade1_MarinaMega$Longitude, digits=8,
                                                  big.mark=".",small.mark=".", big.interval=6)

Atividade1_MarinaMega$Amostra2 <- gsub("a", "A", as.character(Atividade1_MarinaMega$Amostra2))

Atividade1_MarinaMega$Amostra <- gsub("a", "A", as.character(Atividade1_MarinaMega$Amostra))

Atividade1_MarinaMega$Site <- as.numeric(Atividade1_MarinaMega$Site)


#natalia
head(Atividade1_NataliaMelo)

Atividade1_NataliaMelo = rename(Atividade1_NataliaMelo, Amostra = sample, Data = date, Amostra2 = id.ficha, Species = specie, Site = site, Sepal_length_cm = sepal.lenght, Sepal_width_cm = sepal.width, Petal_length_cm = petal.length, Petal_width_cm = petal.width, Latitude = latitude, Longitude = longitude)

Atividade1_NataliaMelo$Latitude <- as.numeric(Atividade1_NataliaMelo$Latitude)
Atividade1_NataliaMelo$Longitude <- as.numeric(Atividade1_NataliaMelo$Longitude)

Atividade1_NataliaMelo$Latitude <- format(Atividade1_NataliaMelo$Latitude, digits=8,
                                         big.mark=".",small.mark=".", big.interval=6)

Atividade1_NataliaMelo$Longitude <- format(Atividade1_NataliaMelo$Longitude, digits=8,
                                          big.mark=".",small.mark=".", big.interval=6)

Atividade1_NataliaMelo$Species <- gsub("iris", "Iris", as.character(Atividade1_NataliaMelo$Species))

Atividade1_NataliaMelo$Site <- as.numeric(Atividade1_NataliaMelo$Site)


#juntar os dataframes


dados.finais <- bind_rows(Atividade1_AnaClara, Atividade1_LeticiaEvangelista, Atividade1_LuizaVieira, Atividade1_MarianaFaitanin, Atividade1_MarinaMega, Atividade1_NataliaMelo, Atividade1_VictorLupinacci)

head(dados.finais)

dados.finais$ï..Amostra <- NULL
dados.finais$Sepal.width.cm <- NULL
dados.finais$Petal.length.cm <- NULL
dados.finais$Petal.width.cm <- NULL
dados.finais$Sepal.length.cm <- NULL
dados.finais$Especie <- NULL

DropNA(dados.finais, message = TRUE)

write.csv(dados.finais, file = "Atividade1.csv")


