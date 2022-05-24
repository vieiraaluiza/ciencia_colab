# Atividade final - Curso de Ferramentas de Ciencia Colaborativa e Banco de dados
## Luiza Vieira
## 23/05/2022


## Especie: Chaetodon striatus
## Objetivo: Relacionar variaveis ambientais com as ocorrencias da especie
## Metodo estatistico utilizado: Correlacao e Analise de Componentes Principais (PCA)

## Definindo um diretorio

setwd("C:/Users/luiza/Documents/Open_science/Chaetodon_striatus")
getwd()

## Carregando os pacotes que vao ser utilizados

library(raster)
library(rgdal) # Carregar shapefiles
library(maptools)
library(usdm) # Teste de Inflação de Variância (VIF) para 'stacks'
library(CoordinateCleaner)
library(spocc)
library(spThin)
library(dplyr)
library(dismo)
library(corrplot)

## Carregar as variaveis
# As variaveis foram retiradas do Bio-Oracle

Calcite.mean = raster("./Variaveis/Present.Surface.Calcite.Mean.tif")
Chlorophyll.mean = raster("./Variaveis/Present.Surface.Chlorophyll.Mean.tif")
Chlorophyll.min = raster("./Variaveis/Present.Surface.Chlorophyll.Min.tif")
Chlorophyll.range = raster("./Variaveis/Present.Surface.Chlorophyll.Range.tif")
Cloud.cover.max = raster("./Variaveis/Present.Surface.Cloud.cover.Max.tif")
Cloud.cover.mean = raster("./Variaveis/Present.Surface.Cloud.cover.Mean.tif")
Cloud.cover.min = raster("./Variaveis/Present.Surface.Cloud.cover.Min.tif")
C.Velocity.mean = raster("./Variaveis/Present.Surface.Current.Velocity.Mean.tif")
C.Velocity.min = raster("./Variaveis/Present.Surface.Current.Velocity.Min.tif")
C.Velocity.range = raster("./Variaveis/Present.Surface.Current.Velocity.Range.tif")
Diffuse.attenuation.max = raster("./Variaveis/Present.Surface.Diffuse.attenuation.Max.tif")
Diffuse.attenuation.min = raster("./Variaveis/Present.Surface.Diffuse.attenuation.Min.tif")
Dissolved.ox.max = raster("./Variaveis/Present.Surface.Dissolved.oxygen.Max.tif")
Dissolved.ox.mean = raster("./Variaveis/Present.Surface.Dissolved.oxygen.Mean.tif")
Dissolved.ox.min = raster("./Variaveis/Present.Surface.Dissolved.oxygen.Min.tif")
Dissolved.ox.range = raster("./Variaveis/Present.Surface.Dissolved.oxygen.Range.tif")
Iron.mean = raster("./Variaveis/Present.Surface.Iron.Mean.tif")
Iron.min = raster("./Variaveis/Present.Surface.Iron.Min.tif")
Nitrate.max = raster("./Variaveis/Present.Surface.Nitrate.Max.tif")
Nitrate.mean = raster("./Variaveis/Present.Surface.Nitrate.Mean.tif")
Nitrate.min = raster("./Variaveis/Present.Surface.Nitrate.Min.tif")
Nitrate.range = raster("./Variaveis/Present.Surface.Nitrate.Range.tif")
PAR.max = raster("./Variaveis/Present.Surface.Par.Max.tif")
PAR.mean = raster("./Variaveis/Present.Surface.Par.Mean.tif")
pH = raster("./Variaveis/Present.Surface.pH.tif")
Phosphate.max = raster("./Variaveis/Present.Surface.Phosphate.Max.tif")
Phosphate.mean = raster("./Variaveis/Present.Surface.Phosphate.Mean.tif")
Phosphate.min = raster("./Variaveis/Present.Surface.Phosphate.Min.tif")
Phosphate.range = raster("./Variaveis/Present.Surface.Phosphate.Range.tif")
Phytoplankton.max = raster("./Variaveis/Present.Surface.Phytoplankton.Max.tif")
Phytoplankton.mean = raster("./Variaveis/Present.Surface.Phytoplankton.Mean.tif")
Phytoplankton.min = raster("./Variaveis/Present.Surface.Phytoplankton.Min.tif")
Phytoplankton.range = raster("./Variaveis/Present.Surface.Phytoplankton.Range.tif")
Primary.Productivity.mean = raster("./Variaveis/Present.Surface.Primary.productivity.Mean.tif")
Primary.Productivity.min = raster("./Variaveis/Present.Surface.Primary.productivity.Min.tif")
Primary.Productivity.range = raster("./Variaveis/Present.Surface.Primary.productivity.Range.tif")
Salinity.max = raster("./Variaveis/Present.Surface.Salinity.Max.tif")
Salinity.mean = raster("./Variaveis/Present.Surface.Salinity.Mean.tif")
Salinity.range = raster("./Variaveis/Present.Surface.Salinity.Range.tif")
Salinity.min = raster("./Variaveis/Present.Surface.Salinity.Min.tif")
Silicate.max = raster("./Variaveis/Present.Surface.Silicate.Max.tif")
Silicate.mean = raster("./Variaveis/Present.Surface.Silicate.Mean.tif")
Silicate.min = raster("./Variaveis/Present.Surface.Silicate.Min.tif")
Temp.max = raster("./Variaveis/Present.Surface.Temperature.Max.tif")
Temp.min = raster("./Variaveis/Present.Surface.Temperature.Min.tif")
Temp.range = raster("./Variaveis/Present.Surface.Temperature.Range.tif")
Temp.mean = raster("./Variaveis/Present.Surface.Temperature.Mean.tif")

## Depois disso, e preciso limitar a extensão que ira utilizar

atlantic.ext <- extent(-100, 0, -40, 40)

## Poligono de ecorregioes do western atlantic

ecoreg <- readOGR("./Ecorregion/Atlantico_EUA_Brazil/Shape_Stega_Atlantico_EUA_Brazil.shp")

## Cortar as variaveis

Calcite.mean.ecoreg <- mask(crop(Calcite.mean, atlantic.ext), ecoreg)
Chlorophyll.mean.ecoreg <- mask(crop(Chlorophyll.mean, atlantic.ext), ecoreg)
Chlorophyll.min.ecoreg <- mask(crop(Chlorophyll.min, atlantic.ext), ecoreg)
Chlorophyll.range.ecoreg <- mask(crop(Chlorophyll.range, atlantic.ext), ecoreg)
Cloud.cover.max.ecoreg <- mask(crop(Cloud.cover.max, atlantic.ext), ecoreg)
Cloud.cover.mean.ecoreg <- mask(crop(Cloud.cover.mean, atlantic.ext), ecoreg)
Cloud.cover.min.ecoreg <- mask(crop(Cloud.cover.min, atlantic.ext), ecoreg)
C.Velocity.mean.ecoreg <- mask(crop(C.Velocity.mean, atlantic.ext), ecoreg)
C.Velocity.min.ecoreg <- mask(crop(C.Velocity.min, atlantic.ext), ecoreg)
C.Velocity.range.ecoreg <- mask(crop(C.Velocity.range, atlantic.ext), ecoreg)
Diffuse.attenuation.max.ecoreg <- mask(crop(Diffuse.attenuation.max, atlantic.ext), ecoreg)
Diffuse.attenuation.min.ecoreg <- mask(crop(Diffuse.attenuation.min, atlantic.ext), ecoreg)
Dissolved.ox.max.ecoreg <- mask(crop(Dissolved.ox.max, atlantic.ext), ecoreg)
Dissolved.ox.mean.ecoreg <- mask(crop(Dissolved.ox.mean, atlantic.ext), ecoreg)
Dissolved.ox.min.ecoreg <- mask(crop(Dissolved.ox.min, atlantic.ext), ecoreg)
Dissolved.ox.range.ecoreg <- mask(crop(Dissolved.ox.range, atlantic.ext), ecoreg)
Iron.mean.ecoreg <- mask(crop(Iron.mean, atlantic.ext), ecoreg)
Iron.min.ecoreg <- mask(crop(Iron.min, atlantic.ext), ecoreg)
Nitrate.max.ecoreg <- mask(crop(Nitrate.max, atlantic.ext), ecoreg)
Nitrate.mean.ecoreg <- mask(crop(Nitrate.mean, atlantic.ext), ecoreg)
Nitrate.min.ecoreg <- mask(crop(Nitrate.min, atlantic.ext), ecoreg)
Nitrate.range.ecoreg <- mask(crop(Nitrate.range, atlantic.ext), ecoreg)
PAR.max.ecoreg <- mask(crop(PAR.max, atlantic.ext), ecoreg)
PAR.mean.ecoreg <- mask(crop(PAR.mean, atlantic.ext), ecoreg)
pH.ecoreg <- mask(crop(pH, atlantic.ext), ecoreg)
Phosphate.max.ecoreg <- mask(crop(Phosphate.max, atlantic.ext), ecoreg)
Phosphate.mean.ecoreg <- mask(crop(Phosphate.mean, atlantic.ext), ecoreg)
Phosphate.min.ecoreg <- mask(crop(Phosphate.min, atlantic.ext), ecoreg)
Phosphate.range.ecoreg <- mask(crop(Phosphate.range, atlantic.ext), ecoreg)
Phytoplankton.max.ecoreg <- mask(crop(Phytoplankton.max, atlantic.ext), ecoreg)
Phytoplankton.mean.ecoreg <- mask(crop(Phytoplankton.mean, atlantic.ext), ecoreg)
Phytoplankton.min.ecoreg <- mask(crop(Phytoplankton.min, atlantic.ext), ecoreg)
Phytoplankton.range.ecoreg <- mask(crop(Phytoplankton.range, atlantic.ext), ecoreg)
Primary.Productivity.mean.ecoreg <- mask(crop(Primary.Productivity.mean, atlantic.ext), ecoreg)
Primary.Productivity.min.ecoreg <- mask(crop(Primary.Productivity.min, atlantic.ext), ecoreg)
Primary.Productivity.range.ecoreg <- mask(crop(Primary.Productivity.range, atlantic.ext), ecoreg)
Salinity.max.ecoreg <- mask(crop(Salinity.max, atlantic.ext), ecoreg)
Salinity.mean.ecoreg <- mask(crop(Salinity.mean, atlantic.ext), ecoreg)
Salinity.range.ecoreg <- mask(crop(Salinity.range, atlantic.ext), ecoreg)
Salinity.min.ecoreg <- mask(crop(Salinity.min, atlantic.ext), ecoreg)
Silicate.max.ecoreg <- mask(crop(Silicate.max, atlantic.ext), ecoreg)
Silicate.mean.ecoreg <- mask(crop(Silicate.mean, atlantic.ext), ecoreg)
Silicate.min.ecoreg <- mask(crop(Silicate.min, atlantic.ext), ecoreg)
Temp.max.ecoreg <- mask(crop(Temp.max, atlantic.ext), ecoreg)
Temp.min.ecoreg <- mask(crop(Temp.min, atlantic.ext), ecoreg)
Temp.range.ecoreg <- mask(crop(Temp.range, atlantic.ext), ecoreg)
Temp.mean.ecoreg <- mask(crop(Temp.mean, atlantic.ext), ecoreg)

## Comparar as variaveis

compareRaster(Temp.mean.ecoreg, Temp.range.ecoreg, Temp.min.ecoreg, Temp.max.ecoreg, Silicate.min.ecoreg, Silicate.mean.ecoreg, Silicate.max.ecoreg, Salinity.min.ecoreg, Salinity.range.ecoreg, Salinity.mean.ecoreg, Salinity.max.ecoreg, Primary.Productivity.range.ecoreg, Primary.Productivity.min.ecoreg, Primary.Productivity.mean.ecoreg, Phytoplankton.range.ecoreg, Phytoplankton.min.ecoreg, Primary.Productivity.mean.ecoreg, Phytoplankton.max.ecoreg, Phosphate.range.ecoreg, Phosphate.min.ecoreg, Phosphate.mean.ecoreg, Phosphate.max.ecoreg, pH.ecoreg, PAR.mean.ecoreg, PAR.max.ecoreg, Nitrate.range.ecoreg, Nitrate.min.ecoreg, Nitrate.mean.ecoreg, Nitrate.max.ecoreg, Iron.min.ecoreg, Iron.mean.ecoreg, Dissolved.ox.range.ecoreg, Dissolved.ox.min.ecoreg, Dissolved.ox.mean.ecoreg, Dissolved.ox.max.ecoreg, Diffuse.attenuation.min.ecoreg, Diffuse.attenuation.max.ecoreg, C.Velocity.range.ecoreg, C.Velocity.min.ecoreg, C.Velocity.mean.ecoreg, Calcite.mean.ecoreg, Chlorophyll.mean.ecoreg, Chlorophyll.min.ecoreg, Chlorophyll.range.ecoreg, Cloud.cover.max.ecoreg, Cloud.cover.mean.ecoreg, Cloud.cover.min.ecoreg)

## Criar um stack com as variaveis

Variaveis_1 <- stack(Temp.mean.ecoreg, Temp.range.ecoreg, Temp.min.ecoreg, Temp.max.ecoreg, Silicate.min.ecoreg, Silicate.mean.ecoreg, Silicate.max.ecoreg, Salinity.min.ecoreg, Salinity.range.ecoreg, Salinity.mean.ecoreg, Salinity.max.ecoreg, Primary.Productivity.range.ecoreg, Primary.Productivity.min.ecoreg, Primary.Productivity.mean.ecoreg, Phytoplankton.range.ecoreg, Phytoplankton.min.ecoreg, Primary.Productivity.mean.ecoreg, Phytoplankton.max.ecoreg, Phosphate.range.ecoreg, Phosphate.min.ecoreg, Phosphate.mean.ecoreg, Phosphate.max.ecoreg, pH.ecoreg, PAR.mean.ecoreg, PAR.max.ecoreg, Nitrate.range.ecoreg, Nitrate.min.ecoreg, Nitrate.mean.ecoreg, Nitrate.max.ecoreg, Iron.min.ecoreg, Iron.mean.ecoreg, Dissolved.ox.range.ecoreg, Dissolved.ox.min.ecoreg, Dissolved.ox.mean.ecoreg, Dissolved.ox.max.ecoreg, Diffuse.attenuation.min.ecoreg, Diffuse.attenuation.max.ecoreg, C.Velocity.range.ecoreg, C.Velocity.min.ecoreg, C.Velocity.mean.ecoreg, Calcite.mean.ecoreg, Chlorophyll.mean.ecoreg, Chlorophyll.min.ecoreg, Chlorophyll.range.ecoreg, Cloud.cover.max.ecoreg, Cloud.cover.mean.ecoreg, Cloud.cover.min.ecoreg)

## Serve para se poder reproduzir os resultados dos geradores de números pseudo-aleatórios

set.seed(1963) 

## Verificar se as variaveis sao colineares por meio do fator de inflação da variância (VIF)
# vircor (threshold = 0.7)

vifcor(Variaveis_1,th = 0.7)

#virstep (threshold = 10)

vifstep(Variaveis_1, th = 10)

# A partir dessa analise, vao ser excluidas as variaveis com multicolinearidade
# Foi escolhido o corte do vifstep

Variaveis_2 <- vifstep(Variaveis_1, th = 10)

Variaveis_2 <- exclude(Variaveis_1, Variaveis_2)

## Carregar pacotes para a segunda etapa

library(rgbif)
library(robis)
library(tidyverse)
library(ggmap)
library(maps)
library(mapdata)


## Baixar as ocorrencias da especie por meio da base de dado GBIF

p_borboleta <- occ_data(scientificName = "Chaetodon striatus", 
                      hasCoordinate = TRUE,
                      hasGeospatialIssue=FALSE)

## Dimensoes

dim(p_borboleta)

dim(p_borboleta$data)

p_borboleta$data %>% names

p_borboleta <- p_borboleta$data

## Checar os possiveis erros nos dados

gbif_issues()

issues_gbif <- p_borboleta$issues %>%
  unique()
gbif_issues() %>%
  data.frame() %>% 
  filter(code %in% issues_gbif) 

# Selecionar as variaveis que irei usar

p_borboleta_1 <- p_borboleta %>%
  dplyr::select(scientificName, decimalLatitude, decimalLongitude) %>% 
  distinct()

lapply(p_borboleta_1, unique)

# Limpar mais uma vez os dados

flags_spatial_gbif <- CoordinateCleaner::clean_coordinates(
  x = p_borboleta_1, 
  species = "scientificName",
  lon = "decimalLongitude", 
  lat = "decimalLatitude",
  tests = c("capitals", # raio ao redor de capitais
            "centroids", # raio ao redor de centroides de paises e provincias
            "duplicates", # duplicatas
            "equal", # coordenadas iguais
            "gbif", # raio ao redor da sede da GBIF
            "institutions", # raio ao redor de instituicoes de pesquisa em biodiversidade
            "urban", # pontos dentro de areas urbanas
            "validity", # ponto de fora do sistema de coordenadas
            "zeros" # zeros e pontos onde lat = lon 
  )
)


## Limpar de fato quais que ficaram com flags

p_borboleta_1 <- p_borboleta_1 %>% 
  dplyr::filter(flags_spatial_gbif$.summary == TRUE)


## Plotar para ver como vai ficar

ggplot() +
  geom_polygon(data = ecoreg, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = p_borboleta_1, aes(x = decimalLongitude, y = decimalLatitude), color = "red") +
  labs(x = "longitude", y = "latitude", title = expression(italic("Chaetodon striatus")))

##  Baixar as ocorrencias da especie por meio da base de dado OBIS

p_borboleta_obis <- robis::occurrence("Chaetodon striatus")

names(p_borboleta_obis)

## Selecionar as colunas de interesse

p_borboleta_obis_1 <- p_borboleta_obis %>% 
  dplyr::select(scientificName, decimalLatitude, decimalLongitude, bathymetry,
                flags, waterBody, basisOfRecord, occurrenceStatus, rightsHolder, 
                datasetName, recordedBy, depth, locality, habitat) %>% 
  distinct()

## Checar problemas reportados

p_borboleta_obis_1 %>% 
  distinct(flags)

## Retirar as observações em terra e verificar as demais variáveis

p_borboleta_obis_1 %>% 
  filter(!flags %in% c("on_land,no_depth","no_depth,on_land", "on_land", "on_land,depth_exceeds_bath", "depth_exceeds_bath,on_land", "no depth", "na", "depth_exceeds_bath"),
         !is.na(datasetName)) %>% 
  lapply(., unique)

## Limpar mais uma vez os dados do OBIS

flags_spatial_obis <- CoordinateCleaner::clean_coordinates(
  x = p_borboleta_obis_1, 
  species = "scientificName",
  lon = "decimalLongitude", 
  lat = "decimalLatitude",
  tests = c("capitals", # raio ao redor de capitais
            "centroids", # raio ao redor de centroides de paises e provincias
            "duplicates", # duplicatas
            "equal", # coordenadas iguais
            "gbif", # raio ao redor da sede da GBIF
            "institutions", # raio ao redor de instituicoes de pesquisa em biodiversidade
            "urban", # pontos dentro de areas urbanas
            "validity", # ponto de fora do sistema de coordenadas
            "zeros" # zeros e pontos onde lat = lon 
  )
)

p_borboleta_obis_1 <- p_borboleta_obis_1 %>% 
  dplyr::filter(flags_spatial_obis$.summary == TRUE)

## Selecionar apenas as variaveis de interesse (apenas as coordenadas)

p_borboleta_obis_1 <- p_borboleta_obis %>% 
  dplyr::select(scientificName, decimalLatitude, decimalLongitude) %>% 
  distinct()


## Plotar

ggplot() +
  geom_polygon(data = ecoreg, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = p_borboleta_obis_1, aes(x = decimalLongitude, y = decimalLatitude)) +
  labs(x = "longitude", y = "latitude", title = expression(italic("Chaetodon striatus")))


# Juntar os dados do gbif com o do obis

all_data <- bind_rows(p_borboleta_1 %>% 
                        mutate(repo = paste0("gbif", row.names(.))), 
                      p_borboleta_obis_1 %>% 
                        mutate(repo = paste0("obis", row.names(.)))) %>%
  column_to_rownames("repo") %>% 
  dplyr::select(decimalLongitude, decimalLatitude) %>% 
  distinct() %>% 
  rownames_to_column("occ") %>% 
  separate(col = "occ", into = c("datasetName", "rn"), sep = 4) %>%
  mutate(scientificName = "Chaetodon striatus") %>% 
  dplyr::select(-rn)

all_data

## spthin

dir.create("./thinned")

all_data_thin <- 
  thin( loc.data = all_data, 
        lat.col = "decimalLatitude", long.col = "decimalLongitude", 
        spec.col = "scientificName", 
        thin.par = 20 #distancia em km que os registros vao ficar separados#  
        ,reps = 5, 
        locs.thinned.list.return = TRUE, 
        write.files = TRUE, 
        max.files = 2, 
        out.dir = "./thinned", out.base = "p_borboleta_thinned", 
        write.log.file = TRUE,
        log.file = "./thinned/p_borboleta_thinned_log.txt")

plotThin(all_data_thin)

all_data_thin = read.csv("./thinned/p_borboleta_thinned_thin1.csv", sep=",")
View(all_data_thin)

## Limpar os registros fora do western atlantic

c(all_data_thin$decimalLongitude)

print(all_data_thin)

all_data_thin_r <- all_data_thin[-c(127, 259),]


# Mapa com todas as ocorrencias

ggplot() +
  geom_polygon(data = ecoreg, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = all_data_thin_r, aes(x = decimalLongitude, y = decimalLatitude)) +
  #theme(legend.title = element_blank()) +
  labs(x = "longitude", y = "latitude", title = expression(italic("Chaetodon striatus")))
write.csv(all_data, "occ_GBIF-OBIS_par_hepa.csv", row.names = FALSE)


## PCA
# Carregar os pacotes necessarios

library(sp)
library(rgdal)
library(ggplot2)
install.packages("ggbiplot")
install.packages("devtools")
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)

### Transformar em Spatial

coordinates(all_data_thin_r) <- ~ decimalLongitude+decimalLatitude
class(all_data_thin_r)
plot(all_data_thin_r)

all_data_thin_r

proj4string (all_data_thin_r) <- CRS("+proj=longlat +datum=WGS84 +no_defs")


## PCA

valores_var_Chaetodonstriatus <- data.frame(raster::extract(Variaveis_2, all_data_thin_r))
View(valores_var_Chaetodonstriatus)

c(valores_var_Chaetodonstriatus)

valores_semNA = valores_var_Chaetodonstriatus %>% tidyr::drop_na(Present.Surface.Temperature.Range, Present.Surface.Temperature.Max, Present.Surface.Silicate.Min, Present.Surface.Salinity.Range, Present.Surface.Nitrate.Range, Present.Surface.Par.Max, Present.Surface.Par.Mean, Present.Surface.pH, Present.Surface.Phosphate.Min, Present.Surface.Phosphate.Range, Present.Surface.Primary.productivity.Min, Present.Surface.Primary.productivity.Range, Present.Surface.Salinity.Max, Present.Surface.Nitrate.Min, Present.Surface.Iron.Mean, Present.Surface.Diffuse.attenuation.Max, Present.Surface.Current.Velocity.Range, Present.Surface.Current.Velocity.Min, Present.Surface.Calcite.Mean, Present.Surface.Cloud.cover.Max, Present.Surface.Cloud.cover.Min)

PCA_1 = prcomp(valores_semNA, center = TRUE, scale. = TRUE)
PCA_1

attributes(PCA_1)
summary(PCA_1)

g_PCA_1= ggbiplot(PCA_1, obs.scale = 0.5, var.scale = 5, ellipse = FALSE, alpha = 0.1, varname.size = 2.5, varname.adjust = 2.5, var.axes = TRUE, varname.abbrev = TRUE)

g_PCA_1= g_PCA_1+theme_bw()

g_PCA_1<-g_PCA_1 + scale_color_discrete(name = '')
g_PCA_1<-g_PCA_1 + theme(legend.direction = 'horizontal',legend.position = 'top')

plot(g_PCA_1)




