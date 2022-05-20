#Atividade3 - Luiza Vieira

library(rgbif)
library(dplyr)
library(CoordinateCleaner)
library(tidyverse)
library(ggmap)
library(maps)
library(mapdata)
install.packages("robis")
library(robis)

# checar funcoes
?occ_data

# baixar ocorrencias do gbif
p_borboleta <- occ_data(scientificName = "Chaetodon striatus", 
                      hasCoordinate = TRUE,
                      hasGeospatialIssue=FALSE)

# dimensoes
dim(p_borboleta)

dim(p_borboleta$data)

p_borboleta$data %>% names

p_borboleta <- p_borboleta$data

#checar erros

gbif_issues()

issues_gbif <- p_borboleta$issues %>%
  unique()
gbif_issues() %>%
  data.frame() %>% 
  filter(code %in% issues_gbif) 

#selecionar e investigar variaveis

p_borboleta_1 <- p_borboleta %>%
  dplyr::select(scientificName, acceptedScientificName, decimalLatitude, decimalLongitude,
                issues, waterBody, basisOfRecord, occurrenceStatus, rightsHolder, 
                datasetName, recordedBy, depth, locality, habitat) %>% 
  distinct()

lapply(p_borboleta_1, unique)

#limpar mais uma vez os dados do gbif

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


p_borboleta_1 <- p_borboleta_1 %>% 
  dplyr::filter(flags_spatial_gbif$.summary == TRUE)


#mapa mundi
world <- map_data('world')

ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = p_borboleta_1, aes(x = decimalLongitude, y = decimalLatitude), color = "red") +
  labs(x = "longitude", y = "latitude", title = expression(italic("Chaetodon striatus")))

#obis

p_borboleta_obis <- robis::occurrence("Chaetodon striatus")

names(p_borboleta_obis)

#selecionar colunar e registros unicos 

p_borboleta_obis_1 <- p_borboleta_obis %>% 
  dplyr::select(scientificName, decimalLatitude, decimalLongitude, bathymetry,
                flags, waterBody, basisOfRecord, occurrenceStatus, rightsHolder, 
                datasetName, recordedBy, depth, locality, habitat) %>% 
  distinct()

#checar problemas reportados

p_borboleta_obis_1 %>% 
  distinct(flags)

#retirar as observações em terra e verificar as demais variáveis

p_borboleta_obis_1 %>% 
  filter(!flags %in% c("on_land,no_depth","no_depth,on_land", "on_land", "on_land,depth_exceeds_bath", "depth_exceeds_bath,on_land", "no depth", "na", "depth_exceeds_bath"),
         !is.na(datasetName)) %>% 
  lapply(., unique)

#limpar mais uma vez os dados do obis

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

#mapa mundi
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = p_borboleta_obis_1, aes(x = decimalLongitude, y = decimalLatitude, color = waterBody)) +
  labs(x = "longitude", y = "latitude", title = expression(italic("Chaetodon striatus")))


#juntar os dados do gbif com o do obis

all_data <- bind_rows(p_borboleta_1 %>% 
                        mutate(repo = paste0("gbif", row.names(.))), 
                      p_borboleta_obis_1 %>% 
                        mutate(repo = paste0("obis", row.names(.)))) %>%
  column_to_rownames("repo") %>% 
  dplyr::select(decimalLongitude, decimalLatitude, depth) %>% 
  distinct() %>% 
  rownames_to_column("occ") %>% 
  separate(col = "occ", into = c("datasetName", "rn"), sep = 4) %>%
  mutate(scientificName = "Chaetodon striatus") %>% 
  dplyr::select(-rn)


#mapa com todas as ocorrencias

ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = all_data, aes(x = decimalLongitude, y = decimalLatitude, color = datasetName)) +
  #theme(legend.title = element_blank()) +
  labs(x = "longitude", y = "latitude", title = expression(italic("Chaetodon striatus")))
write.csv(all_data, "occ_GBIF-OBIS_par_hepa.csv", row.names = FALSE)


