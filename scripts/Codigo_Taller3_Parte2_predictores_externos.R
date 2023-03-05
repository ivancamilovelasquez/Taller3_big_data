#_____________________________________________________________________________#
#                                                                             #
#                      Problem Set 3: Making Money with ML?                     #
# 
#
# Código predictores externos
#_____________________________________________________________________________#

#   Autores: - Jorge Rodríguez                                                  
#            - Iván Velázquez  
#            - Santiago Gonzalez
#            - Maria Jose Colmenares
#
#  Fecha: 12/03/2023 


# - Librerias y paquetes 

library(pacman)
p_load(tidyverse, rstudioapi, rio, leaflet, rgeos, tmaptools, sf, stargazer,osmdata)


# 2.1 Definir colores

color <- rep(NA, nrow(train))
color[train$casa == 1] <- "#012a4a"
color[train$apto_total == 1] <- "#a9d6e5"
  
color_test <- rep(NA, nrow(test))
color_test[test$casa == 1] <- "#012a4a"
color_test[test$apto_total == 1] <- "#a9d6e5"    


# 2.2 Rescalar por año: circulos mas grandes son más nuevos

r <- train$year
r_std <- (r - min(r))/(max(r) - min(r)) 
max = 20
min = 1
r_scaled <- r_std * (max - min) + min
html_popup <-  paste0("Año", train$year )


# 2.3 Gráfica 1 

limites <- getbb("Bogota Colombia")
filtro1 <- between(train$lon, limites[1, "min"], limites[1, "max"])
filtro2 <- between(train$lat, limites[2, "min"], limites[2, "max"])
filtro <- filtro1 & filtro2
train <- train[filtro,] 
# Observamos la primera visualización
leaflet() %>%
  addTiles() %>%
  addCircles(lng = train$lon, 
             lat = train$lat,
             fillOpacity = 1,
             col = color,
             radius = r_scaled,
             popup = html_popup,
             opacity = 1)

## Nota: preguntar si year es el ano se creo el apartamento
# Info externa 

parques <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "leisure" , value = "park") 
parques_sf <- osmdata_sf(parques)
parques_geometria <- parques_sf$osm_polygons %>% 
  select(osm_id, name)


available_tags("leisure")


