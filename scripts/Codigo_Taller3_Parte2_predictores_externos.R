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
p_load(tidyverse, rstudioapi, rio, leaflet, rgeos, tmaptools, sf, stargazer,osmdata, plotly)


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
html_popup <-  paste0("<b>Año: </b>", train$year,
                      "<b>Area: </b>", train$area_maxima,
                      "<b>Precio: </b>", scales::dollar(train$price))

r_test <- test$year
r_std_test <- (r_test - min(r_test))/(max(r_test) - min(r_test)) 
r_scaled_test <- r_std_test * (max - min) + min
html_popup_test <-  paste0("<b>Año: </b>", train$year,
                           "<b>Area: </b>", train$area_maxim)



# 2.3 Gráfica  : Train

limites <- getbb("Bogota Colombia")
filtro1 <- between(train$lon, limites[1, "min"], limites[1, "max"])
filtro2 <- between(train$lat, limites[2, "min"], limites[2, "max"])
filtro <- filtro1 & filtro2
train <- train[filtro,] 


grfica_train <-  leaflet() %>%
                  addTiles() %>%
                  addCircles(lng = train$lon, 
                  lat = train$lat,
                  fillOpacity = 1,
                  col = color,
                  radius = r_scaled,
                  popup = html_popup,
                  opacity = 1)

htmlwidgets::saveWidget(grfica_train, "views//train.html")


# 2.4 Gráfica  : test

limites <- getbb("Bogota Colombia")
filtro1 <- between(test$lon, limites[1, "min"], limites[1, "max"])
filtro2 <- between(test$lat, limites[2, "min"], limites[2, "max"])
filtro <- filtro1 & filtro2
train <- train[filtro,] 


grfica_train <-  leaflet() %>%
  addTiles() %>%
  addCircles(lng = train$lon, 
             lat = train$lat,
             fillOpacity = 1,
             col = color,
             radius = r_scaled_test,
             popup = html_popup_test,
             opacity = 1)

htmlwidgets::saveWidget(grfica_train, "views//test.html")

## Nota: preguntar si year es el ano se creo el apartamento


available_tags("leisure")
available_tags("amenity")


# 3.1 Info externa : parques 

parques <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "leisure" , value = "park") 

fitness_centre <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "leisure" , value = "fitness_centre")


playground <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "leisure" , value = "playground")


horse_riding <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "leisure" , value = "horse_riding")


sports_centre <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "leisure" , value = "sports_centre")


hospital <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "amenity" , value = "hospital")


clinic<- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "amenity" , value = "clinic")


bus_station<- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "amenity" , value = "bus_station")


police<- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "amenity" , value = "police")


pub<- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "amenity" , value = "pub")


school<- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "amenity" , value = "school")


restaurant<- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "amenity" , value = "restaurant")


cinema<- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "amenity" , value = "restaurant")



# 3.2 graficar variables externas

parques_sf <- osmdata_sf(parques)
parques_geometria <- parques_sf$osm_polygons %>% 
  select(osm_id, name)


fitness_centre_sf <- osmdata_sf(fitness_centre)
fitness_centre_geometria <- fitness_centre_sf$osm_polygons %>% 
  select(osm_id, name)


playground_sf <- osmdata_sf(playground)
playground_geometria <- playground_sf$osm_polygons %>% 
  select(osm_id, name)


horse_riding_sf <- osmdata_sf(horse_riding)
horse_riding_geometria <- horse_riding_sf$osm_polygons %>% 
  select(osm_id, name)


sports_centre_sf <- osmdata_sf(sports_centre)
sports_centre_geometria <- sports_centre_sf$osm_polygons %>% 
  select(osm_id, name)


hospital_sf <- osmdata_sf(hospital)
hospital_geometria <- hospital_sf$osm_polygons %>% 
  select(osm_id, name)


clinic_sf <- osmdata_sf(clinic)
clinic_geometria <- clinic_sf$osm_polygons %>% 
  select(osm_id, name)


bus_station_sf <- osmdata_sf(bus_station)
bus_station_geometria <- bus_station_sf$osm_polygons %>% 
  select(osm_id, name)


police_sf <- osmdata_sf(police)
police_geometria <- police_sf$osm_polygons %>% 
  select(osm_id, name)


pub_sf <- osmdata_sf(pub)
pub_geometria <- pub_sf$osm_polygons %>% 
  select(osm_id, name)


school_sf <- osmdata_sf(school)
school_geometria <- school_sf$osm_polygons %>% 
  select(osm_id, name)


cinema_sf <- osmdata_sf(cinema)
cinema_geometria <- cinema_sf$osm_polygons %>% 
  select(osm_id, name)


restaurant_sf <- osmdata_sf(restaurant)
restaurant_geometria <- restaurant_sf$osm_polygons %>% 
  select(osm_id, name)


centroides <- gCentroid(as(parques_geometria$geometry, "Spatial"), byid = T)
centroides2 <- gCentroid(as(fitness_centre_geometria$geometry, "Spatial"), byid = T)
centroides3 <- gCentroid(as(playground_geometria$geometry, "Spatial"), byid = T)
centroides4 <- gCentroid(as(horse_riding_geometria$geometry, "Spatial"), byid = T)
centroides5 <- gCentroid(as(sports_centre_geometria$geometry, "Spatial"), byid = T)


# Graficas 

# Parques 
parques <- leaflet() %>%
            addTiles() %>%
            addPolygons(data = parques_geometria, col = "#003f88",
              opacity = 0.8, popup = parques_geometria$name) %>% 
            addCircles(lng = centroides$x, 
              lat = centroides$y, 
              col = "#ffd500", opacity = 1, radius = 1) 
htmlwidgets::saveWidget(parques, "views//parques.html")  
  

#fitness_centre_sf
leaflet() %>%
  addTiles() %>%
  addPolygons(data = fitness_centre_geometria, col = "green",
              opacity = 0.8, popup = fitness_centre_geometria$name) %>% 
  addCircles(lng = centroides$x, 
             lat = centroides$y, 
             col = "red", opacity = 1, radius = 1) 



#playground_sf
leaflet() %>%
  addTiles() %>%
  addPolygons(data = playground_geometria, col = "green",
              opacity = 0.8, popup = playground_geometria$name) %>% 
  addCircles(lng = centroides$x, 
             lat = centroides$y, 
             col = "red", opacity = 1, radius = 1)



#sports_centre_geometria
leaflet() %>%
  addTiles() %>%
  addPolygons(data = sports_centre_geometria, col = "green",
              opacity = 0.8, popup = sports_centre_geometria$name) %>% 
  addCircles(lng = centroides$x, 
             lat = centroides$y, 
             col = "red", opacity = 1, radius = 1)



#hospitales
leaflet() %>%
  addTiles() %>%
  addPolygons(data = hospital_geometria, col = "green",
              opacity = 0.8, popup = hospital_geometria$name) 
  


#clinic
leaflet() %>%
  addTiles() %>%
  addPolygons(data = clinic_geometria, col = "green",
              opacity = 0.8, popup = clinic_geometria$name) 



#bus_station_geometria
leaflet() %>%
  addTiles() %>%
  addPolygons(data = bus_station_geometria, col = "green",
              opacity = 0.8, popup = bus_station_geometria$name)




# 3.3 Convertir data espacial 

train_sf <- st_as_sf(train, coords = c("lon", "lat"))
st_crs(train_sf) <- 4326
test_sf <- st_as_sf(test, coords = c("lon", "lat"))
st_crs(test_sf) <- 4326


centroides_sf <- st_as_sf(centroides, coords = c("x", "y"))
centroides2_sf <- st_as_sf(centroides2, coords = c("x", "y"))
centroides3_sf <- st_as_sf(centroides3, coords = c("x", "y"))
centroides5_sf <- st_as_sf(centroides5, coords = c("x", "y"))


# 3.4 Calcular distancias 

dist_parque_train <- st_distance(x = train_sf, y = centroides_sf)
dist_fitness_train <- st_distance(x = train_sf, y = centroides2_sf)
dist_playground_train <- st_distance(x = train_sf, y = centroides3_sf)
dist_sports_centre_train <- st_distance(x = train_sf, y = centroides5_sf)


hospital_centre_train <- st_distance(x = train_sf, y = hospital_geometria)
clinic_centre_train <- st_distance(x = train_sf, y = clinic_geometria)
busstation_centre_train <- st_distance(x = train_sf, y = bus_station_geometria)
police_centre_train <- st_distance(x = train_sf, y = police_geometria)
pub_centre_train <- st_distance(x = train_sf, y = pub_geometria)
school_centre_train <- st_distance(x = train_sf, y = school_geometria)
cinema_centre_train <- st_distance(x = train_sf, y = cinema_geometria)
restaurante_centre_train <- st_distance(x = train_sf, y = restaurant_geometria)


dist_parque_test <- st_distance(x = test_sf, y = centroides_sf)
dist_fitness_test <- st_distance(x = test_sf, y = centroides2_sf)
dist_playground_test <- st_distance(x = test_sf, y = centroides3_sf)
dist_sports_centre_test <- st_distance(x = test_sf, y = centroides5_sf)


hospital_centre_test <- st_distance(x = test_sf, y = hospital_geometria)
clinic_centre_test <- st_distance(x = test_sf, y = clinic_geometria)
busstation_centre_test <- st_distance(x = test_sf, y = bus_station_geometria)
police_centre_test <- st_distance(x = test_sf, y = police_geometria)
pub_centre_test <- st_distance(x = test_sf, y = pub_geometria)
school_centre_test <- st_distance(x = test_sf, y = school_geometria)
cinema_centre_test <- st_distance(x = test_sf, y = cinema_geometria)
restaurante_centre_test <- st_distance(x = test_sf, y = restaurant_geometria)


# 3.4 Distancia minima 

#Train
dist_min_train_parque <- apply(dist_parque_train, 1, min)
train$dist_min_train_parque <- dist_min_train_parque
train$dist_min_train_parque <- dist_min_train_parque

dist_min_train_fitness <- apply(dist_fitness_train, 1, min)
train$dist_min_train_fitness <- dist_min_train_fitness
train$dist_min_train_fitnesse <- dist_min_train_fitness

dist_min_train_playground <- apply(dist_playground_train, 1, min)
train$dist_min_train_playground <- dist_min_train_playground
train$dist_min_train_playground <- dist_min_train_playground

#dist_min_train_sportc <- apply(dist_min_train_sportc, 1, min)
#train$dist_min_train_sportc <- dist_min_train_sportc
#train$dist_min_train_sportc <- dist_min_train_sportc


dist_hospital_centre_train <- apply(hospital_centre_train, 1, min)
train$dist_hospital_centre_train <- dist_hospital_centre_train
train$dist_hospital_centre_train <- dist_hospital_centre_train


dist_clinic_centre_train <- apply(clinic_centre_train, 1, min)
train$dist_clinic_centre_train <- dist_clinic_centre_train
train$dist_clinic_centre_train <- dist_clinic_centre_train


dist_busstation_centre_train <- apply(busstation_centre_train, 1, min)
train$dist_busstation_centre_train <- dist_busstation_centre_train
train$dist_busstation_centre_train <- dist_busstation_centre_train


dist_police_centre_train <- apply(police_centre_train, 1, min)
train$dist_police_centre_train <- dist_police_centre_train
train$dist_police_centre_train <- dist_police_centre_train


dist_pub_centre_train <- apply(pub_centre_train, 1, min)
train$dist_pub_centre_train <- dist_pub_centre_train
train$dist_pub_centre_train <- dist_pub_centre_train


dist_school_centre_train <- apply(school_centre_train, 1, min)
train$dist_school_centre_train <- dist_school_centre_train
train$dist_school_centre_train <- dist_school_centre_train


dist_cinema_centre_train <- apply(cinema_centre_train, 1, min)
train$dist_cinema_centre_train <- dist_cinema_centre_train
train$dist_cinema_centre_train <- dist_cinema_centre_train


dist_restaurante_centre_train <- apply(restaurante_centre_train, 1, min)
train$dist_restaurante_centre_train <- dist_restaurante_centre_train
train$dist_restaurante_centre_train <- dist_restaurante_centre_train


#Test
dist_min_test_parque <- apply(dist_parque_test, 1, min)
test$dist_min_test_parque <- dist_min_test_parque
test$dist_min_test_parque <- dist_min_test_parque

dist_min_test_fitness <- apply(dist_fitness_test, 1, min)
test$dist_min_test_fitness <- dist_min_test_fitness
test$dist_min_test_fitnesse <- dist_min_test_fitness

dist_min_test_playground <- apply(dist_playground_test, 1, min)
test$dist_min_test_playground <- dist_min_test_playground
test$dist_min_test_playground <- dist_min_test_playground

dist_min_test_sportc <- apply(dist_min_test_sportc, 1, min)
test$dist_min_test_sportc <- dist_min_test_sportc
test$dist_min_test_sportc <- dist_min_test_sportc


dist_hospital_centre_test <- apply(hospital_centre_test, 1, min)
test$dist_hospital_centre_test <- dist_hospital_centre_test
test$dist_hospital_centre_test <- dist_hospital_centre_test


dist_clinic_centre_test <- apply(clinic_centre_test, 1, min)
test$dist_clinic_centre_test <- dist_clinic_centre_test
test$dist_clinic_centre_test <- dist_clinic_centre_test


dist_busstation_centre_test <- apply(busstation_centre_test, 1, min)
test$dist_busstation_centre_test <- dist_busstation_centre_test
test$dist_busstation_centre_test <- dist_busstation_centre_test


dist_police_centre_test <- apply(police_centre_test, 1, min)
test$dist_police_centre_test <- dist_police_centre_test
test$dist_police_centre_test <- dist_police_centre_test


dist_pub_centre_test <- apply(pub_centre_test, 1, min)
test$dist_pub_centre_test <- dist_pub_centre_test
test$dist_pub_centre_test <- dist_pub_centre_test



dist_school_centre_test <- apply(school_centre_test, 1, min)
test$dist_school_centre_test <- dist_school_centre_test
test$dist_school_centre_test <- dist_school_centre_test


dist_cinema_centre_test <- apply(cinema_centre_test, 1, min)
test$dist_cinema_centre_test <- dist_cinema_centre_test
test$dist_cinema_centre_test <- dist_cinema_centre_test


dist_restaurante_centre_test <- apply(restaurante_centre_test, 1, min)
test$dist_restaurante_centre_test <- dist_restaurante_centre_test
test$dist_restaurante_centre_test <- dist_restaurante_centre_test


# 3.5 Area del parque más cercano 

posicion_train <- apply(dist_parque_train, 1, function(x) which(min(x) == x))
areas <- st_area(parques_geometria)
train$area_parque <- areas[posicion_train]
train$area_parque <- as.numeric(train$area_parque)


posicion_test <- apply(dist_parque_test, 1, function(x) which(min(x) == x))
areas <- st_area(parques_geometria)
test$area_parque <- areas[posicion_test]



# 3.6 Exportar datos

train <- train %>% 
  mutate(dist_min_train_fitness = round(dist_min_train_fitness),
         dist_min_train_parque = round(dist_min_train_parque),
         dist_min_train_playground = round(dist_min_train_playground),
         dist_hospital_centre_train = round(dist_hospital_centre_train),
         dist_clinic_centre_train = round(dist_clinic_centre_train),
         dist_busstation_centre_train = round(dist_busstation_centre_train),
         dist_police_centre_train = round(dist_police_centre_train),
         dist_pub_centre_train = round(dist_pub_centre_train),
         dist_school_centre_train = round(dist_school_centre_train),
         dist_cinema_centre_train = round(dist_cinema_centre_train),
         dist_restaurante_centre_train = round(dist_restaurante_centre_train),
         area_parque = round(area_parque))


test <- test %>% 
  mutate(dist_min_test_fitness = round(dist_min_test_fitness),
         dist_min_test_parque = round(dist_min_test_parque),
         dist_min_test_playground = round(dist_min_test_playground),
         dist_min_test_sportc = round(dist_min_test_sportc),
         dist_hospital_centre_test = round(dist_hospital_centre_test),
         dist_clinic_centre_test = round(dist_clinic_centre_test),
         dist_busstation_centre_test = round(dist_busstation_centre_test),
         dist_police_centre_test = round(dist_police_centre_test),
         dist_school_centre_test = round(dist_school_centre_test),
         dist_pub_centre_test = round(dist_pub_centre_test),
         dist_cinema_centre_test = round(dist_cinema_centre_test),
         dist_restaurante_centre_test = round(dist_restaurante_centre_test))


options(scipen = 999)
write.csv(train, file = "data_ignore/train_final.csv")
write.csv(test, file = "data_ignore/test_final.csv")




