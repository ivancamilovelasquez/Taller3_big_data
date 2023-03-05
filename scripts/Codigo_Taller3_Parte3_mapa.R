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



# 2.1 Train

grafica_train_completa <-  leaflet() %>% addTiles() %>%
  addPolygons(data = parques_geometria, col ="yellow",
              opacity = 0.8, popup = parques_geometria$name) %>%
  addCircles(lng = train$lon, 
             lat = train$lat,
             fillOpacity = 1,
             col = color,
             radius = r_scaled_test,
             popup = html_popup_test,
             opacity = 1)%>%
  addPolygons(data=hospital_geometria , col="#f2542d")%>% #hospitales
  addPolygons(data=clinic_geometria , col="#f2542d")%>% #hospitales
  addPolygons(data=bus_station_geometria , col="#0e9594")%>%#buses
  addPolygons(data=police_geometria , col="#f5dfbb")%>%#policia
  addPolygons(data=school_geometria , col="#0d3b66")%>%#colegio
  addPolygons(data=restaurant_geometria , col="#55286f")#restaruante
htmlwidgets::saveWidget(grafica_train_completa, "views//grafica_train_completa.html") 


# 2.2 Test
grafica_test_completa <-  leaflet() %>% addTiles() %>%
  addPolygons(data = parques_geometria, col ="yellow",
              opacity = 0.8, popup = parques_geometria$name) %>%
  addCircles(lng = test$lon, 
             lat = test$lat,
             fillOpacity = 1,
             col = color,
             radius = r_scaled_test,
             popup = html_popup_test,
             opacity = 1)%>%
  addPolygons(data=hospital_geometria , col="#f2542d")%>% #hospitales
  addPolygons(data=clinic_geometria , col="#f2542d")%>% #hospitales
  addPolygons(data=bus_station_geometria , col="#0e9594")%>%#buses
  addPolygons(data=police_geometria , col="#f5dfbb")%>%#policia
  addPolygons(data=school_geometria , col="#0d3b66")%>%#colegio
  addPolygons(data=restaurant_geometria , col="#55286f")#restaruante
htmlwidgets::saveWidget(grafica_test_completa, "views//grafica_test_completa.html") 


