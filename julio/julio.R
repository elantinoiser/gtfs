#1er paso: se carga la base de datos mensual y se le da formato a las variables
#para poder trabajar con ellas.

julio <- readr::read_csv("C:/Users/85412/Desktop/gtfs_rt/julio.csv")
#julio <- julio[,c(2,3,9,10,13)] 
julio.speed.5.4 <- julio %>% select(TIMESTAMP, VEHICLE, POSITION, LONGITUDE, SPEED) %>% filter(SPEED>=5.4)

##
names(julio.speed.5.4) <- c("timestamp", "vehicle", "latitude", "longitude", "speed")
#
julio.speed.5.4$latitude <- stringr::str_sub(julio.speed.5.4$latitude, 10,-1)
julio.speed.5.4$vehicle <- stringr::str_sub(julio.speed.5.4$vehicle, 4,-1)
julio.speed.5.4$timestamp <- lubridate::as_datetime(julio.speed.5.4$timestamp, tz="CST6CDT")
#
#Cargar el gtfs estático.
gtfs.estatico<-sf::st_read("/Users/85412/Desktop/gtfs_estatico/gtfs_estatico.shp")
gtfs.estatico <- gtfs.estatico %>%  select(agencia, ruta, geometry) %>% filter(agencia == "METROBUS")
gtfs.estatico$agencia <- NULL
julio.speed.5.4.p<- sf::st_as_sf(julio.speed.5.4, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")
julio.speed.5.4.jn <- sf::st_join(julio.speed.5.4.p, gtfs.estatico, join = nngeo::st_nn, maxdist = 50)# Debe correrse library(nngeo) para que trabaje el st_nn
julio.sp.jn <-julio.speed.5.4.jn %>% select(., timestamp, vehicle, speed, geometry, ruta) %>% filter(ruta=="00L1" |ruta=="00L2")
julio.sp.jn.l1 <-julio.speed.5.4.jn %>% select(., timestamp, vehicle, speed, geometry, ruta) %>% filter(ruta=="00L1")
julio.sp.jn.l2 <-julio.speed.5.4.jn %>% select(., timestamp, vehicle, speed, geometry, ruta) %>% filter(ruta=="00L2")

julio.sp.jn.l1.coords<- sf::st_coordinates(julio.sp.jn.l1)
julio.jn <- cbind(julio.sp.jn.l1, julio.sp.jn.l1.coords)


#2do paso: manejar correctamente la conversión de fechas para, a su vez, seleccionar adecuadamente las observaciones.

lubridate::as_datetime(julio.jn$timestamp, tz="CST6CDT")

#Datos de las 6 a las 23 horas
julio.jn.14.6 <- julio.jn %>%  select(timestamp, vehicle, speed, X, Y) %>% filter(as_hour == "7")

#Datos del 20 de julio
jul_20 <- julio %>%  select(id_vehicle, Id, TIMESTAMP, VEHICLE, ROUTEID, STARTTIME, STARTDATE, SCHEDULE_RELATIONSHIP, LABEL, LATITUDE, LONGITUDE, BEARING, ODOMETER, SPEED, CURRENTSTATUS, LECTURA, CST6CDT, as_date, as_hour) %>% filter(as_date == "2020-07-20")


#3er paso: unión espacial.


#4to paso: función que mida...
