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

julio.jn$timestamp<- lubridate::as_datetime(julio.jn$timestamp, tz="CST6CDT")

#3er paso:
julio.jn$timestamp.day <- lubridate::day(julio.jn$timestamp)
julio.jn$timestamp.hour <- lubridate::hour(julio.jn$timestamp)
julio.jn$geometry <- NULL

###

julio.jn %>% select(timestamp, vehicle, speed, ruta, X, Y, timestamp.day, timestamp.hour) %>% filter("2020-07-13 00:00"==as.POSIXct(julio.jn$timestamp))

#########33
############3
###LO ANTERIOR TIENE QUE VER CON LA UNIÓN ESPACIAL CON EL GTFS ESTÁTICO############
#####DESDE AQUÍ#######

#Check

julio.sp.jn.l1 <- read.csv("C:/Users/85412/Desktop/julio.sp.jn.l1.csv") 
names(julio.sp.jn.l1) <- c("timestamp", "vehicle", "speed", "x", "y", "ruta")
julio.sp.jn.l1$x <- substr(julio.sp.jn.l1$x, 3, 10) 
julio.sp.jn.l1$y <- substr(julio.sp.jn.l1$y, 1, 8) 
julio.sp.jn.l1$timestamp <- as.POSIXct(julio.sp.jn.l1$timestamp)
julio.sp.jn.l1$x <- as.numeric(julio.sp.jn.l1$x)
julio.sp.jn.l1$y <- as.numeric(julio.sp.jn.l1$y)
jul.l1 <- julio.sp.jn.l1

#Check

for (i in 15:31) {
  assign(paste0("jul", sep=".", i), data.frame(jul.l1 %>%  select(timestamp, vehicle, speed, x, y, ruta) %>% filter (lubridate::day(jul.l1$timestamp)==i)))  
}

#Check. Se usan estas funciones para obtener los camiones que particpan en las rutas diarias

jul_fun <- function(i) {
  jul.15 %>% select(timestamp, vehicle, speed, x, y, ruta) %>% filter(vehicle==i)
}

for (i in jul.15$vehicle) {
  assign (paste0("vehi", sep=".", i), data.frame(jul_fun(i)))
}

#####





##Función para seleccionr por día y hora

jul.fun <- function(i, j){
  jul.l1 %>%  select(timestamp, vehicle, speed, x, y, ruta) %>% filter (lubridate::day(jul.l1$timestamp)==i & lubridate::hour(jul.l1$timestamp)==j) 
}
#ejemplos  
tail(jul.fun(15,c(6:23)))
jul.19.6 <- jul.fun(19,6)

assign("jul.19.6",data.frame(jul.fun(19,6)))




####Hasta aquí vamos bien, pero recordar que estoy usando solamente la mitad de la distribución
#Ejemplos

jul.15 <- jul.fun(15,6)
jul.15.6 <- jul.fun(15,6)

#Check







#Por hora
#¿Es posible aplicar la siguiente función para archivos con las trayectorias por hora?

jul_fun <- function(k){
  jul.15.6 %>% select(timestamp, vehicle, speed, x, y, ruta) %>% filter(vehicle==k)
} 

for (k in jul.15.6$vehicle) {
  assign (paste0("vehi", sep=",", i), data.frame(jul_fun(k)))
}

#Agregar las trayectorias a una lista. Para la verificación deben coincidir el número de elementos de la lista con el número de valores únicos de la variable vehicle. 

lista <- mget(ls(pattern = "vehi.")) # con este comando agrega los dataframes con un patrón a la lista
unique(jul.15.6$vehicle)

#####Lo que sigue debe ser con lapply y funciones parecidas
