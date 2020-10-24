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

#####DESDE AQUÍ#######
julio.sp.jn.l1 <- read.csv("C:/Users/85412/Desktop/julio.sp.jn.l1.csv") 
names(julio.sp.jn.l1) <- c("timestamp", "vehicle", "speed", "x", "y", "ruta")
julio.sp.jn.l1$x <- substr(julio.sp.jn.l1$x, 3, 10) 
julio.sp.jn.l1$y <- substr(julio.sp.jn.l1$y, 1, 8) 

julio.sp.jn.l1$timestamp <- as.POSIXct(julio.sp.jn.l1$timestamp)
julio.sp.jn.l1$x <- as.numeric(julio.sp.jn.l1$x)
julio.sp.jn.l1$y <- as.numeric(julio.sp.jn.l1$y)

jul.l1 <- julio.sp.jn.l1

####
#For para generar dataframes diarios

for (i in 15:31) {
  assign(paste0("jul", sep=".", i), data.frame(jul.l1 %>%  select(timestamp, vehicle, speed, x, y, ruta) %>% filter (lubridate::day(jul.l1$timestamp)==i)))  
}

###Seleccionar por día y hora

jul.l1 %>%  select(timestamp, vehicle, speed, x, y, ruta) %>% filter (lubridate::day(jul.l1$timestamp)==15 & lubridate::hour(jul.l1$timestamp)==6) 


##Función para seleccionr por día y hora

jul.fun <- function(i, j){
  
  jul.l1 %>%  select(timestamp, vehicle, speed, x, y, ruta) %>% filter (lubridate::day(jul.l1$timestamp)==i & lubridate::hour(jul.l1$timestamp)==j) 
  
}
  
jul.fun(15,7)

jul.15.6 <- jul.fun(15,6)
jul.15.7 <- jul.fun(15,7)
jul.15.8 <- jul.fun(15,8)
jul.15.9 <- jul.fun(15,9)
jul.15.10 <- jul.fun(15,10)
jul.15.11 <- jul.fun(15,11)
jul.15.12 <- jul.fun(15,12)
jul.15.13 <- jul.fun(15,13)
jul.15.14 <- jul.fun(15,14)
jul.15.15 <- jul.fun(15,15)
jul.15.16 <- jul.fun(15,16)
jul.15.17 <- jul.fun(15,17)
jul.15.18 <- jul.fun(15,18)
jul.15.19 <- jul.fun(15,19)
jul.15.20 <- jul.fun(15,20)
jul.15.21 <- jul.fun(15,21)
jul.15.22 <- jul.fun(15,22)
jul.15.23 <- jul.fun(15,23)

### Aplicar para cada hora


for (i in unique(jul.15.6$vehicle)) {
  j <- jul.15.6 %>% select(timestamp, vehicle, speed, x, y, ruta) %>% filter(jul.15.6$vehicle==i) %>% mutate(., y.lead=lead(y, n=1)) %>% mutate(., x.lead=lead(x, n=1)) 
  mutate(j, dist = TrackReconstruction::CalcDistance(j$y, j$x, j$y.lead, j$x.lead))
}
  



for (i in 6:23){
  
  jul.fun(15, i)
  print(i)
}


######


lst.jul <- mget(ls(pattern = "jul.")) # con este comando agrega los dataframes con un patrón a la lista
lst.jul[[19]] <- NULL
lst.jul[[18]] <- NULL
###
jul.15$hora <- lubridate::hour(jul.15$timestamp)  
jul.15$id.vehicle <- paste(jul.15$vehicle, jul.15$hora)

##cORRER DE 6 A 23 PARA CADA DÍA####
for (i in 6:23) {
  assign(paste0("jul.", i), data.frame(jul.15 %>%  select(timestamp, vehicle, speed, x, y, ruta) %>% filter (lubridate::hour(jul.15$timestamp)==i)))  
}


