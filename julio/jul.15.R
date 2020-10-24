
###EN ESTA ESTRATEGIA METODOLÓGICA SE OPTA POR HACER EL JOIN DEL MES PREVIAMENTE###

#1er paso: se carga la base de datos mensual y se le da formato a las variables
#para poder trabajar con ellas.
jul.l1 <- read.csv("C:/Users/85412/Desktop/julio.sp.jn.l1.csv") 
names(jul.l1) <- c("timestamp", "vehicle", "speed", "x", "y", "ruta")

jul.l1$x <- substr(jul.l1$x, 3, 10) 
jul.l1$y <- substr(jul.l1$y, 1, 8) 
jul.l1$timestamp <- as.POSIXct(jul.l1$timestamp)
jul.l1$x <- as.numeric(jul.l1$x)
jul.l1$y <- as.numeric(jul.l1$y)

#2do paso: se extraen los datos correspondientes al día con el que se trabajará.

#Datos de las 6 a las 23 horas
jul.l1 %>%  select(timestamp, vehicle, speed, x, y, ruta) %>% filter(lubridate::day(jul.l1$timestamp)==15 & lubridate::hour(jul.l1$timestamp)==6)
#Datos del 19 de julio
jul_19 <- julio %>%  select(id_vehicle, Id, TIMESTAMP, VEHICLE, ROUTEID, STARTTIME, STARTDATE, SCHEDULE_RELATIONSHIP, LABEL, LATITUDE, LONGITUDE, BEARING, ODOMETER, SPEED, CURRENTSTATUS, LECTURA, CST6CDT, as_date, as_hour) %>% filter(as_date == "2020-07-19")

# 2.b. 
##Función para seleccionr por día y hora

jul.fun.01 <- function(i, j){
  
  jul.l1 %>%  select(timestamp, vehicle, speed, x, y, ruta) %>% filter (lubridate::day(jul.l1$timestamp)==i & lubridate::hour(jul.l1$timestamp)==j) 
  
}

jul.fun.01 (15,7)

