#1er paso: se carga la base de datos mensual y se le da formato a las variables
#para poder trabajar con ellas.

julio <- readr::read_csv("C:/Users/85412/Desktop/gtfs_rt/julio.csv")
julio$CST6CDT <- lubridate::as_datetime(julio$TIMESTAMP, tz="CST6CDT")
colnames(julio)[9] <- "LATITUDE"
julio$LATITUDE <- stringr::str_sub(julio$LATITUDE, 10,-1)
julio$LATITUDE <- as.numeric(julio$LATITUDE)
julio$VEHICLE <- stringr::str_sub(julio$VEHICLE, 4,-1)
julio$as_date<- lubridate::as_date(julio$CST6CDT)
julio$as_hour <- lubridate::hour(julio$CST6CDT)
julio$id_vehicle <- paste(julio$as_date, julio$VEHICLE, sep="-")

#2do paso: se extraen los datos correspondientes al día con el que se trabajará.

#Datos de las 6 a las 23 horas exceptuando las horas pico
julio <- julio %>%  select(id_vehicle, Id, TIMESTAMP, VEHICLE, ROUTEID, STARTTIME, STARTDATE, SCHEDULE_RELATIONSHIP, LABEL, LATITUDE, LONGITUDE, BEARING, ODOMETER, SPEED, CURRENTSTATUS, LECTURA, CST6CDT, as_date, as_hour) %>% filter(as_hour == "6" | as_hour=="9"| as_hour=="10" | as_hour=="11"| as_hour=="12"| as_hour=="13"| as_hour=="14"| as_hour=="15"| as_hour=="16"| as_hour=="17"|as_hour=="19" | as_hour=="20"| as_hour=="21"| as_hour=="22"| as_hour=="23")
#Datos del 14 de julio
jul_14 <- julio %>%  select(id_vehicle, Id, TIMESTAMP, VEHICLE, ROUTEID, STARTTIME, STARTDATE, SCHEDULE_RELATIONSHIP, LABEL, LATITUDE, LONGITUDE, BEARING, ODOMETER, SPEED, CURRENTSTATUS, LECTURA, CST6CDT, as_date, as_hour) %>% filter(as_date == "2020-07-13")

#3er paso: unión espacial.

#Cargar el gtfs estático.
gtfs_estatico<-sf::st_read("/Users/85412/Desktop/gtfs_estatico/gtfs_estatico.shp")
gtfs_estatico <- gtfs_estatico %>%  select(agencia, ruta, geometry) %>% filter(agencia == "METROBUS")
gtfs_estatico$agencia <- NULL
jul_14p<- sf::st_as_sf(jul_14, coords = c("LONGITUDE", "LATITUDE"), crs = 4326, agr = "constant")
jul_14_jn <- sf::st_join(jul_14p, gtfs_estatico, join = nngeo::st_nn, maxdist = 100)# Debe correrse library(nngeo) para que trabaje el st_nn
jul_14_jn <-jul_14_jn %>% select(., id_vehicle, geometry, CST6CDT, ruta) %>% filter(ruta=="00L1" |ruta=="00L2")
jul_14_coords<- sf::st_coordinates(jul_14_jn)
jul_14_jn <- cbind(jul_14_jn, jul_14_coords)

#4to paso: función que mida...

jul_fun <- function(i,j) {
  j<-jul_14_jn %>% select(id_vehicle, Y, X, CST6CDT) %>% filter(jul_14_jn$id_vehicle==i) %>%
    mutate(., y_lead=lead(Y, n=1)) %>% mutate(., x_lead=lead(X, n=1)) 
  mutate(j, dist = TrackReconstruction::CalcDistance(j$Y, j$X, j$y_lead, j$x_lead))
}

#5to paso... 

for (i in jul_14_jn$id_vehicle) {
  assign (i, data.frame(jul_fun(i)))
}

#6to paso

unicos <- as.data.frame(unique(jul_14_jn$id_vehicle))
unicos<- unicos[with(unicos, order(unicos$`unique(jul_14_jn$id_vehicle)`)), ]

#write.csv(unicos, "C:/Users/85412/Desktop/unicos.csv")


#Séptimo paso. 

lista <- mget(ls(pattern = "2020-07-13-")) # con este comando agrega los dataframes con un patrón a la lista

suma <- as.data.frame(sapply(lista, function(x) sum(x$dist, na.rm = TRUE)))
rango <- sapply(lista, function(x) range(x$CST6CDT, na.rm = TRUE)) 
rango_t <- data.frame(t(rango))

jul_14 <- cbind(unicos, suma)
jul_14 <- cbind(jul_14, rango_t)

jul_14$hrs<- lubridate::as_datetime(jul_14$X2) - lubridate::as_datetime(jul_14$X1)
jul_14$hrs <- jul_14$hrs/60/60

#Octavo paso.

jul_14$hrs <- as.numeric(jul_14$hrs)
jul_14$velocidad <- jul_14$`sapply(lista, function(x) sum(x$dist, na.rm = TRUE))`/jul_14$hrs
names(jul_14) <- c("id_vehicle", "distancia", "t1", "t2", "tiempo", "velocidad")
write.csv(jul_14, "/Users/85412/Desktop/gtfs_rt/julio/jul_14_valle.csv")
