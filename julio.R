#install.packages("sf")
#install.packages("TrackReconstruction")
#Cargar el gtfs estático para unirlo con la ruta del Metrobús a la que pertenece
gtfs_estatico<-sf::st_read("/Users/85412/Desktop/gtfs_estatico/gtfs_estatico.shp")
gtfs_estatico <- gtfs_estatico %>%  select(agencia, ruta, geometry) %>% filter(agencia == "METROBUS")
gtfs_estatico$agencia <- NULL
#######################
julio <- readr::read_csv("C:/Users/85412/Desktop/gtfs_rt/julio.csv")
julio$CST6CDT <- lubridate::as_datetime(julio$TIMESTAMP, tz="CST6CDT")
colnames(julio)[9] <- "LATITUDE"
julio$LATITUDE <- stringr::str_sub(julio$LATITUDE, 10,-1)
julio$LATITUDE <- as.numeric(julio$LATITUDE)
julio$VEHICLE <- stringr::str_sub(julio$VEHICLE, 4,-1)
julio$as_date<- lubridate::as_date(julio$CST6CDT)
julio$as_hour <- lubridate::hour(julio$CST6CDT)

#Identificar cantidad de id_vehicles único
julio$id_vehicle <- paste(julio$as_date, julio$VEHICLE, sep="-")
#Extraer datos de las 6 a las 23 horas
julio <- julio %>%  select(Id, TIMESTAMP, VEHICLE, ROUTEID, STARTTIME, STARTDATE, SCHEDULE_RELATIONSHIP, LABEL, LATITUDE, LONGITUDE, BEARING, ODOMETER, SPEED, CURRENTSTATUS, LECTURA, CST6CDT, as_date, as_hour, id_vehicle) %>% filter(as_hour == "6" | as_hour=="7" | as_hour=="8" | as_hour=="9"| as_hour=="10" | as_hour=="11"| as_hour=="12"| as_hour=="13"| as_hour=="14"| as_hour=="15"| as_hour=="16"| as_hour=="17"| as_hour=="18"| as_hour=="19" | as_hour=="20"| as_hour=="21"| as_hour=="22"| as_hour=="23")
#Extraer datos del 13 de julio
julio13 <- julio %>%  select(id_vehicle, Id, TIMESTAMP, VEHICLE, ROUTEID, STARTTIME, STARTDATE, SCHEDULE_RELATIONSHIP, LABEL, LATITUDE, LONGITUDE, BEARING, ODOMETER, SPEED, CURRENTSTATUS, LECTURA, CST6CDT, as_date, as_hour) %>% filter(as_date == "2020-07-13")

#/////Hasta aquí vamos bien/////
#¿Cuántas observaciones distintas hay?
nrow(distinct(julio13, id_vehicle))

-
#¿Cuántas observaciones repetidas del id_vehicle==2020-07-13-1?
nrow(julio13 %>%  select(id_vehicle, Id, TIMESTAMP, VEHICLE, ROUTEID, LABEL, LATITUDE, LONGITUDE, BEARING, ODOMETER, SPEED, CST6CDT, as_date, as_hour) %>% filter(id_vehicle == "2020-07-13-1"))
julio_2020_07_13_1_6 <- julio13 %>%  select(id_vehicle, Id, TIMESTAMP, VEHICLE, ROUTEID, LABEL, LATITUDE, LONGITUDE, BEARING, ODOMETER, SPEED, CST6CDT, as_date, as_hour) %>% filter(id_vehicle == "2020-07-13-1" & as_hour=="6")
#//////Hasta aquí todo fine////////

#¿cuántos casos repetidos hay del caso 2020-07-13-1-6??
#nrow(julio_2020_07_13_1_6 %>%  select(id_vehicle, Id, TIMESTAMP, VEHICLE, ROUTEID, LABEL, LATITUDE, LONGITUDE, BEARING, ODOMETER, SPEED, CST6CDT, as_date) %>% filter(id_vehicle == "2020-07-13-1" & as_hour=="6"))
df_start_1 <- julio_13_1[!duplicated(julio_13_1$id_vehicle),]
df_end_1 <- julio_13_1[rev(!duplicated(rev(julio_13_1$id_vehicle))),]
st_ed_1<- dplyr::left_join(df_start_1, df_end_1, by="id_vehicle")


#////Arriba todo fine//////////
#En la siguiente sección se hace una verificación de los resultados con geosphere, trajr y TrackReconstruction
#Ejemplo con el  caso "2020-07-13-1"
###Resultados con trajr basado en la vignette cran.rstudio.com/web/packages/trajr/vignettes/trajr-vignette.html
coords <- data.frame(x = julio_2020_07_13_1_6$LONGITUDE, 
                     y = julio_2020_07_13_1_6$LATITUDE, 
                     times = julio_2020_07_13_1_6$CST6CDT)
trj <- trajr::TrajFromCoords(coords)
plot(trj)
trajr::TrajStepLengths(trj)

coords <- data.frame(x= c(-99.1460, -99.1439, -99.1437, -99.1444),
                     y= c(19.4737, 19.4680, 19.4675, 19.4631),
                     times= c(0, 1, 2, 4))
trj <- trajr::TrajFromCoords(coords)
plot(trj)
trajr::TrajStepLengths(trj)

#Estimación con la librería TrackReconstruction. El resultado será .67 km?

TrackReconstruction::CalcDistance(19.4675, -99.1437, 19.4631, -99.1444)
TrackReconstruction::CalcDistance(st_ed_1$LATITUDE.x, st_ed_1$LONGITUDE.x, st_ed_1$LATITUDE.y, st_ed_1$LONGITUDE.y) #en km
TrackReconstruction::CalcBearing(st_ed_1$LATITUDE.x, st_ed_1$LONGITUDE.x, st_ed_1$LATITUDE.y, st_ed_1$LONGITUDE.y) #en km


julio_6_1 <- julio_6 %>%  select(id_vehicle, Id, TIMESTAMP, VEHICLE, ROUTEID, LABEL, LATITUDE, LONGITUDE, BEARING, ODOMETER, SPEED, CST6CDT, as_date) %>% filter(id_vehicle == "2020-07-13-1")


#El código de abajo sirve para encontrar el primer caso duplicado y el último
#######
df_start_1 <- julio_6_1[!duplicated(julio_6_1$id_vehicle),]
df_end_1 <- julio_6_1[rev(!duplicated(rev(julio_6_1$id_vehicle))),]
st_ed_1<- dplyr::left_join(df_start_1, df_end_1, by="id_vehicle")
#######

#Estimación con geosphere
P1 <- c(-99.1460, 19.4737)
P2 <- c(-99.1439, 19.4680)
P3 <- c(-99.1437, 19.4675)
P4 <- c(-99.1444, 19.4631)
P5
P6
P7
geosphere::distCosine(P1, P2)#En metros
geosphere::distCosine(P2, P3)
geosphere::bearing(P3, P4) #No sé en qué unidades está el resultado.

#############

Nombres <- c("Juan", "Pedro", "Mayelo", "Enrique", "José", "Pedro", "Mayelo", "José")
Valores <- c("2","9","20","5","2","4","3", "6")
calificaciones <- as.data.frame(cbind(Nombres, Valores))
calificaciones %>% dplyr::select(Nombres, Valores)


df <- as.data.frame(cbind(Nombres, Valores)) 


df_primero <- df[!duplicated(df$Nombres),]#Get starting week
df_final <- df[rev(!duplicated(rev(df$Nombres))),]#Get ending week

#
julio %>% arrange(id_vehicle)

df %>% select(Nombres)

