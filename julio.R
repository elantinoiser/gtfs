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


#Extraer datos de las 6 a las 23
julio <- julio %>%  select(Id, TIMESTAMP, VEHICLE, ROUTEID, STARTTIME, STARTDATE, SCHEDULE_RELATIONSHIP, LABEL, LATITUDE, LONGITUDE, BEARING, ODOMETER, SPEED, CURRENTSTATUS, LECTURA, CST6CDT, as_date, as_hour, id_vehicle) %>% filter(as_hour == "6" | as_hour=="7" | as_hour=="8" | as_hour=="9"| as_hour=="10" | as_hour=="11"| as_hour=="12"| as_hour=="13"| as_hour=="14"| as_hour=="15"| as_hour=="16"| as_hour=="17"| as_hour=="18"| as_hour=="19" | as_hour=="20"| as_hour=="21"| as_hour=="22"| as_hour=="23")

#Extraer datos del 13 de julio
julio13 <- julio %>%  select(id_vehicle, Id, TIMESTAMP, VEHICLE, ROUTEID, STARTTIME, STARTDATE, SCHEDULE_RELATIONSHIP, LABEL, LATITUDE, LONGITUDE, BEARING, ODOMETER, SPEED, CURRENTSTATUS, LECTURA, CST6CDT, as_date, as_hour) %>% filter(as_date == "2020-07-13")

#/////Hasta aquí vamos bien/////
#¿Cuántas observaciones distintas hay?
nrow(distinct(julio13, id_vehicle))

#¿Cuántas observaciones repetidas del id_vehicle==2020-07-13-1
nrow(julio13 %>%  select(id_vehicle, Id, TIMESTAMP, VEHICLE, ROUTEID, LABEL, LATITUDE, LONGITUDE, BEARING, ODOMETER, SPEED, CST6CDT, as_date) %>% filter(id_vehicle == "2020-07-13-1"))

julio_13_1 <- julio13 %>%  select(id_vehicle, Id, TIMESTAMP, VEHICLE, ROUTEID, LABEL, LATITUDE, LONGITUDE, BEARING, ODOMETER, SPEED, CST6CDT, as_date) %>% filter(id_vehicle == "2020-07-13-1")

df_start_1 <- julio_13_1[!duplicated(julio_13_1$id_vehicle),]
df_end_1 <- julio_13_1[rev(!duplicated(rev(julio_13_1$id_vehicle))),]
st_ed_1<- dplyr::left_join(df_start_1, df_end_1, by="id_vehicle")

#////Arriba todo fine//////////

dist_6 <- as.data.frame(TrackReconstruction::CalcDistance(st_ed$LATITUDE.x, st_ed$LONGITUDE.x, st_ed$LATITUDE.y, st_ed$LONGITUDE.y))

#Ejemplo con el  caso "2020-07-13-1"
#¿cuántos casos repetidos hay del caso 2020-07-13-1??
nrow(julio_6 %>%  select(id_vehicle, Id, TIMESTAMP, VEHICLE, ROUTEID, LABEL, LATITUDE, LONGITUDE, BEARING, ODOMETER, SPEED, CST6CDT, as_date) %>% filter(id_vehicle == "2020-07-13-1"))

julio_6_1 <- julio_6 %>%  select(id_vehicle, Id, TIMESTAMP, VEHICLE, ROUTEID, LABEL, LATITUDE, LONGITUDE, BEARING, ODOMETER, SPEED, CST6CDT, as_date) %>% filter(id_vehicle == "2020-07-13-1")

df_start_1 <- julio_6_1[!duplicated(julio_6_1$id_vehicle),]
df_end_1 <- julio_6_1[rev(!duplicated(rev(julio_6_1$id_vehicle))),]
st_ed_1<- dplyr::left_join(df_start_1, df_end_1, by="id_vehicle")
TrackReconstruction::CalcDistance(st_ed_1$LATITUDE.x, st_ed_1$LONGITUDE.x, st_ed_1$LATITUDE.y, st_ed_1$LONGITUDE.y) #en km
TrackReconstruction::CalcBearing(st_ed_1$LATITUDE.x, st_ed_1$LONGITUDE.x, st_ed_1$LATITUDE.y, st_ed_1$LONGITUDE.y) #en km
SF <- c(st_ed_1$LONGITUDE.x, st_ed_1$LATITUDE.x)
AM <- c(st_ed_1$LONGITUDE.y, st_ed_1$LATITUDE.y)
geosphere::distCosine(SF, AM) #En metros
geosphere::bearing(SF, AM) #No sé en qué unidades está el resultado.
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

