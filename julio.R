#install.packages("sf")
#install.packages("TrackReconstruction")
julio <- readr::read_csv("C:/Users/85412/Desktop/gtfs_rt/julio.csv")
julio$CST6CDT <- lubridate::as_datetime(julio$TIMESTAMP, tz="CST6CDT")
colnames(julio)[9] <- "LATITUDE"
julio$LATITUDE <- stringr::str_sub(julio$LATITUDE, 10,-1)
julio$LATITUDE <- as.numeric(julio$LATITUDE)
julio$VEHICLE <- stringr::str_sub(julio$VEHICLE, 4,-1)
julio$as_date<- lubridate::as_date(julio$CST6CDT)
julio$as_hour <- lubridate::hour(julio$CST6CDT)

#Extraer sólo los datos necesarios
julio <- julio %>%  select(Id, TIMESTAMP, VEHICLE, ROUTEID, STARTTIME, STARTDATE, SCHEDULE_RELATIONSHIP, LABEL, LATITUDE, LONGITUDE, BEARING, ODOMETER, SPEED, CURRENTSTATUS, LECTURA, CST6CDT, as_date, as_hour) %>% filter(as_hour == "6" | as_hour=="7" | as_hour=="8" | as_hour=="9"| as_hour=="10" | as_hour=="11"| as_hour=="12"| as_hour=="13"| as_hour=="14"| as_hour=="15"| as_hour=="16"| as_hour=="17"| as_hour=="18"| as_hour=="19" | as_hour=="20"| as_hour=="21"| as_hour=="22"| as_hour=="23")

#Identificar cantidad de id_vehicles único
julio$id_vehicle <- paste(julio$as_date, julio$VEHICLE, sep="-")


#Cargar el gtfs estático para unirlo con la ruta del Metrobús a la que pertenece
gtfs_estatico<-sf::st_read("/Users/85412/Desktop/gtfs_estatico/gtfs_estatico.shp")
gtfs_estatico <- gtfs_estatico %>%  select(agencia, ruta, geometry) %>% filter(agencia == "METROBUS")
gtfs_estatico$agencia <- NULL


#Trayectoria de las unidades

#Agrupar

df_start <- julio[!duplicated(julio$id_vehicle),]#Get starting week
df_end <- julio[rev(!duplicated(rev(julio$id_vehicle))),]#Get ending week
start_end<- dplyr::left_join(df_start, df_end, by="id_vehicle")
track <- as.data.frame(TrackReconstruction::CalcDistance(start_end$LATITUDE.x, start_end$LONGITUDE.x, start_end$LATITUDE.y, start_end$LONGITUDE.y))





