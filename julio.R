#install.packages("sf")
#install.packages("TrackReconstruction")
#Cargar el gtfs estático para unirlo con la ruta del Metrobús a la que pertenece

#######################
#Correr desde aquí    #
#######################

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
julio <- julio %>%  select(id_vehicle, Id, TIMESTAMP, VEHICLE, ROUTEID, STARTTIME, STARTDATE, SCHEDULE_RELATIONSHIP, LABEL, LATITUDE, LONGITUDE, BEARING, ODOMETER, SPEED, CURRENTSTATUS, LECTURA, CST6CDT, as_date, as_hour) %>% filter(as_hour == "6" | as_hour=="7" | as_hour=="8" | as_hour=="9"| as_hour=="10" | as_hour=="11"| as_hour=="12"| as_hour=="13"| as_hour=="14"| as_hour=="15"| as_hour=="16"| as_hour=="17"| as_hour=="18"| as_hour=="19" | as_hour=="20"| as_hour=="21"| as_hour=="22"| as_hour=="23")
#Extraer datos del 13 de julio
julio13 <- julio %>%  select(id_vehicle, Id, TIMESTAMP, VEHICLE, ROUTEID, STARTTIME, STARTDATE, SCHEDULE_RELATIONSHIP, LABEL, LATITUDE, LONGITUDE, BEARING, ODOMETER, SPEED, CURRENTSTATUS, LECTURA, CST6CDT, as_date, as_hour) %>% filter(as_date == "2020-07-13")
unicos_13 <- unique(julio13$id_vehicle)
###############################
#Correr hasta aquí, todo bien #
###############################
gtfs_estatico<-sf::st_read("/Users/85412/Desktop/gtfs_estatico/gtfs_estatico.shp")
gtfs_estatico <- gtfs_estatico %>%  select(agencia, ruta, geometry) %>% filter(agencia == "METROBUS")
gtfs_estatico$agencia <- NULL
plot(gtfs_estatico)
julio13p<- sf::st_as_sf(julio13, coords = c("LONGITUDE", "LATITUDE"), crs = 4326, agr = "constant")
julio13join <- sf::st_join(julio13p, gtfs_estatico, join = st_nn, maxdist = 100)# Debe correrse library(nngeo) para que trabaje el st_nn
julio13jn1y2<-julio13join %>% select(., id_vehicle, geometry, CST6CDT, ruta) %>% filter(ruta=="00L1" |ruta=="00L2")

#¿Cuántas observaciones distintas hay?
nrow(distinct(julio13, id_vehicle))
#¿Cuántas observaciones repetidas del id_vehicle==2020-07-13-1?
nrow(julio13 %>%  select(id_vehicle, Id, TIMESTAMP, VEHICLE, ROUTEID, LABEL, LATITUDE, LONGITUDE, BEARING, ODOMETER, SPEED, CST6CDT, as_date, as_hour) %>% filter(id_vehicle == "2020-07-13-1"))
julio_2020_07_13_1_6 <- julio13 %>%  select(id_vehicle, Id, TIMESTAMP, VEHICLE, ROUTEID, LABEL, LATITUDE, LONGITUDE, BEARING, ODOMETER, SPEED, CST6CDT, as_date, as_hour) %>% filter(id_vehicle == "2020-07-13-1" & as_hour=="6")
julio_2020_07_13_1 <- julio13 %>%  select(id_vehicle, Id, TIMESTAMP, VEHICLE, ROUTEID, LABEL, LATITUDE, LONGITUDE, BEARING, ODOMETER, SPEED, CST6CDT, as_date, as_hour) %>% filter(id_vehicle == "2020-07-13-1")
#¿cuántos casos repetidos hay del caso 2020-07-13-1-6??
#nrow(julio_2020_07_13_1_6 %>%  select(id_vehicle, Id, TIMESTAMP, VEHICLE, ROUTEID, LABEL, LATITUDE, LONGITUDE, BEARING, ODOMETER, SPEED, CST6CDT, as_date) %>% filter(id_vehicle == "2020-07-13-1" & as_hour=="6"))
df_start_1 <- julio_13_1[!duplicated(julio_13_1$id_vehicle),]
df_end_1 <- julio_13_1[rev(!duplicated(rev(julio_13_1$id_vehicle))),]
st_ed_1<- dplyr::left_join(df_start_1, df_end_1, by="id_vehicle")
#################

#////Arriba todo fine//////////
#En la siguiente sección se hace una verificación de los resultados con geosphere, trajr y TrackReconstruction
#Ejemplo con el  caso "2020-07-13-1"
###Resultados con trajr basado en la vignette cran.rstudio.com/web/packages/trajr/vignettes/trajr-vignette.html
coords <- data.frame(x = julio_2020_07_13_1_6$LONGITUDE, 
                     y = julio_2020_07_13_1_6$LATITUDE, 
                     times = julio_2020_07_13_1_6$CST6CDT)
trj <- trajr::TrajFromCoords(coords)
trajr::TrajLength(trj)
trajr::TrajDistance(trj) #En línea recta
plot(trj)
trajr::TrajStepLengths(trj)
head(trajr::TrajStepLengths(trj))
tsl01 <- trajr::TrajStepLengths(trj)
sum(tsl01)
######################

coords <- data.frame(x= c(-99.1460, -99.1439, -99.1437, -99.1444, -99.1441, -99.1443),
                     y= c(19.4737, 19.4680, 19.4675, 19.4631, 19.4599, 19.4589),
                     times= c(1, 2, 3, 4, 5, 6))
trj <- trajr::TrajFromCoords(coords)
plot(trj)
trajr::TrajDirectionalChange(trj)
trajr::TrajStepLengths(trj)
head(trajr::TrajStepLengths(trj))

#TrackReconstruction

TrackReconstruction::CalcDistance(19.4737, -99.1460, 19.4680, -99.1439)
TrackReconstruction::CalcDistance(19.4680, -99.1439, 19.4675, -99.1437)
TrackReconstruction::CalcDistance(19.4675, -99.1437, 19.4631, -99.1414)
TrackReconstruction::CalcDistance(st_ed_1$LATITUDE.x, st_ed_1$LONGITUDE.x, st_ed_1$LATITUDE.y, st_ed_1$LONGITUDE.y) #en km
TrackReconstruction::CalcBearing(st_ed_1$LATITUDE.x, st_ed_1$LONGITUDE.x, st_ed_1$LATITUDE.y, st_ed_1$LONGITUDE.y) #en km
disttrack <- as.data.frame(TrackReconstruction::CalcDistance(trj2$LATITUDE, trj2$LONGITUDE, trj2$LATITUDE2, trj2$LONGITUDE2)) #Distancias calculadas en km


#Estimación con geosphere
P1 <- c(-99.1460, 19.4737)
P2 <- c(-99.1439, 19.4680)
P3 <- c(-99.1437, 19.4675)
P4 <- c(-99.1444, 19.4631)
P5 <- c(-99.1441, 19.4599)
P6 <- c(-99.1443, 19.4589)
P7 <- c(-99.1452, 19.4550)
P8 <- c(-99.1461, 19.4513)
P9 <- c(-99.1471, 19.4467)
P10 <- c(-99.1474,19.4456)
geosphere::distCosine(P1, P2)#En metros
geosphere::distCosine(P2, P3)
geosphere::distCosine(P3, P4)
geosphere::distCosine(P4, P5)
geosphere::distCosine(P5,P6)
geosphere::distm(c(trj2$LONGITUDE, trj2$LATITUDE), c(trj2$LONGITUDE2, trj2$LATITUDE2))
geosphere::bearing(P3, P4) #No sé en qué unidades está el resultado.
################
x2 <-lead(coords$x, n=1L)
y2 <-lead(coords$y, n=1L)
coords <- cbind(coords, x2)
coords <- cbind(coords, y2)
coords$dist <- TrackReconstruction::CalcDistance(coords$y, coords$x, coords$y2, coords$x2)
sum(coords$dist, na.rm = TRUE)

###############
#######
#El código de abajo sirve para encontrar el primer caso duplicado y el último
df_start_1 <- julio_6_1[!duplicated(julio_6_1$id_vehicle),]
df_end_1 <- julio_6_1[rev(!duplicated(rev(julio_6_1$id_vehicle))),]
st_ed_1<- dplyr::left_join(df_start_1, df_end_1, by="id_vehicle")
#######

#################################
#Automatización de los cálculos #
#################################

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
julio <- julio %>%  select(id_vehicle, Id, TIMESTAMP, VEHICLE, ROUTEID, STARTTIME, STARTDATE, SCHEDULE_RELATIONSHIP, LABEL, LATITUDE, LONGITUDE, BEARING, ODOMETER, SPEED, CURRENTSTATUS, LECTURA, CST6CDT, as_date, as_hour) %>% filter(as_hour == "6" | as_hour=="7" | as_hour=="8" | as_hour=="9"| as_hour=="10" | as_hour=="11"| as_hour=="12"| as_hour=="13"| as_hour=="14"| as_hour=="15"| as_hour=="16"| as_hour=="17"| as_hour=="18"| as_hour=="19" | as_hour=="20"| as_hour=="21"| as_hour=="22"| as_hour=="23")

#Extraer datos del 13 de julio
julio13 <- julio %>%  select(id_vehicle, Id, TIMESTAMP, VEHICLE, ROUTEID, STARTTIME, STARTDATE, SCHEDULE_RELATIONSHIP, LABEL, LATITUDE, LONGITUDE, BEARING, ODOMETER, SPEED, CURRENTSTATUS, LECTURA, CST6CDT, as_date, as_hour) %>% filter(as_date == "2020-07-13")
unicos_13 <- unique(julio13$id_vehicle)

#Unión espacial con el gtfs estático para conocer la ruta a la que pertenecen
ggmap01<- ggmap::LonLat2XY(julio13$LONGITUDE, julio13$LATITUDE, zoom = TRUE)



###Este código genera 1 dataframe por id_vehicle
for (i in julio13jn1y2$id_vehicle) {
  assign(i,data.frame(julio13jn1y2 %>% select(id_vehicle, geometry, CST6CDT,ruta) %>% filter(., id_vehicle==i)))
}

for (i in julio13$id_vehicle) {
  julio13 %>% select(id_vehicle, LATITUDE, LONGITUDE, CST6CDT) %>% filter(., id_vehicle==i)
  nrow(i)
}

for(i in julio13$id_vehicle){
julio13 %>% select(id_vehicle, LATITUDE, LONGITUDE, CST6CDT) %>% filter(julio13$id_vehicle==i) %>%
  mutate(., lead=lead(LONGITUDE, n=1))
}
####Pruebas(revisar este código)

juliolag <-julio13 %>% select(id_vehicle, LATITUDE, LONGITUDE, CST6CDT) %>% filter(julio13$id_vehicle=="i") %>%
mutate(., newvariable=lag(LONGITUDE, n=2))


juliolag<-function(i)
  for(i in julio13$id_vehicle){
  julio13 %>% select(id_vehicle, LATITUDE, LONGITUDE, CST6CDT) %>% filter(julio13$id_vehicle==i) %>%
    mutate(., lead=lead(LONGITUDE, n=1))
}

=======
julio13 %>% select(id_vehicle, LATITUDE, LONGITUDE, CST6CDT) %>% filter(., id_vehicle==i)

####Pruebas(revisar este código)

juliolagueado<-julio13 %>% select(id_vehicle, LATITUDE, LONGITUDE, CST6CDT) %>% filter(julio13$id_vehicle=="2020-07-13-771") %>%
mutate(., newvariable=lag(LONGITUDE, n=2))


juliolageado<<-julio13 %>% mutate(., newvariable= lag(julio13$LONGITUDE, n=2))
###############################################################################################################
julio_funct <- function(i,j) {
    j<-julio13 %>% select(id_vehicle, LATITUDE, LONGITUDE, CST6CDT) %>% filter(julio13$id_vehicle==i) %>%
    mutate(., lat_lead=lead(LATITUDE, n=1)) %>% mutate(., lon_lead=lead(LONGITUDE, n=1)) 
    mutate(j, dist = TrackReconstruction::CalcDistance(j$LATITUDE, j$LONGITUDE, j$lat_lead, j$lon_lead))
    }

consuma<- julio_funct("2020-07-13-1077")

length <- sum(consuma$dist, na.rm = TRUE)

range<- range(consuma$CST6CDT)
diftiempo<- as.data.frame(range[2] - range[1])

################################################################################################################

julio_funct <- function(i,j) {
  j<-julio13 %>% select(id_vehicle, LATITUDE, LONGITUDE, CST6CDT) %>% filter(julio13$id_vehicle==i) %>%
    mutate(., lat_lead=lead(LATITUDE, n=1)) %>% mutate(., lon_lead=lead(LONGITUDE, n=1)) 
  mutate(j, dist = TrackReconstruction::CalcDistance(j$LATITUDE, j$LONGITUDE, j$lat_lead, j$lon_lead))
}

julio_for<- for (i in julio13$id_vehicle) {
  julio_funct("i")
  length <- sum(consuma$dist, na.rm = TRUE)
  range<- range(consuma$CST6CDT)
  diftiempo<- as.data.frame(range[2] - range[1])
    
}

julio_for <- list(for (i in julio13$id_vehicle) {
  julio_funct(i)
})
