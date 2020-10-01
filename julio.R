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

<<<<<<< HEAD
julio_fun <- function(i,j) {
=======
julio_funct <- function(i,j) {
>>>>>>> 01d99d4b1fa5d895bae0b1f2b5c2f33ced321a2b
  j<-julio13 %>% select(id_vehicle, LATITUDE, LONGITUDE, CST6CDT) %>% filter(julio13$id_vehicle==i) %>%
    mutate(., lat_lead=lead(LATITUDE, n=1)) %>% mutate(., lon_lead=lead(LONGITUDE, n=1)) 
  mutate(j, dist = TrackReconstruction::CalcDistance(j$LATITUDE, j$LONGITUDE, j$lat_lead, j$lon_lead))
}

for (i in julio13$id_vehicle) {
  assign(i, data.frame(julio_funct(i)))
}








for (i in julio13$id_vehicle) {
  assign (i, data.frame(julio_funct(i)))
}

<<<<<<< HEAD

assign ("Lista", list(append(julio_funct(i))))

sum(`2020-07-13-1077`$dist, na.rm = TRUE)
range<- range(`2020-07-13-1077`$CST6CDT)
as.data.frame(range[2] - range[1])

sum(`2020-07-13-10010`$dist, na.rm = TRUE)
range<- range(`2020-07-13-10010`$CST6CDT)
as.data.frame(range[2] - range[1])

sum(`2020-07-13-10000`$dist, na.rm = TRUE)
range<- range(`2020-07-13-10000`$CST6CDT)
as.data.frame(range[2] - range[1])

################################################################################################################
mylist <- list(`2020-07-13-169`, `2020-07-13-10010`, `2020-07-13-10000`, `2020-07-13-10001`)
sapply(mylist, function(x) sum(x$dist, na.rm = TRUE))
sapply(mylist, function(x) range(x$CST6CDT, na.rm = TRUE)) %>% range[2] - range[1]

########################

unicos <- as.data.frame(unicos)

mylist <- list(`2020-07-13-169`
               ,`2020-07-13-57`
               ,`2020-07-13-178`
               ,`2020-07-13-179`
               ,`2020-07-13-180`
               ,`2020-07-13-1197`
               ,`2020-07-13-1286`
               ,`2020-07-13-1287`
               ,`2020-07-13-1291`
               ,`2020-07-13-1292`
               ,`2020-07-13-1295`
               ,`2020-07-13-1296`
               ,`2020-07-13-360`
               ,`2020-07-13-362`
               ,`2020-07-13-364`
               ,`2020-07-13-365`
               ,`2020-07-13-366`
               ,`2020-07-13-367`
               ,`2020-07-13-369`
               ,`2020-07-13-375`
               ,`2020-07-13-377`
               ,`2020-07-13-380`
               ,`2020-07-13-1017`
               ,`2020-07-13-1018`
               ,`2020-07-13-398`
               ,`2020-07-13-399`
               ,`2020-07-13-400`
               ,`2020-07-13-407`
               ,`2020-07-13-408`
               ,`2020-07-13-416`
               ,`2020-07-13-418`
               ,`2020-07-13-422`
               ,`2020-07-13-424`
               ,`2020-07-13-425`
               ,`2020-07-13-432`
               ,`2020-07-13-441`
               ,`2020-07-13-452`
               ,`2020-07-13-1019`
               ,`2020-07-13-1021`
               ,`2020-07-13-1022`
               ,`2020-07-13-1`
               ,`2020-07-13-2`
               ,`2020-07-13-3`
               ,`2020-07-13-5`
               ,`2020-07-13-8`
               ,`2020-07-13-9`
               ,`2020-07-13-10`
               ,`2020-07-13-11`
               ,`2020-07-13-14`
               ,`2020-07-13-16`
               ,`2020-07-13-20`
               ,`2020-07-13-24`
               ,`2020-07-13-25`
               ,`2020-07-13-26`
               ,`2020-07-13-28`
               ,`2020-07-13-29`
               ,`2020-07-13-30`
               ,`2020-07-13-33`
               ,`2020-07-13-35`
               ,`2020-07-13-36`
               ,`2020-07-13-37`
               ,`2020-07-13-38`
               ,`2020-07-13-39`
               ,`2020-07-13-40`
               ,`2020-07-13-43`
               ,`2020-07-13-45`
               ,`2020-07-13-48`
               ,`2020-07-13-49`
               ,`2020-07-13-51`
               ,`2020-07-13-52`
               ,`2020-07-13-53`
               ,`2020-07-13-54`
               ,`2020-07-13-55`
               ,`2020-07-13-56`
               ,`2020-07-13-1009`
               ,`2020-07-13-1147`
               ,`2020-07-13-1149`
               ,`2020-07-13-1151`
               ,`2020-07-13-1152`
               ,`2020-07-13-981`
               ,`2020-07-13-982`
               ,`2020-07-13-983`
               ,`2020-07-13-984`
               ,`2020-07-13-985`
               ,`2020-07-13-986`
               ,`2020-07-13-461`
               ,`2020-07-13-467`
               ,`2020-07-13-468`
               ,`2020-07-13-469`
               ,`2020-07-13-472`
               ,`2020-07-13-473`
               ,`2020-07-13-474`
               ,`2020-07-13-476`
               ,`2020-07-13-477`
               ,`2020-07-13-479`
               ,`2020-07-13-481`
               ,`2020-07-13-482`
               ,`2020-07-13-483`
               ,`2020-07-13-486`
               ,`2020-07-13-487`
               ,`2020-07-13-489`
               ,`2020-07-13-490`
               ,`2020-07-13-491`
               ,`2020-07-13-492`
               ,`2020-07-13-493`
               ,`2020-07-13-495`
               ,`2020-07-13-496`
               ,`2020-07-13-497`
               ,`2020-07-13-498`
               ,`2020-07-13-499`
               ,`2020-07-13-500`
               ,`2020-07-13-501`
               ,`2020-07-13-502`
               ,`2020-07-13-503`
               ,`2020-07-13-504`
               ,`2020-07-13-507`
               ,`2020-07-13-509`
               ,`2020-07-13-510`
               ,`2020-07-13-512`
               ,`2020-07-13-544`
               ,`2020-07-13-546`
               ,`2020-07-13-549`
               ,`2020-07-13-550`
               ,`2020-07-13-551`
               ,`2020-07-13-552`
               ,`2020-07-13-554`
               ,`2020-07-13-556`
               ,`2020-07-13-557`
               ,`2020-07-13-558`
               ,`2020-07-13-559`
               ,`2020-07-13-562`
               ,`2020-07-13-10000`
               ,`2020-07-13-1178`
               ,`2020-07-13-1179`
               ,`2020-07-13-1277`
               ,`2020-07-13-1074`
               ,`2020-07-13-1076`
               ,`2020-07-13-1077`
               ,`2020-07-13-1078`
               ,`2020-07-13-1080`
               ,`2020-07-13-1081`
               ,`2020-07-13-1082`
               ,`2020-07-13-1083`
               ,`2020-07-13-1084`
               ,`2020-07-13-1085`
               ,`2020-07-13-1086`
               ,`2020-07-13-1087`
               ,`2020-07-13-1088`
               ,`2020-07-13-1089`
               ,`2020-07-13-1090`
               ,`2020-07-13-1091`
               ,`2020-07-13-1093`
               ,`2020-07-13-1094`
               ,`2020-07-13-1095`
               ,`2020-07-13-1097`
               ,`2020-07-13-1098`
               ,`2020-07-13-1100`
               ,`2020-07-13-1101`
               ,`2020-07-13-1102`
               ,`2020-07-13-1103`
               ,`2020-07-13-1180`
               ,`2020-07-13-1181`
               ,`2020-07-13-1182`
               ,`2020-07-13-1183`
               ,`2020-07-13-1104`
               ,`2020-07-13-1105`
               ,`2020-07-13-1106`
               ,`2020-07-13-1107`
               ,`2020-07-13-1108`
               ,`2020-07-13-1110`
               ,`2020-07-13-1111`
               ,`2020-07-13-1112`
               ,`2020-07-13-1113`
               ,`2020-07-13-1114`
               ,`2020-07-13-1115`
               ,`2020-07-13-1117`
               ,`2020-07-13-1120`
               ,`2020-07-13-1122`
               ,`2020-07-13-1123`
               ,`2020-07-13-1127`
               ,`2020-07-13-1128`
               ,`2020-07-13-1129`
               ,`2020-07-13-1130`
               ,`2020-07-13-1131`
               ,`2020-07-13-1132`
               ,`2020-07-13-1133`
               ,`2020-07-13-1138`
               ,`2020-07-13-1139`
               ,`2020-07-13-1142`
               ,`2020-07-13-1145`
               ,`2020-07-13-1146`
               ,`2020-07-13-1281`
               ,`2020-07-13-702`
               ,`2020-07-13-703`
               ,`2020-07-13-705`
               ,`2020-07-13-706`
               ,`2020-07-13-707`
               ,`2020-07-13-709`
               ,`2020-07-13-710`
               ,`2020-07-13-711`
               ,`2020-07-13-714`
               ,`2020-07-13-716`
               ,`2020-07-13-718`
               ,`2020-07-13-720`
               ,`2020-07-13-725`
               ,`2020-07-13-727`
               ,`2020-07-13-728`
               ,`2020-07-13-729`
               ,`2020-07-13-730`
               ,`2020-07-13-731`
               ,`2020-07-13-732`
               ,`2020-07-13-734`
               ,`2020-07-13-736`
               ,`2020-07-13-740`
               ,`2020-07-13-741`
               ,`2020-07-13-742`
               ,`2020-07-13-743`
               ,`2020-07-13-744`
               ,`2020-07-13-747`
               ,`2020-07-13-749`
               ,`2020-07-13-750`
               ,`2020-07-13-754`
               ,`2020-07-13-758`
               ,`2020-07-13-760`
               ,`2020-07-13-761`
               ,`2020-07-13-762`
               ,`2020-07-13-763`
               ,`2020-07-13-764`
               ,`2020-07-13-766`
               ,`2020-07-13-767`
               ,`2020-07-13-769`
               ,`2020-07-13-770`
               ,`2020-07-13-771`
               ,`2020-07-13-772`
               ,`2020-07-13-774`
               ,`2020-07-13-777`
               ,`2020-07-13-779`
               ,`2020-07-13-781`
               ,`2020-07-13-782`
               ,`2020-07-13-783`
               ,`2020-07-13-784`
               ,`2020-07-13-788`
               ,`2020-07-13-789`
               ,`2020-07-13-790`
               ,`2020-07-13-1253`
               ,`2020-07-13-1058`
               ,`2020-07-13-1060`
               ,`2020-07-13-991`
               ,`2020-07-13-992`
               ,`2020-07-13-993`
               ,`2020-07-13-994`
               ,`2020-07-13-995`
               ,`2020-07-13-996`
               ,`2020-07-13-997`
               ,`2020-07-13-1153`
               ,`2020-07-13-881`
               ,`2020-07-13-882`
               ,`2020-07-13-1154`
               ,`2020-07-13-58`
               ,`2020-07-13-1037`
               ,`2020-07-13-1038`
               ,`2020-07-13-1039`
               ,`2020-07-13-10016`
               ,`2020-07-13-10017`
               ,`2020-07-13-10018`
               ,`2020-07-13-10021`
               ,`2020-07-13-10022`
               ,`2020-07-13-10023`
               ,`2020-07-13-10024`
               ,`2020-07-13-10025`
               ,`2020-07-13-1155`
               ,`2020-07-13-1156`
               ,`2020-07-13-1157`
               ,`2020-07-13-1158`
               ,`2020-07-13-1159`
               ,`2020-07-13-525`
               ,`2020-07-13-526`
               ,`2020-07-13-527`
               ,`2020-07-13-528`
               ,`2020-07-13-529`
               ,`2020-07-13-530`
               ,`2020-07-13-531`
               ,`2020-07-13-532`
               ,`2020-07-13-533`
               ,`2020-07-13-535`
               ,`2020-07-13-1040`
               ,`2020-07-13-1041`
               ,`2020-07-13-1042`
               ,`2020-07-13-1257`
               ,`2020-07-13-1259`
               ,`2020-07-13-1261`
               ,`2020-07-13-1262`
               ,`2020-07-13-1263`
               ,`2020-07-13-536`
               ,`2020-07-13-538`
               ,`2020-07-13-1045`
               ,`2020-07-13-1063`
               ,`2020-07-13-10001`
               ,`2020-07-13-10011`
               ,`2020-07-13-10012`
               ,`2020-07-13-10013`
               ,`2020-07-13-1172`
               ,`2020-07-13-1173`
               ,`2020-07-13-1174`
               ,`2020-07-13-1176`
               ,`2020-07-13-1177`
               ,`2020-07-13-1264`
               ,`2020-07-13-1266`
               ,`2020-07-13-1160`
               ,`2020-07-13-1161`
               ,`2020-07-13-1162`
               ,`2020-07-13-1163`
               ,`2020-07-13-1164`
               ,`2020-07-13-1165`
               ,`2020-07-13-1166`
               ,`2020-07-13-1167`
               ,`2020-07-13-1168`
               ,`2020-07-13-987`
               ,`2020-07-13-990`
               ,`2020-07-13-1219`
               ,`2020-07-13-1220`
               ,`2020-07-13-1222`
               ,`2020-07-13-1223`
               ,`2020-07-13-1247`
               ,`2020-07-13-1248`
               ,`2020-07-13-1267`
               ,`2020-07-13-1268`
               ,`2020-07-13-1269`
               ,`2020-07-13-1270`
               ,`2020-07-13-1283`
               ,`2020-07-13-1284`
               ,`2020-07-13-1271`
               ,`2020-07-13-1064`
               ,`2020-07-13-1066`
               ,`2020-07-13-1068`
               ,`2020-07-13-1069`
               ,`2020-07-13-1070`
               ,`2020-07-13-1071`
               ,`2020-07-13-1072`
               ,`2020-07-13-1073`
               ,`2020-07-13-1170`
               ,`2020-07-13-1171`
               ,`2020-07-13-10015`
               ,`2020-07-13-1186`
               ,`2020-07-13-1187`
               ,`2020-07-13-1188`
               ,`2020-07-13-1191`
               ,`2020-07-13-1192`
               ,`2020-07-13-1236`
               ,`2020-07-13-1215`
               ,`2020-07-13-1216`
               ,`2020-07-13-1217`
               ,`2020-07-13-1218`
               ,`2020-07-13-10007`
               ,`2020-07-13-930`
               ,`2020-07-13-1225`
               ,`2020-07-13-1226`
               ,`2020-07-13-1227`
               ,`2020-07-13-1228`
               ,`2020-07-13-1237`
               ,`2020-07-13-1238`
               ,`2020-07-13-1239`
               ,`2020-07-13-1240`
               ,`2020-07-13-1024`
               ,`2020-07-13-1026`
               ,`2020-07-13-1035`
               ,`2020-07-13-10005`
               ,`2020-07-13-10003`
               ,`2020-07-13-10004`
               ,`2020-07-13-1230`
               ,`2020-07-13-1231`
               ,`2020-07-13-1232`
               ,`2020-07-13-1233`
               ,`2020-07-13-1234`
               ,`2020-07-13-1235`
               ,`2020-07-13-1193`
               ,`2020-07-13-378`
               ,`2020-07-13-485`
               ,`2020-07-13-560`
               ,`2020-07-13-1185`
               ,`2020-07-13-1282`
               ,`2020-07-13-726`
               ,`2020-07-13-1065`
               ,`2020-07-13-523`
               ,`2020-07-13-478`
               ,`2020-07-13-1224`
               ,`2020-07-13-712`
               ,`2020-07-13-1184`
               ,`2020-07-13-708`
               ,`2020-07-13-359`
               ,`2020-07-13-1298`
               ,`2020-07-13-1214`
               ,`2020-07-13-1034`
               ,`2020-07-13-10010`
               ,`2020-07-13-1079`
               ,`2020-07-13-363`
               ,`2020-07-13-176`
               ,`2020-07-13-765`
               ,`2020-07-13-999`
               ,`2020-07-13-1229`
               ,`2020-07-13-988`
               ,`2020-07-13-537`
               ,`2020-07-13-6`
               ,`2020-07-13-1265`
               ,`2020-07-13-534`
               ,`2020-07-13-1255`
               ,`2020-07-13-1221`
               ,`2020-07-13-1260`
               ,`2020-07-13-1148`
               ,`2020-07-13-545`
               ,`2020-07-13-1254`
               ,`2020-07-13-1258`
               ,`2020-07-13-22`
               ,`2020-07-13-1011`
               ,`2020-07-13-1175`
               ,`2020-07-13-44`
               ,`2020-07-13-1256`
               ,`2020-07-13-1044`
               ,`2020-07-13-724`
               ,`2020-07-13-713`
               ,`2020-07-13-717`
               ,`2020-07-13-1150`
               ,`2020-07-13-787`
               ,`2020-07-13-753`
               ,`2020-07-13-1135`
               ,`2020-07-13-1124`
)
sum<- sapply(mylist, function(x) sum(x$dist, na.rm = TRUE))
sum <- as.data.frame(sum)
range<- sapply(mylist, function(x) range(x$CST6CDT, na.rm = TRUE)) 
range_tras <- data.frame(t(range))
jul_13 <- cbind(unicos, sum)
jul_13 <- cbind(jul_13, range_tras)
jul_13$hrs <- jul_13$X2-jul_13$X1



=======
julio_for <- list(for (i in julio13$id_vehicle) {
  julio_funct(i)
})
>>>>>>> 01d99d4b1fa5d895bae0b1f2b5c2f33ced321a2b
