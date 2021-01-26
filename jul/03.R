#Verificar
#Leer líneas del metrobus.Esta base tiene 8 variables: name, line, operating, serv_side, bus_length, oper_days, nombre y geometry. 
lineas.metrobus <- sf::st_read("D:/Escritorio/lineas.metrobus/lineas-metrobus.shp")
#Leer mes con el que se trabajará (jul.jn) y dar formato. jul.jn tiene 4,518,151 observaciones y 17 variables.
#nos quedamos sólo con 4 variables que habrán de manipularse.
Sys.time()
jul.jn <- read.csv("C:/Users/rdelatorre/Desktop/gtfs.metrobus/jul/jul.jn.csv")
jul.jn <-  jul.jn %>% select(Id, TIMESTAMP,LECTURA, geometry)
Sys.time()

#Cambiar nombres de las variables
names(jul.jn) <- c("timestamp", "vehicle", "x", "y")

#Formato a las variables. Comencemos por vehicle, ya que la de timestamp se queda igual. Revisar que las variables hayan quedado como quiero.
jul.jn$vehicle <- substr(jul.jn$vehicle, 4, 100000)
jul.jn$vehicle <- as.factor(jul.jn$vehicle)
jul.jn$x <- chartr("c(", "  ", jul.jn$x)
jul.jn$x <- as.numeric(jul.jn$x)
jul.jn$y <- chartr(")", " ", jul.jn$y)
jul.jn$y <- as.numeric(jul.jn$y)
jul.jn$cst6cdt<- lubridate::as_datetime(jul.jn$timestamp, tz="CST6CDT")
jul.jn$day <- lubridate::day(jul.jn$cst6cdt)
jul.jn$hour <- lubridate::hour(jul.jn$cst6cdt)


#####Separar por fecha y hora

jul.jn <- jul.jn %>% select(timestamp, vehicle, x, y, cst6cdt, day, hour) %>% filter(day=="15" & hour=="16")


#Union de la base jul.jn con las líneas de Metrobus. Desde aquí vamos preparando el código para quedarnos con la línea 1. 
Sys.time()
jul.jn <- sf::st_as_sf(jul.jn, coords = c("x", "y"), crs = 4326, agr = "constant") #Convertir la base jul.jn en una con campo geométrico.
est.l1 <- sf::st_join(jul.jn, lineas.metrobus, join = sf::st_nearest_feature, left = T)
Sys.time()

#Separar claves de Metrobus para poder seleccionar la línea 1. Seleccionar línea 1.
Sys.time()
est.l1$l.mbus <- substr(est.l1$name, 1, 4)
est.l1<- est.l1 %>% select(timestamp, vehicle, cst6cdt, name, nombre, geometry, l.mbus) %>% filter(l.mbus=="MB01")
Sys.time()

Sys.time()
#Unir con las estaciones de la línea 1. Las estaciones de la ruta 1 las estimé en un paso previo.
#Leer
l1.mbus.est <- sf::st_read("D:/Escritorio/gtfs_estatico/l1.mbus.est03.shp")
#Unir
l1.mbus.est <- sf::st_join(est.l1, l1.mbus.est, join=sf::st_nearest_feature, left= T)
Sys.time()

l1.mbus.est$lon <- as.numeric(as.character(l1.mbus.est$lon))
l1.mbus.est$lat <- as.numeric(as.character(l1.mbus.est$lat))


l1.mbus.est <-l1.mbus.est %>%  select(timestamp, vehicle, cst6cdt, nombre.y, lon, lat)

#En este punto ya quedó nuestra base de datos mensual con coordenadas que arroja el gtfs
#No me queda claro para que hice las siguientes líneas
#l1.mbus.est$day<- lubridate::day(l1.mbus.est$cstlcdt)
#l1.mbus.est$hour<- lubridate::hour(l1.mbus.est$cstlcdt)
#l1.mbus.est$id.vehi <- paste0(l1.mbus.est$day, sep=".", l1.mbus.est$hour)

#Check. Creación de un dataframe por vehículo ## Voy aquí

unicos<- unique((l1.mbus.est$vehicle))

Sys.time()
for (i in unicos) {
  assign(paste0("vehi", sep=".", i), data.frame(l1.mbus.est %>%  select(timestamp, vehicle, cst6cdt, nombre.y, lon, lat ) %>% filter (l1.mbus.est$vehicle==i)))  
}
Sys.time()
#
lista <- mget(ls(pattern = "vehi.")) # aquí están los vehículos
#
Sys.time()
lista <- lapply(lista, function(x) mutate(x, y.l=lead(lat, n=1)))
lista <- lapply(lista, function(x) mutate(x, x.l=lead(lon, n=1)))
lista <- lapply(lista, function (j) mutate(j, dist = TrackReconstruction::CalcDistance(j$lat, j$lon, j$y.l, j$x.l)))#Da la distancia en kilómetros
lista <- lapply(lista, function(k) mutate(k, dia = lubridate::day(k$cst6cdt)))
lista <- lapply(lista, function(k) mutate(k, hora = lubridate::hour(k$cst6cdt)))
Sys.time()
####
#Agregado de distancias recorridas a nivel mensual

suma <- sapply(lista, function(x) sum(x$dist, na.rm = TRUE))
rango <- sapply(lista, function(x) range(x$cst6cdt, na.rm = TRUE)) 
rango_t <- data.frame(t(rango))
head<- as.character(sapply (lista, function(x) head(x$nombre.y,1)))
tail<- as.character(sapply (lista, function(x) tail(x$nombre.y,1)))

dura<- as.vector(lubridate::as_datetime(rango_t$X2) - lubridate::as_datetime(rango_t$X1))
dura <- dura/60/60
velo <- suma/dura


cbind<- as.data.frame(cbind(suma, dura, velo, head, tail))
cbind$dia <- as.factor("15")
cbind$hora <- as.factor("16")
write.csv(cbind, "C:/Users/rdelatorre/Desktop/gtfs.metrobus/jul/jul1516.csv")


