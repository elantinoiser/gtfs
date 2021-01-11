#Verificar
#Leer líneas del metrobus
lineas.metrobus <- sf::st_read("D:/Escritorio/lineas.metrobus/lineas-metrobus.shp")
#Leer mes con el que se trabajará (jul.jn) y dar formato
jul.jn <- read.csv("C:/Users/rdelatorre/Desktop/gtfs.metrobus/jul/jul.jn.csv")
jul.jn <-  jul.jn %>% select(Id, TIMESTAMP,LECTURA, geometry)

#Cambiar nombres de las variables
names(jul.jn) <- c("timestamp", "vehicle", "x", "y")

#Formato a las variables. Comencemos por vehicle, ya que la de timestamp se queda igual.
jul.jn$vehicle <- substr(jul.jn$vehicle, 4, 100000)
jul.jn$vehicle <- as.factor(jul.jn$vehicle)
jul.jn$x <- chartr("c(", "  ", jul.jn$x)
jul.jn$x <- as.numeric(jul.jn$x)
jul.jn$y <- chartr(")", " ", jul.jn$y)
jul.jn$y <- as.numeric(jul.jn$y)
jul.jn$cstlcdt<- lubridate::as_datetime(jul.jn$timestamp, tz="CST6CDT")


#Union de la base jul.jn con las líneas de Metrobus. Desde aquí vamos preparando el código para quedarnos con la línea 1. 
Sys.time()
jul.jn <- sf::st_as_sf(jul.jn, coords = c("x", "y"), crs = 4326, agr = "constant") #Convertir la base jul.jn en una con campo geométrico.
est.l1 <- sf::st_join(jul.jn, lineas.metrobus, join = sf::st_nearest_feature, left = T)
Sys.time()

#Separar claves de Metrobus para poder seleccionar la línea 1. Seleccionar línea 1.
Sys.time()
est.l1$l.mbus <- substr(est.l1$name, 1, 4)
est.l1<- est.l1 %>% select(timestamp, vehicle, cstlcdt, name, nombre, geometry, l.mbus) %>% filter(l.mbus=="MB01")
Sys.time()
#Unir con las estaciones de la línea 1.
l1.mbus.est <- sf::st_read("D:/Escritorio/gtfs_estatico/l1.mbus.est.shp")

#Continuar con la union
l1.mbus.est <- sf::st_join(est.l1, l1.mbus.est, join=sf::st_nearest_feature, left= T)
l1.mbus.est <-l1.mbus.est %>%  select(timestamp, vehicle, cstlcdt, nombre.y, lon, lat)

#En este punto ya quedó nuestra base de datos mensual con coordenadas que arroja el gtfs
#No me queda claro para que hice las siguientes líneas
#l1.mbus.est$day<- lubridate::day(l1.mbus.est$cstlcdt)
#l1.mbus.est$hour<- lubridate::hour(l1.mbus.est$cstlcdt)
#l1.mbus.est$id.vehi <- paste0(l1.mbus.est$day, sep=".", l1.mbus.est$hour)

#Check. Creación de un dataframe por vehículo ## Voy aquí

unicos<- unique((l1.mbus.est$vehicle))

for (i in unicos) {
  assign(paste0("vehi", sep=".", i), data.frame(l1.mbus.est %>%  select(timestamp, vehicle, cstlcdt, nombre.y, lon, lat ) %>% filter (l1.mbus.est$vehicle==i)))  
}

#
lista <- mget(ls(pattern = "vehi.")) # aquí están los vehículos
#
lista02 <- lista
lista02 <- lapply(lista02, function(x) mutate(x, lat=as.numeric(lat)))
lista02 <- lapply(lista02, function(x) mutate(x, lat=as.double(lon)))
lista02 <- lapply(lista02, function(x) mutate(x, lat=as.numeric(lon)))



lista02 <- lapply(lista02, function(x) mutate(x, y.l=lead(lat, n=1)))
lista02 <- lapply(lista02, function(x) mutate(x, y.l=as.numeric(y.l)))

lista02 <- lapply(lista02, function(x) mutate(x, x.l=lead(lon, n=1)))
lista02 <- lapply(lista02, function (j) mutate(j, dist = TrackReconstruction::CalcDistance(j$lat, j$lon, j$y.l, j$x.l)))#Da la distancia en kilómetros
lista02 <- lapply(lista02, function(k) mutate(k, dia = lubridate::day(k$cstlcdt)))
lista02 <- lapply(lista02, function(k) mutate(k, hora = lubridate::hour(k$cstlcdt)))
####
#Agregado de distancias recorridas a nivel mensual

suma <- sapply(lista02, function(x) sum(x$dist, na.rm = TRUE))
rango <- sapply(lista02, function(x) range(x$cstlcdt, na.rm = TRUE)) 
rango_t <- data.frame(t(rango))






