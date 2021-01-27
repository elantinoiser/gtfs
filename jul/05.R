lineas.metrobus <- sf::st_read("D:/Escritorio/lineas.metrobus/lineas-metrobus.shp")
lineas.metrobus = lineas.metrobus %>% sf::st_set_crs(4326) %>% sf::st_transform(32214)

jul.jn <- read.csv("D:/Escritorio/gtfs.metrobus/jul/jul.jn.csv")

jul.jn <-  jul.jn %>% select(Id, TIMESTAMP,LECTURA, geometry)

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


jul.jn <- jul.jn %>% select(timestamp, vehicle, x, y, cst6cdt, day, hour) %>% filter(day=="15")


#Union de la base jul.jn con las líneas de Metrobus. Desde aquí vamos preparando el código para quedarnos con la línea 1. 
jul.jn <- sf::st_as_sf(jul.jn, coords = c("x", "y"), crs = 4326, agr = "constant") #Convertir la base jul.jn en una con campo geométrico.
jul.jn <- jul.jn %>% sf::st_set_crs(4326) %>% sf::st_transform(32214)

est.l1 <- sf::st_join(jul.jn, lineas.metrobus, join = sf::st_nearest_feature, left = T)

est.l1$l.mbus <- substr(est.l1$name, 1, 4)
est.l1<- est.l1 %>% select(timestamp, vehicle, cst6cdt, name, nombre, geometry, l.mbus) %>% filter(l.mbus=="MB01")

#Objeto de clase sf
linea1.1 <- sf::st_read("D:/Escritorio/gtfs_estatico/linea1.1.shp")
linea1.1 = linea1.1 %>% sf::st_set_crs(4326) %>% sf::st_transform(32214)

st_join<- sf::st_join(linea1.1, est.l1, join = nngeo::st_nn, k = 1, maxdist = 500)
