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
jul.jn <- sf::st_as_sf(jul.jn, coords = c("x", "y"), crs = 4326, agr = "constant") #Convertir la base jul.jn en una con campo geométrico.
est.l1 <- sf::st_join(jul.jn, lineas.metrobus, join = sf::st_nearest_feature, left = T)

#Separar claves de Metrobus para poder seleccionar la línea 1. Seleccionar línea 1.
est.l1$l.mbus <- substr(est.l1$name, 1, 4)
est.l1<- est.l1 %>% select(timestamp, vehicle, cstlcdt, name, nombre, geometry, l.mbus) %>% filter(l.mbus=="MB01")

#Unir con las estaciones de la línea 1. Pero antes, corregir los nombres de las estaciones.
l1.mbus.est <- sf::st_read("D:/Escritorio/gtfs_estatico/est.l1.shp")

#Corregir strings
l1.mbus.est$nombre.y<- stringr::str_replace(l1.mbus.est$nombre.y, "Buenavista. LÃ­nea 1.", "Buenavista")
l1.mbus.est$nombre.y<- stringr::str_replace(l1.mbus.est$nombre.y, "Plaza de la RepÃºblica", "Plaza de la Republica")
l1.mbus.est$nombre.y<- stringr::str_replace(l1.mbus.est$nombre.y, "Ã\u0081lvaro ObregÃ³n", "Alvaro Obregon")
l1.mbus.est$nombre.y<- stringr::str_replace(l1.mbus.est$nombre.y, "Dr. GÃ¡lvez", "Dr. Galvez")
l1.mbus.est$nombre.y<- stringr::str_replace(l1.mbus.est$nombre.y, "Manuel GonzÃ¡lez", "Manuel Gonzalez")
l1.mbus.est$nombre.y<- stringr::str_replace(l1.mbus.est$nombre.y, "RevoluciÃ³n", "Revolucion")
l1.mbus.est$nombre.y<- stringr::str_replace(l1.mbus.est$nombre.y, "La Raza. LÃ­nea 1.", "La Raza")
l1.mbus.est$nombre.y<- stringr::str_replace(l1.mbus.est$nombre.y, "San SimÃ³n", "San Simon")
l1.mbus.est$nombre.y<- stringr::str_replace(l1.mbus.est$nombre.y, "Nuevo LeÃ³n", "Nuevo Leon")
l1.mbus.est$nombre.y<- stringr::str_replace(l1.mbus.est$nombre.y, "NÃ¡poles", "Napoles")
l1.mbus.est$nombre.y<- stringr::str_replace(l1.mbus.est$nombre.y, "Santa Ãšrsula", "Santa Ursula")
l1.mbus.est$nombre.y<- stringr::str_replace(l1.mbus.est$nombre.y, "RÃ­o Churubusco", "Rio Churubusco")
l1.mbus.est$nombre.y<- stringr::str_replace(l1.mbus.est$nombre.y, "Villa OlÃ­mpica", "Villa Olimpica")

#Continuar con la union
l1.mbus.est <- sf::st_join(est.l1, l1.mbus.est, join=sf::st_nearest_feature, left= T)
l1.mbus.est <-l1.mbus.est %>%  select(timestamp, vehicle, cstlcdt, nombre.y, lon, lat)

#Ordenar por vehiculo
l1.mbus.est<- as.data.frame(sort(l1.mbus.est$vehicle, decreasing = FALSE, na.last = TRUE))
l1.mbus.est %>% select(timestamp, vehicle, cstlcdt, nombre.y, geometry) %>% filter(vehicle=="1")







