#https://r-spatial.github.io/sf/articles/sf3.html
#https://eujournal.org/files/journals/1/books/JeanFrancoisMas.pdf



#Leer lineas de metrobus
lineas.metrobus <- sf::st_read("D:/Escritorio/lineas.metrobus/lineas-metrobus.shp")
lineas.metrobus = lineas.metrobus %>% sf::st_set_crs(4326) %>% sf::st_transform(32214)

#Leer datos de julio
jul.jn <- read.csv("D:/Escritorio/gtfs.metrobus/jul/jul.jn.csv")

#Seleccionar solamente las columnas que me sirven 
jul.jn <-  jul.jn %>% select(Id, TIMESTAMP, ODOMETER, LECTURA, geometry)

#Cambiar nombres de las variables
names(jul.jn) <- c("timestamp", "vehicle", "speed", "x", "y")

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

jul.jn <- jul.jn %>% select(timestamp, vehicle, speed, x, y, cst6cdt, day, hour) %>% filter(y!="0")
jul.jn <- jul.jn %>% select(timestamp, vehicle, speed, x, y, cst6cdt, day, hour) %>% filter(x!="0")
jul.jn <- jul.jn %>% select(timestamp, vehicle, speed, x, y, cst6cdt, day, hour) %>% filter(speed!="0")


aggregate(jul.jn$speed, list(jul.jn$day, jul.jn$hour), FUN=mean)

aggregate(jul.jn$speed, list(jul.jn$day, jul.jn$hour), FUN=max)

aggregate(jul.jn$speed, list(jul.jn$day, jul.jn$hour), FUN=median)

aggregate(jul.jn$speed, list(jul.jn$day, jul.jn$hour), FUN = function(x) quantile(x, probs = 0.95))
