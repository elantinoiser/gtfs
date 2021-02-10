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


jul.jn <- sf::st_as_sf(jul.jn, coords = c("x", "y"), crs = 4326, agr = "constant") #Convertir la base jul.jn en una con campo geométrico.
jul.jn <- jul.jn %>% sf::st_set_crs(4326) %>% sf::st_transform(32214)

linea1.1<- sf::st_wrap_dateline(sf::st_sfc(sf::st_linestring(rbind(c(-99.1691, 19.2793),c(-99.1700, 19.2803), c(-99.1755, 19.2837), c(-99.1744, 19.2883), c(-99.1773, 19.2925), c(-99.1812, 19.2941), c(-99.1855, 19.2994), c(-99.1860, 19.3044), c(-99.1874, 19.3146), c(-99.1884, 19.3227), c(-99.1899, 19.3408))), crs = 4326))
linea1.1 = linea1.1 %>% sf::st_set_crs(4326) %>% sf::st_transform(32214)

st_buffer<-sf::st_buffer(linea1.1, 100)
st_within<- sf::st_within(jul.jn, st_buffer, sparse = FALSE)
jul.jn <- cbind(jul.jn, st_within)
jul.jn<- jul.jn %>% select(timestamp, vehicle, cst6cdt, speed, day, hour, st_within, geometry) %>% filter(st_within=="TRUE")











mean<- aggregate(jul.jn$speed, list(jul.jn$day, jul.jn$hour), FUN=mean)

max<- aggregate(jul.jn$speed, list(jul.jn$day, jul.jn$hour), FUN=max)

aggregate(jul.jn$speed, list(jul.jn$day, jul.jn$hour), FUN=min)


median<- aggregate(jul.jn$speed, list(jul.jn$day, jul.jn$hour), FUN=median)

var<- aggregate(jul.jn$speed, list(jul.jn$day, jul.jn$hour), FUN=var)


aggregate(jul.jn$speed, list(jul.jn$day, jul.jn$hour), FUN = function(x) quantile(x, probs = 0.95))

aggregate(jul.jn$speed, list(jul.jn$day, jul.jn$hour), FUN = function(x) sd(x))

aggregate(jul.jn$speed, list(jul.jn$day, jul.jn$hour), FUN = function(x) summary(x))


#Coeficiente de variacion

CV <- function(x, na.rm = FALSE) {
  sd(x, na.rm=na.rm) / mean(x, na.rm=na.rm)
}

cv<-aggregate(jul.jn$speed, list(jul.jn$day, jul.jn$hour), FUN = function(x) CV(x))

