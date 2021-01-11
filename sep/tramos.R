#


#Jalar datos de septiembre y darles formato

sep01 <- read.csv("C:/Users/rdelatorre/Desktop/gtfs.metrobus/sep/sep01.csv")
names(sep01) <- c("timestamp", "vehicle", "speed", "date", "hour", "x", "y", "route")

sep01$y <- chartr(")", " ", sep01$y)
sep01$y <- as.numeric(sep01$y)
sep01$x <- substr(sep01$x, 3,10)
sep01$x <- as.numeric(sep01$x)

sep01$timestamp <- as.numeric(sep01$timestamp)
sep01$speed <- as.numeric(sep01$speed)
sep01$date <- as.POSIXct(sep01$date)
sep01$hour <- as.factor(sep01$hour)

sep01 <- sep01 %>% select(timestamp, vehicle, speed, date, hour, x, y, route) %>% filter(!hour==0 & !hour==1 & !hour==2 & !hour==3 & !hour==4  & !hour==5)
sep01 <- sep01 %>% select(timestamp, vehicle, speed, date, hour, x, y, route) %>% filter(route=="00L1")
sep01 <- sep01 %>% select(timestamp, vehicle, speed, date, hour, x, y, route) %>% filter(hour=="6")
boxplot(sep01$speed) #Mediana de 6 a lo largo de todo el día


sep01.sf<- sf::st_as_sf(sep01, coords = c("x", "y"), crs = 4326, agr = "constant")
tramos <- sf::st_join(sep01.sf, linea.1, join = st_nearest_feature)# Debe correrse library(nngeo) para que trabaje el st_nn

linea.1 <- sf::st_read("D:/Escritorio/gtfs_estatico/Linea01.shp")

est.l1 <- sf::st_read("D:/Escritorio/gtfs_estatico/est.l1.shp")
est.l1 <- sf::st_join(tramos, est.l1, join = st_nearest_feature)# Debe correrse library(nngeo) para que trabaje el st_nn
