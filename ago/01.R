#http://postgis.net/workshops/postgis-intro/index.html
#https://gist.github.com/ateucher/e2e5bd0b592f3efd6d56

buffer<- sf::read_sf("D:/Escritorio/estaciones.ohiiunam/buffer.shp")

buffer.5k <-sf::st_buffer(buffer, 5000)

l1.mbus.est <- sf::read_sf("D:/Escritorio/gtfs_estatico/l1.mbus.est03.shp")
l1.mbus.est <- l1.mbus.est %>% sf::st_set_crs(4326) %>% sf::st_transform(32214)

buffer.within <- sf::st_within(l1.mbus.est, buffer.5k, sparse = TRUE)
buffer.within<- as.data.frame(buffer.within)
buffer.within<- buffer.within[!duplicated(buffer.within[,1]), ]

buffer.within<-buffer.within[,2]
l1.mbus.est <- as.data.frame(l1.mbus.est)
l1.mbus.est<- cbind(l1.mbus.est, buffer.within)

l1.mbus.est<- sf::st_as_sf(l1.mbus.est, coords = c("lon", "lat"), crs = 4326)
l1.mbus.est <- l1.mbus.est %>% sf::st_set_crs(4326) %>% sf::st_transform(32214)


###############################################################################################################################
tramo.picacho <- l1.mbus.est %>% select(nombre, geometry, buffer.within) %>% filter(buffer.within=="3")
xy <- sf::st_coordinates(tramo.picacho)
xy.sort = tramo.picacho[order(xy[,"Y"], xy[,"Y"]),]
tramo.picacho <- (xy.sort %>% sf::st_coordinates() %>% sf::st_linestring())

tramo.dhuman <- l1.mbus.est %>% select(nombre, geometry, buffer.within) %>% filter(buffer.within=="1")
xy <- sf::st_coordinates(tramo.dhuman)
xy.sort = tramo.dhuman[order(xy[,"Y"], xy[,"Y"]),]
tramo.dhuman <- (xy.sort %>% sf::st_coordinates() %>% sf::st_linestring())

tramo.zapata <- l1.mbus.est %>% select(nombre, geometry, buffer.within) %>% filter(buffer.within=="4")
xy <- sf::st_coordinates(tramo.zapata)
xy.sort = tramo.zapata[order(xy[,"Y"], xy[,"Y"]),]
tramo.zapata <- (xy.sort %>% sf::st_coordinates() %>% sf::st_linestring())

tramo.sacmex <- l1.mbus.est %>% select(nombre, geometry, buffer.within) %>% filter(buffer.within=="5")
xy <- sf::st_coordinates(tramo.sacmex)
xy.sort = tramo.sacmex[order(xy[,"Y"], xy[,"Y"]),]
tramo.sacmex <- (xy.sort %>% sf::st_coordinates() %>% sf::st_linestring())

tramo.cchvallej <- l1.mbus.est %>% select(nombre, geometry, buffer.within) %>% filter(buffer.within=="2")
xy <- sf::st_coordinates(tramo.cchvallej)
xy.sort = tramo.cchvallej[order(xy[,"Y"], xy[,"Y"]),]
tramo.cchvallej <- (xy.sort %>% sf::st_coordinates() %>% sf::st_linestring())
###############################################################################################################

#Leer datos de AGOSTO
ago <- read.csv("D:/Escritorio/gtfs.mes/ago/ago.csv")

#Seleccionar solamente las columnas que me sirven 
ago <-  ago %>% select(TIMESTAMP, VEHICLE, POSITION, LONGITUDE, SPEED)

#Cambiar nombres de las variables
names(ago) <- c("timestamp", "vehicle", "y", "x", "speed")

#Formato a las variables. Comencemos por vehicle, ya que la de timestamp se queda igual. Revisar que las variables hayan quedado como quiero.
ago$vehicle <- substr(ago$vehicle, 4, 10)
ago$vehicle <- as.factor(ago$vehicle)
ago$y <- substr(ago$y, 10, 16)
ago$y <- as.numeric(ago$y)
ago$cst6cdt<- lubridate::as_datetime(ago$timestamp, tz="CST6CDT")
ago$day <- lubridate::day(ago$cst6cdt)
ago$hour <- lubridate::hour(ago$cst6cdt)

############ Eliminar aquellas observaciones con latitud y longitud 0########

ago <- ago %>% select(timestamp, vehicle, speed, x, y, cst6cdt, day, hour) %>% filter(y!="0")
ago <- ago %>% select(timestamp, vehicle, speed, x, y, cst6cdt, day, hour) %>% filter(x!="0")
ago <- ago %>% select(timestamp, vehicle, speed, x, y, cst6cdt, day, hour) %>% filter(speed!="0")

#############

ago <- sf::st_as_sf(ago, coords = c("x", "y"), crs = 4326, agr = "constant") #Convertir la base jul.jn en una con campo geométrico.
ago <- ago %>% sf::st_set_crs(4326) %>% sf::st_transform(32214)

###############################

st_buffer<-sf::st_buffer(tramo.picacho, 30)
st_within<- sf::st_within(ago, st_buffer, sparse = FALSE)
ago.picacho <- cbind(ago, st_within)
ago.picacho <- ago.picacho %>% select(timestamp, vehicle, cst6cdt, speed, day, hour, st_within, geometry) %>% filter(st_within=="TRUE")

ago.picacho.summarize <- ago.picacho %>% group_by(day, hour) %>%
  summarise(
    n = n(),
    mean = mean(speed),
    median = median(speed),
    max = max(speed),
    sd = sd(speed), 
    p75 = quantile(speed, 0.75)
  )    


ago.picacho.summarize$nom <- "PICACH" 
ago.picacho.summarize$hour <- stringr::str_pad(ago.picacho.summarize$hour, 2, side = "left", pad= "0")
ago.picacho.summarize$id <- paste0(ago.picacho.summarize$nom, ago.picacho.summarize$day, ago.picacho.summarize$hour)

#####################################################################################
st_buffer<-sf::st_buffer(tramo.dhuman, 30)
st_within<- sf::st_within(ago, st_buffer, sparse = FALSE)
ago.dhumanos <- cbind(ago, st_within)
ago.dhumanos <- ago.dhumanos %>% select(timestamp, vehicle, cst6cdt, speed, day, hour, st_within, geometry) %>% filter(st_within=="TRUE")

ago.dhumanos.summarize <- ago.dhumanos %>% group_by(day, hour) %>%
  summarise(
    n = n(),
    mean = mean(speed),
    median = median(speed),
    max = max(speed),
    sd = sd(speed), 
    p75 = quantile(speed, 0.75)
  )    


ago.dhumanos.summarize$nom <- "DHUMAN" 
ago.dhumanos.summarize$hour <- stringr::str_pad(ago.dhumanos.summarize$hour, 2, side = "left", pad= "0")
ago.dhumanos.summarize$id <- paste0(ago.dhumanos.summarize$nom, ago.dhumanos.summarize$day, ago.dhumanos.summarize$hour)


#############################################################################33


st_buffer<-sf::st_buffer(tramo.zapata, 30)
st_within<- sf::st_within(ago, st_buffer, sparse = FALSE)
ago.zapata <- cbind(ago, st_within)
ago.zapata <- ago.zapata %>% select(timestamp, vehicle, cst6cdt, speed, day, hour, st_within, geometry) %>% filter(st_within=="TRUE")


ago.zapata.summarize <- ago.zapata %>% group_by(day, hour) %>%
  summarise(
    n = n(),
    mean = mean(speed),
    median = median(speed),
    max = max(speed),
    sd = sd(speed), 
    p75 = quantile(speed, 0.75)
  )    


ago.zapata.summarize$nom <- "ZAPATA" 
ago.zapata.summarize$hour <- stringr::str_pad(ago.zapata.summarize$hour, 2, side = "left", pad= "0")
ago.zapata.summarize$id <- paste0(ago.zapata.summarize$nom, ago.zapata.summarize$day, ago.zapata.summarize$hour)



#######################################################################################



st_buffer<-sf::st_buffer(tramo.sacmex, 30)
st_within<- sf::st_within(ago, st_buffer, sparse = FALSE)
ago.sacmex <- cbind(ago, st_within)
ago.sacmex <- ago.sacmex %>% select(timestamp, vehicle, cst6cdt, speed, day, hour, st_within, geometry) %>% filter(st_within=="TRUE")


ago.sacmex.summarize <- ago.sacmex %>% group_by(day, hour) %>%
  summarise(
    n = n(),
    mean = mean(speed),
    median = median(speed),
    max = max(speed),
    sd = sd(speed), 
    p75 = quantile(speed, 0.75)
  )    


ago.sacmex.summarize$nom <- "SACMEX" 
ago.sacmex.summarize$hour <- stringr::str_pad(ago.sacmex.summarize$hour, 2, side = "left", pad= "0")
ago.sacmex.summarize$id <- paste0(ago.sacmex.summarize$nom, ago.sacmex.summarize$day, ago.sacmex.summarize$hour)



###############################################################################






st_buffer<-sf::st_buffer(tramo.cchvallej, 30)
st_within<- sf::st_within(ago, st_buffer, sparse = FALSE)
ago.cchvallej <- cbind(ago, st_within)
ago.cchvallej <- ago.cchvallej %>% select(timestamp, vehicle, cst6cdt, speed, day, hour, st_within, geometry) %>% filter(st_within=="TRUE")


ago.cchvallej.summarize <- ago.cchvallej %>% group_by(day, hour) %>%
  summarise(
    n = n(),
    mean = mean(speed),
    median = median(speed),
    max = max(speed),
    sd = sd(speed), 
    p75 = quantile(speed, 0.75)
  )    


ago.cchvallej.summarize$nom <- "CCHVAL" 
ago.cchvallej.summarize$hour <- stringr::str_pad(ago.cchvallej.summarize$hour, 2, side = "left", pad= "0")
ago.cchvallej.summarize$id <- paste0(ago.cchvallej.summarize$nom, ago.cchvallej.summarize$day, ago.cchvallej.summarize$hour)

############################################33
ago.summarize.rbind<- rbind(ago.picacho.summarize, ago.dhumanos.summarize, ago.zapata.summarize, ago.sacmex.summarize, ago.cchvallej.summarize)
ago.summarize.rbind$geometry <- NULL
ago.rbind <- ago.summarize.rbind %>% select(id, nom, day, hour, n, mean, median, max, sd, p75)

write.csv(ago.rbind, "D:/Escritorio/gtfs.mes/ago/ago.rbind.csv")

##########################################################################

ago.rbind<- read.csv("D:/Escritorio/gtfs.mes/ago/ago.rbind.csv")


ago.rbind$year <- 2020
ago.rbind$month <- 08

ago.rbind$month <- stringr::str_pad(ago.rbind$month, width = 2, side = "left", pad = "0")
ago.rbind$day <- stringr::str_pad(ago.rbind$day, width = 2, side = "left", pad = "0")


ago.rbind$date <- paste(ago.rbind$year,ago.rbind$month, ago.rbind$day, sep = "-")
ago.rbind$date <- as.POSIXct(ago.rbind$date)
head(ago.rbind$date)

ago.rbind$hour <- stringr::str_pad(ago.rbind$hour, width = 2, side = "left", pad = "0")
ago.rbind$hour <- stringr::str_pad(ago.rbind$hour, width = 3, side = "right", pad = ":")
ago.rbind$hour <- stringr::str_pad(ago.rbind$hour, width = 4, side = "right", pad = "0")
ago.rbind$hour <- stringr::str_pad(ago.rbind$hour, width = 5, side = "right", pad = "0")

ago.rbind$ts<- as.POSIXct(paste(ago.rbind$date, ago.rbind$hour), "CST6CDT")

####################################################################################################################3
picacho <- ago.rbind %>% select(id, nom, day, hour, n, mean, median, max, sd, p75, year, month, date, ts) %>% filter(nom=="PICACH")

## First make up some mock data
## Make a data frame with a full series of dates from the min date to the max date
## in the incomplete data frame
full_dates <- seq(min(picacho$ts), max(picacho$ts), 
                  by = "1 hour")
full_dates <- data.frame(date = full_dates)

## Merge the complete data frame with the incomplete to fill in the dates and add 
## NAs for missing values
my_complete_data <- merge(full_dates, picacho, by = "date", 
                          all.x = TRUE)
