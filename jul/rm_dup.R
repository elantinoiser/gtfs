#http://postgis.net/workshops/postgis-intro/index.html

buffer<- sf::read_sf("D:/Escritorio/estaciones.ohiiunam/buffer.shp")
#buffer <- sf::st_wrap_dateline(sf::st_sfc(sf::st_multipoint(rbind(c(-99.2023, 19.2633), c(-99.2157, 19.3159), c(-99.1462, 19.3685), c(-99.1373, 19.4231), c(-99.1397, 19.4817))), crs = 4326)) 
#buffer <- buffer %>% sf::st_set_crs(4326) %>% sf::st_transform(32214)

buffer.5k <-sf::st_buffer(buffer, 5000)

l1.mbus.est <- sf::read_sf("D:/Escritorio/gtfs_estatico/l1.mbus.est03.shp")
l1.mbus.est <- l1.mbus.est %>% sf::st_set_crs(4326) %>% sf::st_transform(32214)

#buffer.contains <- sf::st_contains(buffer.5k, l1.mbus.est, sparse = TRUE)

buffer.within <- sf::st_within(l1.mbus.est, buffer.5k, sparse = TRUE)
buffer.within<- as.data.frame(buffer.within)
buffer.within<- buffer.within[!duplicated(buffer.within[,1]), ]

buffer.within<-buffer.within[,2]
l1.mbus.est <- as.data.frame(l1.mbus.est)
l1.mbus.est<- cbind(l1.mbus.est, buffer.within)

l1.mbus.est<- sf::st_as_sf(l1.mbus.est, coords = c("lon", "lat"), crs = 4326)
l1.mbus.est <- l1.mbus.est %>% sf::st_set_crs(4326) %>% sf::st_transform(32214)



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

###############################

st_buffer<-sf::st_buffer(tramo.picacho, 30)
st_within<- sf::st_within(jul.jn, st_buffer, sparse = FALSE)
jul.jn.picacho <- cbind(jul.jn, st_within)
jul.jn.picacho <- jul.jn.picacho %>% select(timestamp, vehicle, cst6cdt, speed, day, hour, st_within, geometry) %>% filter(st_within=="TRUE")
#write.csv(jul.jn.picacho, "D:/Escritorio/gtfs.mes/jul/jul.picacho.csv")
#jul.jn.picacho.mean<- aggregate(jul.jn.picacho$speed, list(jul.jn.picacho$day, jul.jn.picacho$hour), FUN=mean)
#jul.jn.picacho.median<- aggregate(jul.jn.picacho$speed, list(jul.jn.picacho$day, jul.jn.picacho$hour), FUN=median)
#jul.jn.picacho.p75<- aggregate(jul.jn.picacho$speed, list(jul.jn.picacho$day, jul.jn.picacho$hour), FUN= function(x) quantile(x, probs = 0.75))
#jul.jn.picacho.summary<- aggregate(jul.jn.picacho$speed, list(jul.jn.picacho$day, jul.jn.picacho$hour), FUN= function(x) summary(x))

# Otra forma de hacer lo mismo

        #jul.jn.picacho %>%
        #group_by(day, hour) %>%
        #summarise(mean = mean(speed))
 

jul.jn.picacho.summarize <- jul.jn.picacho %>% group_by(day, hour) %>%
          summarise(
            n = n(),
            mean = mean(speed),
            median = median(speed),
            max = max(speed),
            sd = sd(speed), 
            p75 = quantile(speed, 0.75)
          )    


jul.jn.picacho.summarize$nom <- "PICACH" 
jul.jn.picacho.summarize$hour <- stringr::str_pad(jul.jn.picacho.summarize$hour, 2, side = "left", pad= "0")
jul.jn.picacho.summarize$id <- paste0(jul.jn.picacho.summarize$nom, jul.jn.picacho.summarize$day, jul.jn.picacho.summarize$hour)

#write.csv(jul.jn.picacho.mean, "D:/Escritorio/gtfs.mes/jul/jul.picacho.mean.csv")
#jul.jn.picacho.max<- aggregate(jul.jn.picacho$speed, list(jul.jn.picacho$day, jul.jn.picacho$hour), FUN=max)
#jul.jn.picacho.max$nom <- "PICACH" 
#jul.jn.picacho.max$Group.2 <- stringr::str_pad(jul.jn.picacho.max$Group.2, 2, side = "left", pad= "0")
#jul.jn.picacho.max$id <- paste0(jul.jn.picacho.max$nom, jul.jn.picacho.max$Group.1, jul.jn.picacho.max$Group.2)
#write.csv(jul.jn.picacho.max, "D:/Escritorio/gtfs.mes/jul/jul.picacho.max.csv")

#####################################################################################
st_buffer<-sf::st_buffer(tramo.dhuman, 30)
st_within<- sf::st_within(jul.jn, st_buffer, sparse = FALSE)
jul.jn.dhumanos <- cbind(jul.jn, st_within)
jul.jn.dhumanos <- jul.jn.dhumanos %>% select(timestamp, vehicle, cst6cdt, speed, day, hour, st_within, geometry) %>% filter(st_within=="TRUE")
#write.csv(jul.jn.dhuman, "D:/Escritorio/gtfs.mes/jul/jul.dhuman.csv")
#jul.jn.dhuman.mean<- aggregate(jul.jn.dhumanos$speed, list(jul.jn.dhumanos$day, jul.jn.dhumanos$hour), FUN=mean)
#write.csv(jul.jn.dhuman.mean, "D:/Escritorio/gtfs.mes/jul/jul.dhuman.mean.csv")
#jul.jn.dhuman.max<- aggregate(jul.jn.dhuman$speed, list(jul.jn.dhuman$day, jul.jn.dhuman$hour), FUN=max)
#jul.jn.dhuman.max$nom <- "DHUMAN" 
#jul.jn.dhuman.max$Group.2 <- stringr::str_pad(jul.jn.dhuman.max$Group.2, 2, side = "left", pad= "0")
#jul.jn.dhuman.max$id <- paste0(jul.jn.dhuman.max$nom, jul.jn.dhuman.max$Group.1, jul.jn.dhuman.max$Group.2)
#write.csv(jul.jn.dhuman.max, "D:/Escritorio/gtfs.mes/jul/jul.dhuman.max.csv")

jul.jn.dhumanos.summarize <- jul.jn.dhumanos %>% group_by(day, hour) %>%
        summarise(
                n = n(),
                mean = mean(speed),
                median = median(speed),
                max = max(speed),
                sd = sd(speed), 
                p75 = quantile(speed, 0.75)
        )    


jul.jn.dhumanos.summarize$nom <- "DHUMAN" 
jul.jn.dhumanos.summarize$hour <- stringr::str_pad(jul.jn.dhumanos.summarize$hour, 2, side = "left", pad= "0")
jul.jn.dhumanos.summarize$id <- paste0(jul.jn.dhumanos.summarize$nom, jul.jn.dhumanos.summarize$day, jul.jn.dhumanos.summarize$hour)


#############################################################################33


st_buffer<-sf::st_buffer(tramo.zapata, 30)
st_within<- sf::st_within(jul.jn, st_buffer, sparse = FALSE)
jul.jn.zapata <- cbind(jul.jn, st_within)
jul.jn.zapata <- jul.jn.zapata %>% select(timestamp, vehicle, cst6cdt, speed, day, hour, st_within, geometry) %>% filter(st_within=="TRUE")
#write.csv(jul.jn.zapata, "D:/Escritorio/gtfs.mes/jul/jul.zapata.csv")
#jul.jn.zapata.mean<- aggregate(jul.jn.zapata$speed, list(jul.jn.zapata$day, jul.jn.zapata$hour), FUN=mean)
#write.csv(jul.jn.zapata.mean, "D:/Escritorio/gtfs.mes/jul/jul.zapata.mean.csv")
#jul.jn.zapata.max<- aggregate(jul.jn.zapata$speed, list(jul.jn.zapata$day, jul.jn.zapata$hour), FUN=max)
#jul.jn.zapata.max$nom <- "ZAPATA" 
#jul.jn.zapata.max$Group.2 <- stringr::str_pad(jul.jn.zapata.max$Group.2, 2, side = "left", pad= "0")
#jul.jn.zapata.max$id <- paste0(jul.jn.zapata.max$nom, jul.jn.zapata.max$Group.1, jul.jn.zapata.max$Group.2)
#write.csv(jul.jn.zapata.max, "D:/Escritorio/gtfs.mes/jul/jul.zapata.max.csv")


jul.jn.zapata.summarize <- jul.jn.zapata %>% group_by(day, hour) %>%
        summarise(
                n = n(),
                mean = mean(speed),
                median = median(speed),
                max = max(speed),
                sd = sd(speed), 
                p75 = quantile(speed, 0.75)
        )    


jul.jn.zapata.summarize$nom <- "ZAPATA" 
jul.jn.zapata.summarize$hour <- stringr::str_pad(jul.jn.zapata.summarize$hour, 2, side = "left", pad= "0")
jul.jn.zapata.summarize$id <- paste0(jul.jn.zapata.summarize$nom, jul.jn.zapata.summarize$day, jul.jn.zapata.summarize$hour)



#######################################################################################



st_buffer<-sf::st_buffer(tramo.sacmex, 30)
st_within<- sf::st_within(jul.jn, st_buffer, sparse = FALSE)
jul.jn.sacmex <- cbind(jul.jn, st_within)
jul.jn.sacmex <- jul.jn.sacmex %>% select(timestamp, vehicle, cst6cdt, speed, day, hour, st_within, geometry) %>% filter(st_within=="TRUE")
#write.csv(jul.jn.sacmex, "D:/Escritorio/gtfs.mes/jul/jul.sacmex.csv")
#jul.jn.sacmex.mean<- aggregate(jul.jn.sacmex$speed, list(jul.jn.sacmex$day, jul.jn.sacmex$hour), FUN=mean)
#write.csv(jul.jn.sacmex.mean, "D:/Escritorio/gtfs.mes/jul/jul.sacmex.mean.csv")
#jul.jn.sacmex.max<- aggregate(jul.jn.sacmex$speed, list(jul.jn.sacmex$day, jul.jn.sacmex$hour), FUN=max)
#jul.jn.sacmex.max$nom <- "SACMEX" 
#jul.jn.sacmex.max$Group.2 <- stringr::str_pad(jul.jn.sacmex.max$Group.2, 2, side = "left", pad= "0")
#jul.jn.sacmex.max$id <- paste0(jul.jn.sacmex.max$nom, jul.jn.sacmex.max$Group.1, jul.jn.sacmex.max$Group.2)
#write.csv(jul.jn.sacmex.max, "D:/Escritorio/gtfs.mes/jul/jul.sacmex.max.csv")


jul.jn.sacmex.summarize <- jul.jn.sacmex %>% group_by(day, hour) %>%
        summarise(
                n = n(),
                mean = mean(speed),
                median = median(speed),
                max = max(speed),
                sd = sd(speed), 
                p75 = quantile(speed, 0.75)
        )    


jul.jn.sacmex.summarize$nom <- "SACMEX" 
jul.jn.sacmex.summarize$hour <- stringr::str_pad(jul.jn.sacmex.summarize$hour, 2, side = "left", pad= "0")
jul.jn.sacmex.summarize$id <- paste0(jul.jn.sacmex.summarize$nom, jul.jn.sacmex.summarize$day, jul.jn.sacmex.summarize$hour)



###############################################################################






st_buffer<-sf::st_buffer(tramo.cchvallej, 30)
st_within<- sf::st_within(jul.jn, st_buffer, sparse = FALSE)
jul.jn.cchvallej <- cbind(jul.jn, st_within)
jul.jn.cchvallej <- jul.jn.cchvallej %>% select(timestamp, vehicle, cst6cdt, speed, day, hour, st_within, geometry) %>% filter(st_within=="TRUE")
#write.csv(jul.jn.cchvallej, "D:/Escritorio/gtfs.mes/jul/jul.cchvallej.csv")
#jul.jn.cchvallej.mean<- aggregate(jul.jn.cchvallej$speed, list(jul.jn.cchvallej$day, jul.jn.cchvallej$hour), FUN=mean)
#write.csv(jul.jn.cchvallej.mean, "D:/Escritorio/gtfs.mes/jul/jul.cchvallej.mean.csv")
#jul.jn.cchvallej.max<- aggregate(jul.jn.cchvallej$speed, list(jul.jn.cchvallej$day, jul.jn.cchvallej$hour), FUN=max)
#jul.jn.cchvallej.max$nom <- "CCHVAL" 
#jul.jn.cchvallej.max$Group.2 <- stringr::str_pad(jul.jn.cchvallej.max$Group.2, 2, side = "left", pad= "0")
#jul.jn.cchvallej.max$id <- paste0(jul.jn.cchvallej.max$nom, jul.jn.cchvallej.max$Group.1, jul.jn.cchvallej.max$Group.2)
#write.csv(jul.jn.cchvallej.max, "D:/Escritorio/gtfs.mes/jul/jul.cchvallej.max.csv")


jul.jn.cchvallej.summarize <- jul.jn.cchvallej %>% group_by(day, hour) %>%
        summarise(
                n = n(),
                mean = mean(speed),
                median = median(speed),
                max = max(speed),
                sd = sd(speed), 
                p75 = quantile(speed, 0.75)
        )    


jul.jn.cchvallej.summarize$nom <- "CCHVAL" 
jul.jn.cchvallej.summarize$hour <- stringr::str_pad(jul.jn.cchvallej.summarize$hour, 2, side = "left", pad= "0")
jul.jn.cchvallej.summarize$id <- paste0(jul.jn.cchvallej.summarize$nom, jul.jn.cchvallej.summarize$day, jul.jn.cchvallej.summarize$hour)


jul.jn.summarize.rbind<- rbind(jul.jn.picacho.summarize, jul.jn.dhumanos.summarize, jul.jn.zapata.summarize, jul.jn.sacmex.summarize, jul.jn.cchvallej.summarize)
jul.jn.summarize.rbind$geometry <- NULL
jul.jn.rbind <- jul.jn.summarize.rbind %>% select(id, nom, day, hour, n, mean, median, max, sd, p75)

#############################################################################################
jul.jn.rbind <- read.csv("D:/Escritorio/gtfs.mes/jul/jul.jn.rbind.csv")

jul.jn.rbind$year <- 2020
jul.jn.rbind$month <- 07

jul.jn.rbind$month <- stringr::str_pad(jul.jn.rbind$month, width = 2, side = "left", pad = "0")
jul.jn.rbind$day <- stringr::str_pad(jul.jn.rbind$day, width = 2, side = "left", pad = "0")


jul.jn.rbind$date <- paste(jul.jn.rbind$year,jul.jn.rbind$month, jul.jn.rbind$day, sep = "-")
jul.jn.rbind$date <- as.POSIXct(jul.jn.rbind$date)
head(jul.jn.rbind$date)

jul.jn.rbind$hour <- stringr::str_pad(jul.jn.rbind$hour, width = 2, side = "left", pad = "0")
jul.jn.rbind$hour <- stringr::str_pad(jul.jn.rbind$hour, width = 3, side = "right", pad = ":")
jul.jn.rbind$hour <- stringr::str_pad(jul.jn.rbind$hour, width = 4, side = "right", pad = "0")
jul.jn.rbind$hour <- stringr::str_pad(jul.jn.rbind$hour, width = 5, side = "right", pad = "0")

jul.jn.rbind$ts<- as.POSIXct(paste(jul.jn.rbind$date, jul.jn.rbind$hour), "CST6CDT")

write.csv(jul.jn.rbind, "D:/Escritorio/gtfs.mes/jul/jul.jn.rbind.csv")


####################################################################################################################3
picacho <- jul.jn.rbind %>% select(id, nom, day, hour, n, mean, median, max, sd, p75, year, month, date, ts) %>% filter(nom=="PICACH")

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

my_complete_data_picacho <- my_complete_data

#######################

dhuman <- jul.jn.rbind %>% select(id, nom, day, hour, n, mean, median, max, sd, p75, year, month, date, ts) %>% filter(nom=="DHUMAN")

## First make up some mock data
## Make a data frame with a full series of dates from the min date to the max date
## in the incomplete data frame
full_dates <- seq(min(dhuman$ts), max(dhuman$ts), 
                  by = "1 hour")
full_dates <- data.frame(date = full_dates)

## Merge the complete data frame with the incomplete to fill in the dates and add 
## NAs for missing values
my_complete_data <- merge(full_dates, dhuman, by = "date", 
                          all.x = TRUE)

my_complete_data_dhuman <- my_complete_data

#####################################################

zapata <- jul.jn.rbind %>% select(id, nom, day, hour, n, mean, median, max, sd, p75, year, month, date, ts) %>% filter(nom=="ZAPATA")

## First make up some mock data
## Make a data frame with a full series of dates from the min date to the max date
## in the incomplete data frame
full_dates <- seq(min(zapata$ts), max(zapata$ts), 
                  by = "1 hour")
full_dates <- data.frame(date = full_dates)

## Merge the complete data frame with the incomplete to fill in the dates and add 
## NAs for missing values
my_complete_data <- merge(full_dates, zapata, by = "date", 
                          all.x = TRUE)

my_complete_data_zapata <- my_complete_data
#################################################
sacmex <- jul.jn.rbind %>% select(id, nom, day, hour, n, mean, median, max, sd, p75, year, month, date, ts) %>% filter(nom=="SACMEX")

## First make up some mock data
## Make a data frame with a full series of dates from the min date to the max date
## in the incomplete data frame
full_dates <- seq(min(sacmex$ts), max(sacmex$ts), 
                  by = "1 hour")
full_dates <- data.frame(date = full_dates)

## Merge the complete data frame with the incomplete to fill in the dates and add 
## NAs for missing values
my_complete_data <- merge(full_dates, sacmex, by = "date", 
                          all.x = TRUE)

my_complete_data_sacmex <- my_complete_data

####################################################################################

cchval <- jul.jn.rbind %>% select(id, nom, day, hour, n, mean, median, max, sd, p75, year, month, date, ts) %>% filter(nom=="CCHVAL")

## First make up some mock data
## Make a data frame with a full series of dates from the min date to the max date
## in the incomplete data frame
full_dates <- seq(min(cchval$ts), max(cchval$ts), 
                  by = "1 hour")
full_dates <- data.frame(date = full_dates)

## Merge the complete data frame with the incomplete to fill in the dates and add 
## NAs for missing values
my_complete_data <- merge(full_dates, cchval, by = "date", 
                          all.x = TRUE)

my_complete_data_cchval <- my_complete_data

###############################################################3

jul.jn.mcd <- rbind(my_complete_data_picacho,my_complete_data_dhuman, my_complete_data_dhuman, my_complete_data_sacmex, my_complete_data_cchval)

jul.jn.mcd$dow<- weekdays(as.Date(jul.jn.mcd$date))

write.csv(jul.jn.mcd, "D:/Escritorio/gtfs.mes/jul/jul.jn.mcd.csv")

plot.ts()




