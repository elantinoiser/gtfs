

###     SEIS

sep01 <- read.csv("C:/Users/rdelatorre/Desktop/gtfs.metrobus/sep/sep11.csv")
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






#Check. Se usan estas funciones para obtener los camiones que particpan en las rutas diarias

jul.fun <- function(i) {
  sep01 %>% select(timestamp, vehicle, speed, date, hour, x, y, route) %>% filter(vehicle==i)
}

for (i in sep01$vehicle) {
  assign (paste0("vehi", sep=".", i), data.frame(jul.fun(i)))
}

#Check.Agregar las trayectorias a una lista. Para la verificación deben coincidir el número de elementos de la lista con el número de valores únicos de la variable vehicle. 

lista <- mget(ls(pattern = "vehi.")) # con este comando agrega los dataframes con un patrón a la lista
unique(sep01$vehicle)

#Check. Teniendo las trayectorias en una lista.

lista <- lapply(lista, function(x) mutate(x, y.lead=lead(y, n=1)))
lista <- lapply(lista, function(x) mutate(x, x.lead=lead(x, n=1)))
lista <- lapply(lista, function (j) mutate(j, dist = TrackReconstruction::CalcDistance(j$y, j$x, j$y.lead, j$x.lead)))#Da la distancia en kilómetros

suma <- as.data.frame(sapply(lista, function(x) sum(x$dist, na.rm = TRUE)))
rango <- sapply(lista, function(x) range(x$timestamp, na.rm = TRUE)) 
rango_t <- data.frame(t(rango))
dura<- as.vector(lubridate::as_datetime(rango_t$X2) - lubridate::as_datetime(rango_t$X1))
dura <- dura/60/60
velo <- suma/dura

boxplot(velo) #Mediana de 9.7 a lo largo de todo el día #Media de 9.49
media <- mean(velo$`sapply(lista, function(x) sum(x$dist, na.rm = TRUE))`, na.rm = TRUE)
length<- as.data.frame(length(lista))
media <- cbind(media, length)
write.csv(media, "C:/Users/rdelatorre/Desktop/6.csv")
rm(list=ls())

#Filtrar por hora desde arriba


#SIETE




sep01 <- read.csv("C:/Users/rdelatorre/Desktop/gtfs.metrobus/sep/sep11.csv")
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
sep01 <- sep01 %>% select(timestamp, vehicle, speed, date, hour, x, y, route) %>% filter(hour=="7")


boxplot(sep01$speed) #Mediana de 6 a lo largo de todo el día

#Check. Se usan estas funciones para obtener los camiones que particpan en las rutas diarias

jul.fun <- function(i) {
  sep01 %>% select(timestamp, vehicle, speed, date, hour, x, y, route) %>% filter(vehicle==i)
}

for (i in sep01$vehicle) {
  assign (paste0("vehi", sep=".", i), data.frame(jul.fun(i)))
}

#Check.Agregar las trayectorias a una lista. Para la verificación deben coincidir el número de elementos de la lista con el número de valores únicos de la variable vehicle. 

lista <- mget(ls(pattern = "vehi.")) # con este comando agrega los dataframes con un patrón a la lista
unique(sep01$vehicle)

#Check. Teniendo las trayectorias en una lista.

lista <- lapply(lista, function(x) mutate(x, y.lead=lead(y, n=1)))
lista <- lapply(lista, function(x) mutate(x, x.lead=lead(x, n=1)))
lista <- lapply(lista, function (j) mutate(j, dist = TrackReconstruction::CalcDistance(j$y, j$x, j$y.lead, j$x.lead)))#Da la distancia en kilómetros

suma <- as.data.frame(sapply(lista, function(x) sum(x$dist, na.rm = TRUE)))
rango <- sapply(lista, function(x) range(x$timestamp, na.rm = TRUE)) 
rango_t <- data.frame(t(rango))
dura<- as.vector(lubridate::as_datetime(rango_t$X2) - lubridate::as_datetime(rango_t$X1))
dura <- dura/60/60
velo <- suma/dura

boxplot(velo) #Mediana de 9.7 a lo largo de todo el día #Media de 9.49
media <- mean(velo$`sapply(lista, function(x) sum(x$dist, na.rm = TRUE))`, na.rm = TRUE)
length<- as.data.frame(length(lista))
media <- cbind(media, length)
write.csv(media, "C:/Users/rdelatorre/Desktop/7.csv")
rm(list=ls())


#########   OCHO




sep01 <- read.csv("C:/Users/rdelatorre/Desktop/gtfs.metrobus/sep/sep11.csv")
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
sep01 <- sep01 %>% select(timestamp, vehicle, speed, date, hour, x, y, route) %>% filter(hour=="8")


boxplot(sep01$speed) #Mediana de 6 a lo largo de todo el día

#Check. Se usan estas funciones para obtener los camiones que particpan en las rutas diarias

jul.fun <- function(i) {
  sep01 %>% select(timestamp, vehicle, speed, date, hour, x, y, route) %>% filter(vehicle==i)
}

for (i in sep01$vehicle) {
  assign (paste0("vehi", sep=".", i), data.frame(jul.fun(i)))
}

#Check.Agregar las trayectorias a una lista. Para la verificación deben coincidir el número de elementos de la lista con el número de valores únicos de la variable vehicle. 

lista <- mget(ls(pattern = "vehi.")) # con este comando agrega los dataframes con un patrón a la lista
unique(sep01$vehicle)

#Check. Teniendo las trayectorias en una lista.

lista <- lapply(lista, function(x) mutate(x, y.lead=lead(y, n=1)))
lista <- lapply(lista, function(x) mutate(x, x.lead=lead(x, n=1)))
lista <- lapply(lista, function (j) mutate(j, dist = TrackReconstruction::CalcDistance(j$y, j$x, j$y.lead, j$x.lead)))#Da la distancia en kilómetros

suma <- as.data.frame(sapply(lista, function(x) sum(x$dist, na.rm = TRUE)))
rango <- sapply(lista, function(x) range(x$timestamp, na.rm = TRUE)) 
rango_t <- data.frame(t(rango))
dura<- as.vector(lubridate::as_datetime(rango_t$X2) - lubridate::as_datetime(rango_t$X1))
dura <- dura/60/60
velo <- suma/dura

boxplot(velo) #Mediana de 9.7 a lo largo de todo el día #Media de 9.49
media <- mean(velo$`sapply(lista, function(x) sum(x$dist, na.rm = TRUE))`, na.rm = TRUE)
length<- as.data.frame(length(lista))
media <- cbind(media, length)
write.csv(media, "C:/Users/rdelatorre/Desktop/8.csv")
rm(list=ls())


######NUEVE



sep01 <- read.csv("C:/Users/rdelatorre/Desktop/gtfs.metrobus/sep/sep11.csv")
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
sep01 <- sep01 %>% select(timestamp, vehicle, speed, date, hour, x, y, route) %>% filter(hour=="9")


boxplot(sep01$speed) #Mediana de 6 a lo largo de todo el día

#Check. Se usan estas funciones para obtener los camiones que particpan en las rutas diarias

jul.fun <- function(i) {
  sep01 %>% select(timestamp, vehicle, speed, date, hour, x, y, route) %>% filter(vehicle==i)
}

for (i in sep01$vehicle) {
  assign (paste0("vehi", sep=".", i), data.frame(jul.fun(i)))
}

#Check.Agregar las trayectorias a una lista. Para la verificación deben coincidir el número de elementos de la lista con el número de valores únicos de la variable vehicle. 

lista <- mget(ls(pattern = "vehi.")) # con este comando agrega los dataframes con un patrón a la lista
unique(sep01$vehicle)

#Check. Teniendo las trayectorias en una lista.

lista <- lapply(lista, function(x) mutate(x, y.lead=lead(y, n=1)))
lista <- lapply(lista, function(x) mutate(x, x.lead=lead(x, n=1)))
lista <- lapply(lista, function (j) mutate(j, dist = TrackReconstruction::CalcDistance(j$y, j$x, j$y.lead, j$x.lead)))#Da la distancia en kilómetros

suma <- as.data.frame(sapply(lista, function(x) sum(x$dist, na.rm = TRUE)))
rango <- sapply(lista, function(x) range(x$timestamp, na.rm = TRUE)) 
rango_t <- data.frame(t(rango))
dura<- as.vector(lubridate::as_datetime(rango_t$X2) - lubridate::as_datetime(rango_t$X1))
dura <- dura/60/60
velo <- suma/dura

boxplot(velo) #Mediana de 9.7 a lo largo de todo el día #Media de 9.49
media <- mean(velo$`sapply(lista, function(x) sum(x$dist, na.rm = TRUE))`, na.rm = TRUE)
length<- as.data.frame(length(lista))
media <- cbind(media, length)
write.csv(media, "C:/Users/rdelatorre/Desktop/9.csv")
rm(list=ls())

#Filtrar por hora desde arriba

########               DIEZ


sep01 <- read.csv("C:/Users/rdelatorre/Desktop/gtfs.metrobus/sep/sep11.csv")
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
sep01 <- sep01 %>% select(timestamp, vehicle, speed, date, hour, x, y, route) %>% filter(hour=="10")


boxplot(sep01$speed) #Mediana de 6 a lo largo de todo el día

#Check. Se usan estas funciones para obtener los camiones que particpan en las rutas diarias

jul.fun <- function(i) {
  sep01 %>% select(timestamp, vehicle, speed, date, hour, x, y, route) %>% filter(vehicle==i)
}

for (i in sep01$vehicle) {
  assign (paste0("vehi", sep=".", i), data.frame(jul.fun(i)))
}

#Check.Agregar las trayectorias a una lista. Para la verificación deben coincidir el número de elementos de la lista con el número de valores únicos de la variable vehicle. 

lista <- mget(ls(pattern = "vehi.")) # con este comando agrega los dataframes con un patrón a la lista
unique(sep01$vehicle)

#Check. Teniendo las trayectorias en una lista.

lista <- lapply(lista, function(x) mutate(x, y.lead=lead(y, n=1)))
lista <- lapply(lista, function(x) mutate(x, x.lead=lead(x, n=1)))
lista <- lapply(lista, function (j) mutate(j, dist = TrackReconstruction::CalcDistance(j$y, j$x, j$y.lead, j$x.lead)))#Da la distancia en kilómetros

suma <- as.data.frame(sapply(lista, function(x) sum(x$dist, na.rm = TRUE)))
rango <- sapply(lista, function(x) range(x$timestamp, na.rm = TRUE)) 
rango_t <- data.frame(t(rango))
dura<- as.vector(lubridate::as_datetime(rango_t$X2) - lubridate::as_datetime(rango_t$X1))
dura <- dura/60/60
velo <- suma/dura

boxplot(velo) #Mediana de 9.7 a lo largo de todo el día #Media de 9.49
media <- mean(velo$`sapply(lista, function(x) sum(x$dist, na.rm = TRUE))`, na.rm = TRUE)
length<- as.data.frame(length(lista))
media <- cbind(media, length)
write.csv(media, "C:/Users/rdelatorre/Desktop/10.csv")
rm(list=ls())

### ONCE



sep01 <- read.csv("C:/Users/rdelatorre/Desktop/gtfs.metrobus/sep/sep11.csv")
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
sep01 <- sep01 %>% select(timestamp, vehicle, speed, date, hour, x, y, route) %>% filter(hour=="11")


boxplot(sep01$speed) #Mediana de 6 a lo largo de todo el día

#Check. Se usan estas funciones para obtener los camiones que particpan en las rutas diarias

jul.fun <- function(i) {
  sep01 %>% select(timestamp, vehicle, speed, date, hour, x, y, route) %>% filter(vehicle==i)
}

for (i in sep01$vehicle) {
  assign (paste0("vehi", sep=".", i), data.frame(jul.fun(i)))
}

#Check.Agregar las trayectorias a una lista. Para la verificación deben coincidir el número de elementos de la lista con el número de valores únicos de la variable vehicle. 

lista <- mget(ls(pattern = "vehi.")) # con este comando agrega los dataframes con un patrón a la lista
unique(sep01$vehicle)

#Check. Teniendo las trayectorias en una lista.

lista <- lapply(lista, function(x) mutate(x, y.lead=lead(y, n=1)))
lista <- lapply(lista, function(x) mutate(x, x.lead=lead(x, n=1)))
lista <- lapply(lista, function (j) mutate(j, dist = TrackReconstruction::CalcDistance(j$y, j$x, j$y.lead, j$x.lead)))#Da la distancia en kilómetros

suma <- as.data.frame(sapply(lista, function(x) sum(x$dist, na.rm = TRUE)))
rango <- sapply(lista, function(x) range(x$timestamp, na.rm = TRUE)) 
rango_t <- data.frame(t(rango))
dura<- as.vector(lubridate::as_datetime(rango_t$X2) - lubridate::as_datetime(rango_t$X1))
dura <- dura/60/60
velo <- suma/dura

boxplot(velo) #Mediana de 9.7 a lo largo de todo el día #Media de 9.49
media <- mean(velo$`sapply(lista, function(x) sum(x$dist, na.rm = TRUE))`, na.rm = TRUE)
length<- as.data.frame(length(lista))
media <- cbind(media, length)
write.csv(media, "C:/Users/rdelatorre/Desktop/11.csv")
rm(list=ls())


### DOCE



sep01 <- read.csv("C:/Users/rdelatorre/Desktop/gtfs.metrobus/sep/sep11.csv")
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
sep01 <- sep01 %>% select(timestamp, vehicle, speed, date, hour, x, y, route) %>% filter(hour=="12")


boxplot(sep01$speed) #Mediana de 6 a lo largo de todo el día

#Check. Se usan estas funciones para obtener los camiones que particpan en las rutas diarias

jul.fun <- function(i) {
  sep01 %>% select(timestamp, vehicle, speed, date, hour, x, y, route) %>% filter(vehicle==i)
}

for (i in sep01$vehicle) {
  assign (paste0("vehi", sep=".", i), data.frame(jul.fun(i)))
}

#Check.Agregar las trayectorias a una lista. Para la verificación deben coincidir el número de elementos de la lista con el número de valores únicos de la variable vehicle. 

lista <- mget(ls(pattern = "vehi.")) # con este comando agrega los dataframes con un patrón a la lista
unique(sep01$vehicle)

#Check. Teniendo las trayectorias en una lista.

lista <- lapply(lista, function(x) mutate(x, y.lead=lead(y, n=1)))
lista <- lapply(lista, function(x) mutate(x, x.lead=lead(x, n=1)))
lista <- lapply(lista, function (j) mutate(j, dist = TrackReconstruction::CalcDistance(j$y, j$x, j$y.lead, j$x.lead)))#Da la distancia en kilómetros

suma <- as.data.frame(sapply(lista, function(x) sum(x$dist, na.rm = TRUE)))
rango <- sapply(lista, function(x) range(x$timestamp, na.rm = TRUE)) 
rango_t <- data.frame(t(rango))
dura<- as.vector(lubridate::as_datetime(rango_t$X2) - lubridate::as_datetime(rango_t$X1))
dura <- dura/60/60
velo <- suma/dura

boxplot(velo) #Mediana de 9.7 a lo largo de todo el día #Media de 9.49
media <- mean(velo$`sapply(lista, function(x) sum(x$dist, na.rm = TRUE))`, na.rm = TRUE)
length<- as.data.frame(length(lista))
media <- cbind(media, length)
write.csv(media, "C:/Users/rdelatorre/Desktop/12.csv")
rm(list=ls())


### TRECE



sep01 <- read.csv("C:/Users/rdelatorre/Desktop/gtfs.metrobus/sep/sep11.csv")
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
sep01 <- sep01 %>% select(timestamp, vehicle, speed, date, hour, x, y, route) %>% filter(hour=="13")


boxplot(sep01$speed) #Mediana de 6 a lo largo de todo el día

#Check. Se usan estas funciones para obtener los camiones que particpan en las rutas diarias

jul.fun <- function(i) {
  sep01 %>% select(timestamp, vehicle, speed, date, hour, x, y, route) %>% filter(vehicle==i)
}

for (i in sep01$vehicle) {
  assign (paste0("vehi", sep=".", i), data.frame(jul.fun(i)))
}

#Check.Agregar las trayectorias a una lista. Para la verificación deben coincidir el número de elementos de la lista con el número de valores únicos de la variable vehicle. 

lista <- mget(ls(pattern = "vehi.")) # con este comando agrega los dataframes con un patrón a la lista
unique(sep01$vehicle)

#Check. Teniendo las trayectorias en una lista.

lista <- lapply(lista, function(x) mutate(x, y.lead=lead(y, n=1)))
lista <- lapply(lista, function(x) mutate(x, x.lead=lead(x, n=1)))
lista <- lapply(lista, function (j) mutate(j, dist = TrackReconstruction::CalcDistance(j$y, j$x, j$y.lead, j$x.lead)))#Da la distancia en kilómetros

suma <- as.data.frame(sapply(lista, function(x) sum(x$dist, na.rm = TRUE)))
rango <- sapply(lista, function(x) range(x$timestamp, na.rm = TRUE)) 
rango_t <- data.frame(t(rango))
dura<- as.vector(lubridate::as_datetime(rango_t$X2) - lubridate::as_datetime(rango_t$X1))
dura <- dura/60/60
velo <- suma/dura

boxplot(velo) #Mediana de 9.7 a lo largo de todo el día #Media de 9.49
media <- mean(velo$`sapply(lista, function(x) sum(x$dist, na.rm = TRUE))`, na.rm = TRUE)
length<- as.data.frame(length(lista))
media <- cbind(media, length)
write.csv(media, "C:/Users/rdelatorre/Desktop/13.csv")
rm(list=ls())

#######CATORCE




sep01 <- read.csv("C:/Users/rdelatorre/Desktop/gtfs.metrobus/sep/sep11.csv")
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
sep01 <- sep01 %>% select(timestamp, vehicle, speed, date, hour, x, y, route) %>% filter(hour=="14")


boxplot(sep01$speed) #Mediana de 6 a lo largo de todo el día

#Check. Se usan estas funciones para obtener los camiones que particpan en las rutas diarias

jul.fun <- function(i) {
  sep01 %>% select(timestamp, vehicle, speed, date, hour, x, y, route) %>% filter(vehicle==i)
}

for (i in sep01$vehicle) {
  assign (paste0("vehi", sep=".", i), data.frame(jul.fun(i)))
}

#Check.Agregar las trayectorias a una lista. Para la verificación deben coincidir el número de elementos de la lista con el número de valores únicos de la variable vehicle. 

lista <- mget(ls(pattern = "vehi.")) # con este comando agrega los dataframes con un patrón a la lista
unique(sep01$vehicle)

#Check. Teniendo las trayectorias en una lista.

lista <- lapply(lista, function(x) mutate(x, y.lead=lead(y, n=1)))
lista <- lapply(lista, function(x) mutate(x, x.lead=lead(x, n=1)))
lista <- lapply(lista, function (j) mutate(j, dist = TrackReconstruction::CalcDistance(j$y, j$x, j$y.lead, j$x.lead)))#Da la distancia en kilómetros

suma <- as.data.frame(sapply(lista, function(x) sum(x$dist, na.rm = TRUE)))
rango <- sapply(lista, function(x) range(x$timestamp, na.rm = TRUE)) 
rango_t <- data.frame(t(rango))
dura<- as.vector(lubridate::as_datetime(rango_t$X2) - lubridate::as_datetime(rango_t$X1))
dura <- dura/60/60
velo <- suma/dura

boxplot(velo) #Mediana de 9.7 a lo largo de todo el día #Media de 9.49
media <- mean(velo$`sapply(lista, function(x) sum(x$dist, na.rm = TRUE))`, na.rm = TRUE)
length<- as.data.frame(length(lista))
media <- cbind(media, length)
write.csv(media, "C:/Users/rdelatorre/Desktop/14.csv")
rm(list=ls())


##### QUINCE



sep01 <- read.csv("C:/Users/rdelatorre/Desktop/gtfs.metrobus/sep/sep11.csv")
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
sep01 <- sep01 %>% select(timestamp, vehicle, speed, date, hour, x, y, route) %>% filter(hour=="15")


boxplot(sep01$speed) #Mediana de 6 a lo largo de todo el día

#Check. Se usan estas funciones para obtener los camiones que particpan en las rutas diarias

jul.fun <- function(i) {
  sep01 %>% select(timestamp, vehicle, speed, date, hour, x, y, route) %>% filter(vehicle==i)
}

for (i in sep01$vehicle) {
  assign (paste0("vehi", sep=".", i), data.frame(jul.fun(i)))
}

#Check.Agregar las trayectorias a una lista. Para la verificación deben coincidir el número de elementos de la lista con el número de valores únicos de la variable vehicle. 

lista <- mget(ls(pattern = "vehi.")) # con este comando agrega los dataframes con un patrón a la lista
unique(sep01$vehicle)

#Check. Teniendo las trayectorias en una lista.

lista <- lapply(lista, function(x) mutate(x, y.lead=lead(y, n=1)))
lista <- lapply(lista, function(x) mutate(x, x.lead=lead(x, n=1)))
lista <- lapply(lista, function (j) mutate(j, dist = TrackReconstruction::CalcDistance(j$y, j$x, j$y.lead, j$x.lead)))#Da la distancia en kilómetros

suma <- as.data.frame(sapply(lista, function(x) sum(x$dist, na.rm = TRUE)))
rango <- sapply(lista, function(x) range(x$timestamp, na.rm = TRUE)) 
rango_t <- data.frame(t(rango))
dura<- as.vector(lubridate::as_datetime(rango_t$X2) - lubridate::as_datetime(rango_t$X1))
dura <- dura/60/60
velo <- suma/dura

boxplot(velo) #Mediana de 9.7 a lo largo de todo el día #Media de 9.49
media <- mean(velo$`sapply(lista, function(x) sum(x$dist, na.rm = TRUE))`, na.rm = TRUE)
length<- as.data.frame(length(lista))
media <- cbind(media, length)
write.csv(media, "C:/Users/rdelatorre/Desktop/15.csv")
rm(list=ls())


#### DIECISEIS




sep01 <- read.csv("C:/Users/rdelatorre/Desktop/gtfs.metrobus/sep/sep11.csv")
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
sep01 <- sep01 %>% select(timestamp, vehicle, speed, date, hour, x, y, route) %>% filter(hour=="16")


boxplot(sep01$speed) #Mediana de 6 a lo largo de todo el día

#Check. Se usan estas funciones para obtener los camiones que particpan en las rutas diarias

jul.fun <- function(i) {
  sep01 %>% select(timestamp, vehicle, speed, date, hour, x, y, route) %>% filter(vehicle==i)
}

for (i in sep01$vehicle) {
  assign (paste0("vehi", sep=".", i), data.frame(jul.fun(i)))
}

#Check.Agregar las trayectorias a una lista. Para la verificación deben coincidir el número de elementos de la lista con el número de valores únicos de la variable vehicle. 

lista <- mget(ls(pattern = "vehi.")) # con este comando agrega los dataframes con un patrón a la lista
unique(sep01$vehicle)

#Check. Teniendo las trayectorias en una lista.

lista <- lapply(lista, function(x) mutate(x, y.lead=lead(y, n=1)))
lista <- lapply(lista, function(x) mutate(x, x.lead=lead(x, n=1)))
lista <- lapply(lista, function (j) mutate(j, dist = TrackReconstruction::CalcDistance(j$y, j$x, j$y.lead, j$x.lead)))#Da la distancia en kilómetros

suma <- as.data.frame(sapply(lista, function(x) sum(x$dist, na.rm = TRUE)))
rango <- sapply(lista, function(x) range(x$timestamp, na.rm = TRUE)) 
rango_t <- data.frame(t(rango))
dura<- as.vector(lubridate::as_datetime(rango_t$X2) - lubridate::as_datetime(rango_t$X1))
dura <- dura/60/60
velo <- suma/dura

boxplot(velo) #Mediana de 9.7 a lo largo de todo el día #Media de 9.49
media <- mean(velo$`sapply(lista, function(x) sum(x$dist, na.rm = TRUE))`, na.rm = TRUE)
length<- as.data.frame(length(lista))
media <- cbind(media, length)
write.csv(media, "C:/Users/rdelatorre/Desktop/16.csv")
rm(list=ls())


#### DIECISIETE



sep01 <- read.csv("C:/Users/rdelatorre/Desktop/gtfs.metrobus/sep/sep11.csv")
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
sep01 <- sep01 %>% select(timestamp, vehicle, speed, date, hour, x, y, route) %>% filter(hour=="17")


boxplot(sep01$speed) #Mediana de 6 a lo largo de todo el día

#Check. Se usan estas funciones para obtener los camiones que particpan en las rutas diarias

jul.fun <- function(i) {
  sep01 %>% select(timestamp, vehicle, speed, date, hour, x, y, route) %>% filter(vehicle==i)
}

for (i in sep01$vehicle) {
  assign (paste0("vehi", sep=".", i), data.frame(jul.fun(i)))
}

#Check.Agregar las trayectorias a una lista. Para la verificación deben coincidir el número de elementos de la lista con el número de valores únicos de la variable vehicle. 

lista <- mget(ls(pattern = "vehi.")) # con este comando agrega los dataframes con un patrón a la lista
unique(sep01$vehicle)

#Check. Teniendo las trayectorias en una lista.

lista <- lapply(lista, function(x) mutate(x, y.lead=lead(y, n=1)))
lista <- lapply(lista, function(x) mutate(x, x.lead=lead(x, n=1)))
lista <- lapply(lista, function (j) mutate(j, dist = TrackReconstruction::CalcDistance(j$y, j$x, j$y.lead, j$x.lead)))#Da la distancia en kilómetros

suma <- as.data.frame(sapply(lista, function(x) sum(x$dist, na.rm = TRUE)))
rango <- sapply(lista, function(x) range(x$timestamp, na.rm = TRUE)) 
rango_t <- data.frame(t(rango))
dura<- as.vector(lubridate::as_datetime(rango_t$X2) - lubridate::as_datetime(rango_t$X1))
dura <- dura/60/60
velo <- suma/dura

boxplot(velo) #Mediana de 9.7 a lo largo de todo el día #Media de 9.49
media <- mean(velo$`sapply(lista, function(x) sum(x$dist, na.rm = TRUE))`, na.rm = TRUE)
length<- as.data.frame(length(lista))
media <- cbind(media, length)
write.csv(media, "C:/Users/rdelatorre/Desktop/17.csv")
rm(list=ls())

##### DIECIOCHO



sep01 <- read.csv("C:/Users/rdelatorre/Desktop/gtfs.metrobus/sep/sep11.csv")
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
sep01 <- sep01 %>% select(timestamp, vehicle, speed, date, hour, x, y, route) %>% filter(hour=="18")


boxplot(sep01$speed) #Mediana de 6 a lo largo de todo el día

#Check. Se usan estas funciones para obtener los camiones que particpan en las rutas diarias

jul.fun <- function(i) {
  sep01 %>% select(timestamp, vehicle, speed, date, hour, x, y, route) %>% filter(vehicle==i)
}

for (i in sep01$vehicle) {
  assign (paste0("vehi", sep=".", i), data.frame(jul.fun(i)))
}

#Check.Agregar las trayectorias a una lista. Para la verificación deben coincidir el número de elementos de la lista con el número de valores únicos de la variable vehicle. 

lista <- mget(ls(pattern = "vehi.")) # con este comando agrega los dataframes con un patrón a la lista
unique(sep01$vehicle)

#Check. Teniendo las trayectorias en una lista.

lista <- lapply(lista, function(x) mutate(x, y.lead=lead(y, n=1)))
lista <- lapply(lista, function(x) mutate(x, x.lead=lead(x, n=1)))
lista <- lapply(lista, function (j) mutate(j, dist = TrackReconstruction::CalcDistance(j$y, j$x, j$y.lead, j$x.lead)))#Da la distancia en kilómetros

suma <- as.data.frame(sapply(lista, function(x) sum(x$dist, na.rm = TRUE)))
rango <- sapply(lista, function(x) range(x$timestamp, na.rm = TRUE)) 
rango_t <- data.frame(t(rango))
dura<- as.vector(lubridate::as_datetime(rango_t$X2) - lubridate::as_datetime(rango_t$X1))
dura <- dura/60/60
velo <- suma/dura

boxplot(velo) #Mediana de 9.7 a lo largo de todo el día #Media de 9.49
media <- mean(velo$`sapply(lista, function(x) sum(x$dist, na.rm = TRUE))`, na.rm = TRUE)
length<- as.data.frame(length(lista))
media <- cbind(media, length)
write.csv(media, "C:/Users/rdelatorre/Desktop/18.csv")
rm(list=ls())


#### DIECINUEVE



sep01 <- read.csv("C:/Users/rdelatorre/Desktop/gtfs.metrobus/sep/sep11.csv")
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
sep01 <- sep01 %>% select(timestamp, vehicle, speed, date, hour, x, y, route) %>% filter(hour=="19")


boxplot(sep01$speed) #Mediana de 6 a lo largo de todo el día

#Check. Se usan estas funciones para obtener los camiones que particpan en las rutas diarias

jul.fun <- function(i) {
  sep01 %>% select(timestamp, vehicle, speed, date, hour, x, y, route) %>% filter(vehicle==i)
}

for (i in sep01$vehicle) {
  assign (paste0("vehi", sep=".", i), data.frame(jul.fun(i)))
}

#Check.Agregar las trayectorias a una lista. Para la verificación deben coincidir el número de elementos de la lista con el número de valores únicos de la variable vehicle. 

lista <- mget(ls(pattern = "vehi.")) # con este comando agrega los dataframes con un patrón a la lista
unique(sep01$vehicle)

#Check. Teniendo las trayectorias en una lista.

lista <- lapply(lista, function(x) mutate(x, y.lead=lead(y, n=1)))
lista <- lapply(lista, function(x) mutate(x, x.lead=lead(x, n=1)))
lista <- lapply(lista, function (j) mutate(j, dist = TrackReconstruction::CalcDistance(j$y, j$x, j$y.lead, j$x.lead)))#Da la distancia en kilómetros

suma <- as.data.frame(sapply(lista, function(x) sum(x$dist, na.rm = TRUE)))
rango <- sapply(lista, function(x) range(x$timestamp, na.rm = TRUE)) 
rango_t <- data.frame(t(rango))
dura<- as.vector(lubridate::as_datetime(rango_t$X2) - lubridate::as_datetime(rango_t$X1))
dura <- dura/60/60
velo <- suma/dura

boxplot(velo) #Mediana de 9.7 a lo largo de todo el día #Media de 9.49
media <- mean(velo$`sapply(lista, function(x) sum(x$dist, na.rm = TRUE))`, na.rm = TRUE)
length<- as.data.frame(length(lista))
media <- cbind(media, length)
write.csv(media, "C:/Users/rdelatorre/Desktop/19.csv")
rm(list=ls())



#### VEINTE





sep01 <- read.csv("C:/Users/rdelatorre/Desktop/gtfs.metrobus/sep/sep11.csv")
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
sep01 <- sep01 %>% select(timestamp, vehicle, speed, date, hour, x, y, route) %>% filter(hour=="20")


boxplot(sep01$speed) #Mediana de 6 a lo largo de todo el día

#Check. Se usan estas funciones para obtener los camiones que particpan en las rutas diarias

jul.fun <- function(i) {
  sep01 %>% select(timestamp, vehicle, speed, date, hour, x, y, route) %>% filter(vehicle==i)
}

for (i in sep01$vehicle) {
  assign (paste0("vehi", sep=".", i), data.frame(jul.fun(i)))
}

#Check.Agregar las trayectorias a una lista. Para la verificación deben coincidir el número de elementos de la lista con el número de valores únicos de la variable vehicle. 

lista <- mget(ls(pattern = "vehi.")) # con este comando agrega los dataframes con un patrón a la lista
unique(sep01$vehicle)

#Check. Teniendo las trayectorias en una lista.

lista <- lapply(lista, function(x) mutate(x, y.lead=lead(y, n=1)))
lista <- lapply(lista, function(x) mutate(x, x.lead=lead(x, n=1)))
lista <- lapply(lista, function (j) mutate(j, dist = TrackReconstruction::CalcDistance(j$y, j$x, j$y.lead, j$x.lead)))#Da la distancia en kilómetros

suma <- as.data.frame(sapply(lista, function(x) sum(x$dist, na.rm = TRUE)))
rango <- sapply(lista, function(x) range(x$timestamp, na.rm = TRUE)) 
rango_t <- data.frame(t(rango))
dura<- as.vector(lubridate::as_datetime(rango_t$X2) - lubridate::as_datetime(rango_t$X1))
dura <- dura/60/60
velo <- suma/dura

boxplot(velo) #Mediana de 9.7 a lo largo de todo el día #Media de 9.49
media <- mean(velo$`sapply(lista, function(x) sum(x$dist, na.rm = TRUE))`, na.rm = TRUE)
length<- as.data.frame(length(lista))
media <- cbind(media, length)
write.csv(media, "C:/Users/rdelatorre/Desktop/20.csv")
rm(list=ls())


### VEINTIUNO



sep01 <- read.csv("C:/Users/rdelatorre/Desktop/gtfs.metrobus/sep/sep11.csv")
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
sep01 <- sep01 %>% select(timestamp, vehicle, speed, date, hour, x, y, route) %>% filter(hour=="21")


boxplot(sep01$speed) #Mediana de 6 a lo largo de todo el día

#Check. Se usan estas funciones para obtener los camiones que particpan en las rutas diarias

jul.fun <- function(i) {
  sep01 %>% select(timestamp, vehicle, speed, date, hour, x, y, route) %>% filter(vehicle==i)
}

for (i in sep01$vehicle) {
  assign (paste0("vehi", sep=".", i), data.frame(jul.fun(i)))
}

#Check.Agregar las trayectorias a una lista. Para la verificación deben coincidir el número de elementos de la lista con el número de valores únicos de la variable vehicle. 

lista <- mget(ls(pattern = "vehi.")) # con este comando agrega los dataframes con un patrón a la lista
unique(sep01$vehicle)

#Check. Teniendo las trayectorias en una lista.

lista <- lapply(lista, function(x) mutate(x, y.lead=lead(y, n=1)))
lista <- lapply(lista, function(x) mutate(x, x.lead=lead(x, n=1)))
lista <- lapply(lista, function (j) mutate(j, dist = TrackReconstruction::CalcDistance(j$y, j$x, j$y.lead, j$x.lead)))#Da la distancia en kilómetros

suma <- as.data.frame(sapply(lista, function(x) sum(x$dist, na.rm = TRUE)))
rango <- sapply(lista, function(x) range(x$timestamp, na.rm = TRUE)) 
rango_t <- data.frame(t(rango))
dura<- as.vector(lubridate::as_datetime(rango_t$X2) - lubridate::as_datetime(rango_t$X1))
dura <- dura/60/60
velo <- suma/dura

boxplot(velo) #Mediana de 9.7 a lo largo de todo el día #Media de 9.49
media <- mean(velo$`sapply(lista, function(x) sum(x$dist, na.rm = TRUE))`, na.rm = TRUE)
length<- as.data.frame(length(lista))
media <- cbind(media, length)
write.csv(media, "C:/Users/rdelatorre/Desktop/21.csv")
rm(list=ls())


###VEINTIDOS



sep01 <- read.csv("C:/Users/rdelatorre/Desktop/gtfs.metrobus/sep/sep11.csv")
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
sep01 <- sep01 %>% select(timestamp, vehicle, speed, date, hour, x, y, route) %>% filter(hour=="10")


boxplot(sep01$speed) #Mediana de 6 a lo largo de todo el día

#Check. Se usan estas funciones para obtener los camiones que particpan en las rutas diarias

jul.fun <- function(i) {
  sep01 %>% select(timestamp, vehicle, speed, date, hour, x, y, route) %>% filter(vehicle==i)
}

for (i in sep01$vehicle) {
  assign (paste0("vehi", sep=".", i), data.frame(jul.fun(i)))
}

#Check.Agregar las trayectorias a una lista. Para la verificación deben coincidir el número de elementos de la lista con el número de valores únicos de la variable vehicle. 

lista <- mget(ls(pattern = "vehi.")) # con este comando agrega los dataframes con un patrón a la lista
unique(sep01$vehicle)

#Check. Teniendo las trayectorias en una lista.

lista <- lapply(lista, function(x) mutate(x, y.lead=lead(y, n=1)))
lista <- lapply(lista, function(x) mutate(x, x.lead=lead(x, n=1)))
lista <- lapply(lista, function (j) mutate(j, dist = TrackReconstruction::CalcDistance(j$y, j$x, j$y.lead, j$x.lead)))#Da la distancia en kilómetros

suma <- as.data.frame(sapply(lista, function(x) sum(x$dist, na.rm = TRUE)))
rango <- sapply(lista, function(x) range(x$timestamp, na.rm = TRUE)) 
rango_t <- data.frame(t(rango))
dura<- as.vector(lubridate::as_datetime(rango_t$X2) - lubridate::as_datetime(rango_t$X1))
dura <- dura/60/60
velo <- suma/dura

boxplot(velo) #Mediana de 9.7 a lo largo de todo el día #Media de 9.49
media <- mean(velo$`sapply(lista, function(x) sum(x$dist, na.rm = TRUE))`, na.rm = TRUE)
length<- as.data.frame(length(lista))
media <- cbind(media, length)
write.csv(media, "C:/Users/rdelatorre/Desktop/22.csv")
rm(list=ls())

#### VEINTITRES



sep01 <- read.csv("C:/Users/rdelatorre/Desktop/gtfs.metrobus/sep/sep11.csv")
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
sep01 <- sep01 %>% select(timestamp, vehicle, speed, date, hour, x, y, route) %>% filter(hour=="23")


boxplot(sep01$speed) #Mediana de 6 a lo largo de todo el día

#Check. Se usan estas funciones para obtener los camiones que particpan en las rutas diarias

jul.fun <- function(i) {
  sep01 %>% select(timestamp, vehicle, speed, date, hour, x, y, route) %>% filter(vehicle==i)
}

for (i in sep01$vehicle) {
  assign (paste0("vehi", sep=".", i), data.frame(jul.fun(i)))
}

#Check.Agregar las trayectorias a una lista. Para la verificación deben coincidir el número de elementos de la lista con el número de valores únicos de la variable vehicle. 

lista <- mget(ls(pattern = "vehi.")) # con este comando agrega los dataframes con un patrón a la lista
unique(sep01$vehicle)

#Check. Teniendo las trayectorias en una lista.

lista <- lapply(lista, function(x) mutate(x, y.lead=lead(y, n=1)))
lista <- lapply(lista, function(x) mutate(x, x.lead=lead(x, n=1)))
lista <- lapply(lista, function (j) mutate(j, dist = TrackReconstruction::CalcDistance(j$y, j$x, j$y.lead, j$x.lead)))#Da la distancia en kilómetros

suma <- as.data.frame(sapply(lista, function(x) sum(x$dist, na.rm = TRUE)))
rango <- sapply(lista, function(x) range(x$timestamp, na.rm = TRUE)) 
rango_t <- data.frame(t(rango))
dura<- as.vector(lubridate::as_datetime(rango_t$X2) - lubridate::as_datetime(rango_t$X1))
dura <- dura/60/60
velo <- suma/dura

boxplot(velo) #Mediana de 9.7 a lo largo de todo el día #Media de 9.49
media <- mean(velo$`sapply(lista, function(x) sum(x$dist, na.rm = TRUE))`, na.rm = TRUE)
length<- as.data.frame(length(lista))
media <- cbind(media, length)
write.csv(media, "C:/Users/rdelatorre/Desktop/23.csv")
rm(list=ls())

