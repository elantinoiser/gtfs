#
julio.sp.jn.l1 <- read.csv("C:/Users/85412/Desktop/julio.sp.jn.l1.csv") 
names(julio.sp.jn.l1) <- c("timestamp", "vehicle", "speed", "x", "y", "ruta")
julio.sp.jn.l1$x <- substr(julio.sp.jn.l1$x, 3, 10) 
julio.sp.jn.l1$y <- substr(julio.sp.jn.l1$y, 1, 8) 
julio.sp.jn.l1$timestamp <- as.POSIXct(julio.sp.jn.l1$timestamp)
julio.sp.jn.l1$x <- as.numeric(julio.sp.jn.l1$x)
julio.sp.jn.l1$y <- as.numeric(julio.sp.jn.l1$y)
jul.l1 <- julio.sp.jn.l1
#

jul.l1$day<- lubridate::day(jul.l1$timestamp)
jul.l1$hour<- lubridate::hour(jul.l1$timestamp)
jul.l1$id.vehi <- paste0(jul.l1$day, sep=".", jul.l1$hour)

#

#Check. Creación de un dataframe por vehículo ## Voy aquí

unicos<- unique((jul.l1$vehicle))

for (i in unicos) {
  assign(paste0("jul", sep=".", i), data.frame(jul.l1 %>%  select(timestamp, vehicle, speed, x, y, ruta) %>% filter (jul.l1$vehicle==i)))  
}
#
lista <- mget(ls(pattern = "jul.")) # aquí están los vehículos
lista$jul.l1 <- NULL
lista$julio.sp.jn.l1 <- NULL
#
#Estimamos distancias recorridas por cada vehículo
lista01 <- lista
lista01 <- lapply(lista01, function(x) mutate(x, y.l=lead(y, n=1)))
lista01 <- lapply(lista01, function(x) mutate(x, x.l=lead(x, n=1)))
lista01 <- lapply(lista01, function (j) mutate(j, dist = TrackReconstruction::CalcDistance(j$y, j$x, j$y.l, j$x.l)))#Da la distancia en kilómetros
lista02 <- lapply(lista01, function(k) mutate(k, dia = lubridate::day(k$timestamp)))
lista02 <- lapply(lista01, function(k) mutate(k, hora = lubridate::hour(k$timestamp)))

#Agregado de distancias recorridas a nivel mensual

suma <- sapply(lista01, function(x) sum(x$dist, na.rm = TRUE))
rango <- sapply(lista01, function(x) range(x$timestamp, na.rm = TRUE)) 
rango_t <- data.frame(t(rango))



#Ejemplo con 1 dataframe en particular para entender lo que está haciendo
#jul.1$y.l <- lead(jul.1$y, n=1)
#jul.1$x.l <- lead(jul.1$x, n=1)
#jul.1$dist<- TrackReconstruction::CalcDistance(jul.1$y,jul.1$x,jul.1$y.l, jul.1$x.l)







#Check. Se usan estas funciones para obtener los camiones que particpan en las rutas diarias

jul_fun <- function(i) {
  jul.15.20 %>% select(timestamp, vehicle, speed, x, y, ruta) %>% filter(vehicle==i)
}

for (i in jul.15.20$vehicle) {
  assign (paste0("vehi", sep=".", i), data.frame(jul_fun(i)))
}

#
lista <- mget(ls(pattern = "vehi.")) # con este comando agrega los dataframes con un patrón a la lista
#
list=ls()
list
unicos01 <- list
unicos01$unicos <- NULL 
for (i in unicos){
  i %>% mutate(i, dist=dist)
}






