#Cargamos la mitad de la distribución de las observaciones que hay de la línea 1 entre el 13 y el 31 de julio.

jul.jn <- read.csv("C:/Users/85412/Downloads/jul.jn.csv")
jul.jn <- jul.jn %>% select(Id, TIMESTAMP,LABEL, LECTURA, ruta) %>% filter(ruta=="00L2")
names(jul.jn) <- c("timestamp", "vehicle", "y", "x", "ruta")
#
jul.jn$timestamp <- lubridate::as_datetime(jul.jn$timestamp, tz="CST6CDT")
jul.jn$vehicle<- substr(jul.jn$vehicle, 4,8)
jul.jn$y<- substr(jul.jn$y, 10, 16) 
jul.jn$x<- substr(jul.jn$x, 3, 10) 
jul.jn$x <- as.numeric(jul.jn$x)
jul.jn$y <- as.numeric(jul.jn$y)

#Creamos los identificadores únicos que estarán conformados por el día, la hora y el id del vehículo.

jul.jn$day<- lubridate::day(jul.jn$timestamp)
jul.jn$hour<- lubridate::hour(jul.jn$timestamp)
jul.jn$id <- paste0(jul.jn$day, sep=".", jul.jn$hour, sep=".", jul.jn$vehicle)
#
#Se aplica un for loop a las 31,353 observaciones.

unicos<- unique((jul.jn$id))

for (i in unicos) {
  assign(paste0("jul", sep=".", i), data.frame(jul.jn %>%  select(timestamp, vehicle, x, y, ruta, id) %>% filter (jul.jn$id==i)))  
}

#Meter los dataframes creados con el for loop anterior.

lista <- mget(ls(pattern = "jul.")) # aquí están los vehículos
lista$jul.jn <- NULL

#Estimamos distancias recorridas por cada vehículo
lista01 <- lista
lista01 <- lapply(lista01, function(x) mutate(x, y.l=lead(y, n=1)))
lista01 <- lapply(lista01, function(x) mutate(x, x.l=lead(x, n=1)))
lista01 <- lapply(lista01, function (j) mutate(j, dist = TrackReconstruction::CalcDistance(j$y, j$x, j$y.l, j$x.l)))#Da la distancia en kilómetros

#Agregado de distancias recorridas por hora

suma <- sapply(lista01, function(x) sum(x$dist, na.rm = TRUE))
rango <- sapply(lista01, function(x) range(x$timestamp, na.rm = TRUE)) 
rango_t <- data.frame(t(rango))
cbind <- cbind(rango_t, suma)
cbind$dura <- lubridate::as_datetime(cbind$X2) - lubridate::as_datetime(cbind$X1)
cbind$dura<- cbind$dura/60/60
#
cbind$x2<- lubridate::as_datetime(cbind$X2)
cbind$x1<- lubridate::as_datetime(cbind$X1)
cbind$dura <- as.numeric(cbind$dura)
cbind$velo<- cbind$suma/cbind$dura 
cbind$rnames <- rownames(cbind)
cbind$dia <- substr(cbind$rnames, 5,6)
cbind$hora <- substr(cbind$rnames, 8,9)
#
cbind<- cbind %>% select(X1, X2, suma, dura, x2, x1, velo, rnames, dia, hora) %>% filter(!rnames=="jul.jn")
#
sum.cbind <- cbind %>% group_by(dia, hora) %>% summarize(mean(velo, na.rm=TRUE))
#
write.csv(sum.cbind, "C:/Users/85412/Desktop/sum.cbind.2.csv")
#
plot(cbind$velo, na.rm =TRUE)
unname(quantile(cbind$velo, 1, na.rm = TRUE))
sd(cbind$velo, na.rm = TRUE)
mean(cbind$velo, na.rm = TRUE)
median(cbind$velo, na.rm = TRUE)
range(cbind$velo, na.rm = TRUE)
hist(cbind$velo)
ggplot(cbind, aes(velo)) + geom_histogram()



