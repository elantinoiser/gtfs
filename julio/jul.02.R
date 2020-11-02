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
jul.l1$id <- paste0(jul.l1$day, sep=".", jul.l1$hour, sep=".", jul.l1$vehicle)

#

unicos<- unique((jul.l1$id))

for (i in unicos) {
  assign(paste0("jul", sep=".", i), data.frame(jul.l1 %>%  select(timestamp, vehicle, speed, x, y, ruta, id) %>% filter (jul.l1$id==i)))  
}

#

lista <- mget(ls(pattern = "jul.")) # aquí están los vehículos
lista$jul.l1 <- NULL
lista$julio.sp.jn.l1 <- NULL

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
) 
#
cbind$x2<- lubridate::as_datetime(cbind$X2)
cbind$x1<- lubridate::as_datetime(cbind$X1)
cbind$dura <- as.numeric(cbind$dura)
cbind$velo<- cbind$suma/cbind$dura 
cbind$rnames <- rownames(cbind)
cbind$dia <- substr(cbind$rnames, 5,6)
cbind$hora <- substr(cbind$rnames, 8,9)
#

sum.cbind <- cbind %>% group_by(dia, hora) %>% summarize(mean(velo, na.rm=TRUE))
plot(sum.cbind$`mean(velo, na.rm = TRUE)`)

write.csv(sum.cbind, "C:/Users/85412/Desktop/sum.cbind.csv")
