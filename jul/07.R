#Establecimiento del directorio de trabajo
setwd("D:/Escritorio/estaciones.ohiiunam/julio")
#Agregar archivos .csv
temp <- list.files(pattern="*.csv")
#Lista con los archivos .csv del directorio
lista = lapply(temp, read.csv)
#Fecha.hora <- lapply(lista, function(x) substr (x$Fecha.hora,1,2)) 
lista<- lapply(lista, transform, Fecha.hora = as.character(Fecha.hora))
#Cambio de nombres
new_col_name <- c("estacion", "fecha.hora", "intensidad.mm.h")
names(lista) <- c("estacion", "fecha.hora", "intensidad.mm.h")

lista<- lapply(lista, setNames, nm = new_col_name)
lista <- lapply(lista, transform, estacion = as.character(estacion))
lista <- lapply(lista, transform, fecha.hora = as.character(fecha.hora))
#
lista <- lapply(lista, transform, nombre = substr(estacion, 1,6))
lista <- lapply(lista, transform, fecha = substr (fecha.hora,1,2))
lista <- lapply(lista, transform, hora = substr (fecha.hora,12,13))
lista <- lapply(lista, transform, nombre = as.character(nombre))
lista <- lapply(lista, transform, fecha = as.character(fecha))
lista <- lapply(lista, transform, hora = as.character(hora))
lista <- lapply(lista, transform, id = paste0(nombre, fecha, hora))
lista <- lapply(lista, transform, id = as.character(id))
lista <- lapply(lista, transform, intensidad.mm.h = as.numeric(intensidad.mm.h))

lista <- lapply(lista, transform, acumulada = intensidad.mm.h*.0166)
#
lista.sum <- lapply(lista,  function(i) {
  
  i %>% group_by(id) %>% summarize(sum(acumulada))
  
})

cchvallejo<- as.data.frame(lista.sum[1])
dhumanos <- as.data.frame(lista.sum[2])
sacmex <- as.data.frame(lista.sum[4])
zapata <- as.data.frame(lista.sum[5])


rlist::list.rbind(lista.sum)

write.csv(as.data.frame(rlist::list.rbind(lista.sum)), "D:/Escritorio/estaciones.ohiiunam/julio/lista.sum.csv")

#Hasta aquí funcionó bien para todos los archivos, excepto el de picacho

names(picacho.07) <- c("estacion", "fecha.hora", "intensidad.mm.h")
picacho.07$estacion <- as.character(picacho.07$estacion)
picacho.07$fecha.hora <- as.character(picacho.07$fecha.hora)
picacho.07$nombre <- substr(picacho.07$estacion, 1, 6)
picacho.07$fecha <- substr(picacho.07$fecha.hora, 9,10)
picacho.07$hora <- substr(picacho.07$fecha.hora, 12,13)
picacho.07$id <- paste0(picacho.07$nombre, picacho.07$fecha, picacho.07$hora)
picacho.07$acumulada<- picacho.07$intensidad.mm.h*.0166

#¿por qué demonios no funciona con esta?
#picacho <- picacho.07 %>% group_by(picacho.07$id) %>% summarize(sum(picacho.07$acumulada))

#con esta sí funciona

picacho<- aggregate(picacho.07$acumulada, list(picacho.07$id), FUN=sum)

names(picacho) <- c("id", "acum")
names(dhumanos) <- c("id", "acum")
names(zapata) <- c("id", "acum")
names(sacmex) <- c("id", "acum")
names(cchvallejo) <- c("id", "acum")

jul.rbind <- rbind(picacho, dhumanos, zapata, sacmex, cchvallejo)

write.csv(jul.rbind, "D:/Escritorio/estaciones.ohiiunam/julio/jul.rbind.csv")

