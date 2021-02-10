#Establecimiento del directorio de trabajo
setwd("D:/Escritorio/estaciones_ohiiunam/julio")
#Agregar archivos .csv
temp <- list.files(pattern="*.csv")
#Lista con los archivos .csv del directorio
lista = lapply(temp, read.csv)
#Fecha.hora <- lapply(lista, function(x) substr (x$Fecha.hora,1,2)) 
lista2<- lapply(lista, transform, Fecha.hora = as.character(Fecha.hora))
#Cambio de nombres
new_col_name <- c("estacion", "fecha.hora", "intensidad.mm.h", "acumulada")
names(lista2) <- c("estacion", "fecha.hora", "intensidad.mm.h", "acumulada")

lista2<- lapply(lista2, setNames, nm = new_col_name)
lista2 <- lapply(lista2, transform, estacion = as.character(estacion))
lista2 <- lapply(lista2, transform, fecha.hora = as.character(fecha.hora))
summary(lista2[[34]]$intensidad.mm.h)
#
lista3 <- lapply(lista2, transform, nombre = substr(estacion, 1,3))
lista3 <- lapply(lista3, transform, fecha = substr (fecha.hora,1,2))
lista3 <- lapply(lista3, transform, hora = substr (fecha.hora,12,13))
lista3 <- lapply(lista3, transform, nombre = as.character(nombre))
lista3 <- lapply(lista3, transform, fecha = as.character(fecha))
lista3 <- lapply(lista3, transform, hora = as.character(hora))
lista3 <- lapply(lista3, transform, id.fecha.hora = paste0(nombre, fecha, hora))
lista3 <- lapply(lista3, transform, id.fecha.hora = as.character(id.fecha.hora))
#
lista3.mean <- lapply(lista3,  function(i) {
  
  i %>% group_by(id.fecha.hora) %>% summarize(mean(intensidad.mm.h))
  
})
#
df.lista3.rbind <- rbind_list(lista3.mean)#voy aquí
names(df.lista3.rbind) <- c("id", "intensidad.mm.h")
#
percentiles<- df.lista3.rbind %>% select(id, intensidad.mm.h) %>% filter(intensidad.mm.h>0)
quantile(percentiles$intensidad.mm.h, probs = c(0, 0.95), na.rm=TRUE)
#
df.lista3.rbind$nom <- substr(df.lista3.rbind$id, 1,3)
df.lista3.rbind$dia <- substr(df.lista3.rbind$id, 4,5)
df.lista3.rbind$hora <- substr(df.lista3.rbind$id, 6,7)
#Aquí me acabo de dar cuenta de que tengo muchos dias 0s
df.lista3.rbind$dia <- as.numeric(df.lista3.rbind$dia)
#
df.l3 <- df.lista3.rbind[ which(df.lista3.rbind$dia > 12), ]
df.l3$dia <- as.character(df.l3$dia) 
#
class(df.l3$intensidad.mm.h)
df.l3 %>% group_by(dia) %>% summarize(sum(intensidad.mm.h, na.rm = TRUE))







lista3 <- lapply(lista3, function(x), nom = substr(x$estacion, 4,5) )


lista3.id.fecha <- lapply(lista3, function(x) id.fecha=paste0(x$estacion,x$fecha))
lista3.id.fecha <- lapply(lista3, transform, id.fecha=paste0(estacion,fecha))
lista3.id.fecha <- lapply(lista3.id.fecha, transform, id.fecha=as.character(id.fecha))

lista3 <- lapply(lista3, transform, intensidad.mm.h = as.numeric(intensidad.mm.h))


lista3.mean <- lapply(lista3,  function(i) {
  
  i %>% group_by(id.fecha) %>% summarize(mean(intensidad.mm.h))
  
})

lista3.rbind <- rbind_list(lista3.mean)




lista4 <- lapply(lista3, transform, fecha = substr (fecha.hora,1,10))
lista4 <- lapply(lista4, transform, fecha= as.character(fecha))
lista4 <- lapply(lista4, transform, hora = substr (fecha.hora,12,13))
lista4 <- lapply(lista4, transform, hora= as.character(hora))

lista5 <- lapply(lista4, transform, id.fecha.hora = paste0(id.fecha, hora))
lista5 <- lapply(lista5, transform, id.fecha.hora = as.character(id.fecha.hora))

lista5.mean <- lapply(lista5,  function(i) {
  
  i %>% group_by(id.fecha.hora) %>% summarize(mean(intensidad.mm.h))
  
})

lista5.rbind.mean <- rbind_list(lista5.mean)

lista5.sum <- lapply(lista5,  function(i) {
  
  i %>% group_by(id.fecha.hora) %>% summarize(sum(intensidad.mm.h))
  
})

lista5.rbind.sum <- rbind_list(lista5.sum)

substrRight <- function(i, n){
  substr(i, nchar(i)-n+1, nchar(i))
}

lista5.rbind.sum$fecha.hora <- substrRight(lista5.rbind.sum$id.fecha.hora, 4)

lista5.rbind.sum$fecha <- substr(lista5.rbind.sum$fecha.hora, 1, 2)

lista5.rbind.sum <- lista5.rbind.sum %>% select(id.fecha.hora, `sum(intensidad.mm.h)`,fecha.hora, fecha) %>% filter(fecha=="13"|fecha=="14"| fecha=="15"|fecha=="16"| fecha=="17"|fecha=="18"| fecha=="19"| fecha=="20"|fecha=="21"|fecha=="22"|fecha=="23"|fecha=="24"|fecha=="25"|fecha=="26"|fecha=="27"|fecha=="28"|fecha=="29"|fecha=="30"|fecha=="31")


prec_agre <- stats::aggregate(acopilco_202007,
                              by = list(acopilco_202007$dia, acopilco_202007$hora),
                              FUN = mean)


acopilco_202007$dia<- substr(acopilco_202007$`Fecha/hora`,9,10)
acopilco_202007$hora<- substr(acopilco_202007$`Fecha/hora`,12,13)
acopilco_202007$diayhora <- paste0(acopilco_202007$dia, acopilco_202007$hora)


#Sí funcionó



unicos_dia <- unique(acopilco_202007$dia)
unicos_hora <- unique(acopilco_202007$hora)

for (i in unicos_dia) {
  i<- assign(i, as.data.frame(prec_agre %>% select(Group.1, Group.2, `Intensidad [mm/h]`) %>% filter(Group.1==i)) ) 
  
}



prec_agre_14<- prec_agre %>% select(Group.1, Group.2, `Intensidad [mm/h]`) %>% filter(Group.1=="14")
