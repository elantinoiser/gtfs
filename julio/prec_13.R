setwd("/Users/85412/Desktop/estaciones_ohiiunam/julio")
temp = list.files(pattern="*.csv")
lista = lapply(temp, read.csv)

Fecha.hora <- lapply(lista, function(x) substr (x$Fecha.hora,1,2)) 


lista2<- lapply(lista, transform, Fecha.hora = as.character(Fecha.hora))

lista3 <- lapply(lista2, transform, fecha = substr (Fecha.hora,1,2))
lista3 <- lapply(lista3, transform, fecha = as.character(fecha))

new_col_name <- c("estacion", "fecha.hora", "intensidad.mm.h", "acumulada", "fecha")

lista3<- lapply(lista3, setNames, nm = new_col_name)

lista3.mean <- lapply(lista3,  function(i) {
  
  i %>% group_by(fecha) %>% summarize(mean(intensidad.mm.h))
  
})





lista4 <- lapply(lista2, transform, fecha = substr (Fecha.hora,1,10))

lista4 <- lapply(lista4, transform, fecha= as.character(fecha))

lista5 <- lapply(lista4, transform, hora = substr (Fecha.hora,12,13))

lista5 <- lapply(lista4, transform, hora= as.character(hora))





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
