setwd("/Users/85412/Desktop/estaciones_ohiiunam/julio")
temp = list.files(pattern="*.csv")
myfiles = lapply(temp, read.csv)

Fecha.hora <- lapply(myfiles, function(x) substr (x$Fecha.hora,1,10)) 


myfiles2<- lapply(myfiles, transform, Fecha.hora = as.character(Fecha.hora)[1])


#Sí funcionó
summarized_list <- lapply(myfiles,  function(i) {

i %>% group_by(Fecha.hora) %>% summarize(mean(precip))

})


acopilco_202007$dia<- substr(acopilco_202007$`Fecha/hora`,9,10)
acopilco_202007$hora<- substr(acopilco_202007$`Fecha/hora`,12,13)
acopilco_202007$diayhora <- paste0(acopilco_202007$dia, acopilco_202007$hora)


#Sí funcionó

prec_agre <- stats::aggregate(acopilco_202007,
                by = list(acopilco_202007$dia, acopilco_202007$hora),
                FUN = mean)

unicos_dia <- unique(acopilco_202007$dia)
unicos_hora <- unique(acopilco_202007$hora)

for (i in unicos_dia) {
  i<- assign(i, as.data.frame(prec_agre %>% select(Group.1, Group.2, `Intensidad [mm/h]`) %>% filter(Group.1==i)) ) 
  
}



prec_agre_14<- prec_agre %>% select(Group.1, Group.2, `Intensidad [mm/h]`) %>% filter(Group.1=="14")
