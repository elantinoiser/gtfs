#Convertir de timestamp a datetime
gtfs_rt_jul_ago<- read.csv("/Users/85412/Desktop/gtfs_rt/gtfs_rt_jul_ago.csv")
gtfs_rt_jul_ago$TIMESTAMP<- lubridate::as_datetime(gtfs_rt_jul_ago$TIMESTAMP)

#Es necesario realizar una serie de transformaciones de los datos para:
#Quitar agosto para quedarme únicamente con julio
#Que el formato datetime correspondan a la hora del centro de México
gtfs_rt_jul_ago$anio<-substr(gtfs_rt_jul_ago$TIMESTAMP, 1,4)
gtfs_rt_jul_ago$mes<-substr(gtfs_rt_jul_ago$TIMESTAMP, 6,7)
gtfs_rt_jul_ago$dia<-substr(gtfs_rt_jul_ago$TIMESTAMP, 9,10)
gtfs_rt_jul_ago$hora<-substr(gtfs_rt_jul_ago$TIMESTAMP, 12,13)
gtfs_rt_jul_ago$minuto<-substr(gtfs_rt_jul_ago$TIMESTAMP, 15,16)

