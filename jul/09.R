jul.jn.rbind <- read.csv("D:/Escritorio/gtfs.mes/jul/jul.jn.rbind.csv")
jul.rbind <- read.csv("D:/Escritorio/estaciones.ohiiunam/julio/jul.rbind.csv")

left_join<-dplyr::left_join(jul.jn.max.rbind, jul.rbind, by="id")

left_join$X.x <- NULL
left_join$X.y <- NULL




#julio.piloto<- left_join[with(left_join, order(dia, hora)), ]

write.csv(left_join, "D:/Escritorio/julio.piloto.csv")

jul.rbind <- read.csv("D:/Escritorio/estaciones.ohiiunam/julio/jul.rbind.csv")


############################################################



