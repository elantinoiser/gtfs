jul_gtfs_ohiiunam <- read_excel("D:/Escritorio/gtfs.mes/jul/jul.gtfs.ohiiunam.xlsx")
jul_gtfs_ohiiunam$llueve <- ifelse(jul_gtfs_ohiiunam$prec.acum.mm>0,1,0) 

llueve<- jul_gtfs_ohiiunam %>% group_by(nom, day) %>%
  summarise(
    sum = sum(llueve)
  ) 

llueve.hora<- jul_gtfs_ohiiunam %>% group_by(nom, hour) %>%
  summarise(
    sum = sum(llueve)
  ) 

write.csv(llueve.hora, "D:/Escritorio/gtfs.mes/jul/llueve.hora.csv")

jul_gtfs_ohiiunam %>% 
  group_by(llueve) %>% 
  summarise(mean = mean(mean, na.rm = TRUE))

jul_gtfs_ohiiunam %>% 
  group_by(llueve) %>% 
  summarise(mean = mean(max, na.rm = TRUE))


jul_gtfs_ohiiunam$llueve1SD <- ifelse(jul_gtfs_ohiiunam$prec.acum.mm>0.0804,1,0) 
jul_gtfs_ohiiunam$llueve2SD <- ifelse(jul_gtfs_ohiiunam$prec.acum.mm>0.0808,1,0)
jul_gtfs_ohiiunam$llueve3SD <- ifelse(jul_gtfs_ohiiunam$prec.acum.mm>0.0812,1,0)

jul_gtfs_ohiiunam %>% 
  group_by(llueve3SD) %>% 
  summarise(mean = mean(mean, na.rm = TRUE))


