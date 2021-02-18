#https://gist.github.com/ateucher/e2e5bd0b592f3efd6d56

#Importar datos de velocidades del Metrobús y precipitaciones.


jul.gtfs.ohiiunam <- read.csv("D:/Escritorio/jul.gtfs.ohiiunam.csv")
head(jul.gtfs.ohiiunam)


#Crear variables de mes y año


jul.gtfs.ohiiunam$year <- 2020
jul.gtfs.ohiiunam$month <- 07
jul.gtfs.ohiiunam$Date <- paste(jul.gtfs.ohiiunam$year,jul.gtfs.ohiiunam$month, jul.gtfs.ohiiunam$day, sep = "-")
head(jul.gtfs.ohiiunam$Date)

jul.gtfs.ohiiunam$hour <- stringr::str_pad(jul.gtfs.ohiiunam$hour, width = 2, side = "left", pad = "0")
jul.gtfs.ohiiunam$hour <- stringr::str_pad(jul.gtfs.ohiiunam$hour, width = 3, side = "right", pad = ":")
jul.gtfs.ohiiunam$hour <- stringr::str_pad(jul.gtfs.ohiiunam$hour, width = 4, side = "right", pad = "0")
jul.gtfs.ohiiunam$hour <- stringr::str_pad(jul.gtfs.ohiiunam$hour, width = 5, side = "right", pad = "0")

jul.gtfs.ohiiunam$ts<- as.POSIXct(paste(jul.gtfs.ohiiunam$Date, jul.gtfs.ohiiunam$hour), "CST6CDT")

picacho <- jul.gtfs.ohiiunam %>% select(id, nom, day, hour, n, mean, median, max, sd, p75, prec.acum.mm, year, month, Date, ts) %>% filter(nom=="PICACH")

## Make a data frame with a full series of dates from the min date to the max date
## in the incomplete data frame
full_dates <- seq(min(picacho$ts), max(picacho$ts), 
                  by = "1 hour")
full_dates <- data.frame(ts = full_dates)

## Merge the complete data frame with the incomplete to fill in the dates and add 
## NAs for missing values
my_complete_data <- merge(full_dates, picacho, by = "ts", 
                          all.x = TRUE)