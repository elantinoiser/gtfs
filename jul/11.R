jul.jn.rbind <- read.csv("D:/Escritorio/gtfs.mes/jul/jul.jn.rbind.csv")

jul.jn.rbind$year <- 2020
jul.jn.rbind$month <- 08

jul.jn.rbind$month <- stringr::str_pad(jul.jn.rbind$month, width = 2, side = "left", pad = "0")
jul.jn.rbind$day <- stringr::str_pad(jul.jn.rbind$day, width = 2, side = "left", pad = "0")


jul.jn.rbind$date <- paste(jul.jn.rbind$year,jul.jn.rbind$month, jul.jn.rbind$day, sep = "-")
jul.jn.rbind$date <- as.POSIXct(jul.jn.rbind$date)
head(jul.jn.rbind$date)

jul.jn.rbind$hour <- stringr::str_pad(jul.jn.rbind$hour, width = 2, side = "left", pad = "0")
jul.jn.rbind$hour <- stringr::str_pad(jul.jn.rbind$hour, width = 3, side = "right", pad = ":")
jul.jn.rbind$hour <- stringr::str_pad(jul.jn.rbind$hour, width = 4, side = "right", pad = "0")
jul.jn.rbind$hour <- stringr::str_pad(jul.jn.rbind$hour, width = 5, side = "right", pad = "0")

jul.jn.rbind$ts<- as.POSIXct(paste(jul.jn.rbind$date, jul.jn.rbind$hour), "CST6CDT")






picacho <- jul.jn.rbind %>% select(id, nom, day, hour, n, mean, median, max, sd, p75, year, month, date, ts) %>% filter(nom=="PICACH")

## First make up some mock data
## Make a data frame with a full series of dates from the min date to the max date
## in the incomplete data frame
full_dates <- seq(min(picacho$ts), max(picacho$ts), 
                  by = "1 hour")
full_dates <- data.frame(date = full_dates)

## Merge the complete data frame with the incomplete to fill in the dates and add 
## NAs for missing values
my_complete_data <- merge(full_dates, picacho, by = "date", 
                          all.x = TRUE)
