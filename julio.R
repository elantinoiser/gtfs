julio <- readr::read_csv("C:/Users/85412/Desktop/gtfs_rt/julio.csv")
julio$CST6CDT<- lubridate::as_datetime(julio$TIMESTAMP, tz="CST6CDT")

