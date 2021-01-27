library(sp)
library(maptools)
#https://r-spatial.github.io/sf/reference/st_transform.html

l1.mbus.est <- sf::st_read("D:/Escritorio/gtfs_estatico/l1.mbus.est03.shp")

lineas.metrobus <- sf::st_read("D:/Escritorio/lineas.metrobus/lineas-metrobus.shp")

#Hasta Dr. Gálvez
linea1.1<- st_linestring(rbind(c(-99.1691, 19.2793), c(-99.1700, 19.2803), c(-99.1755, 19.2837), c(-99.1744, 19.2883), c(-99.1773, 19.2925), c(-99.1855, 19.2994), c(-99.1860, 19.3044), c(-99.1874, 19.3146), c(-99.1884, 19.3227), c(-99.1899, 19.3408)))
plot(linea1.1)


linea1.1 <- st_wrap_dateline(st_sfc(st_linestring(rbind(c(-99.1691, 19.2793), c(-99.1700, 19.2803), c(-99.1755, 19.2837), c(-99.1744, 19.2883), c(-99.1773, 19.2925), c(-99.1855, 19.2994), c(-99.1860, 19.3044), c(-99.1874, 19.3146), c(-99.1884, 19.3227), c(-99.1899, 19.3408), crs=4326))))

linea1.1 <- st_sfc(st_linestring(rbind(c(-99.1691, 19.2793), c(-99.1700, 19.2803), c(-99.1755, 19.2837), c(-99.1744, 19.2883), c(-99.1773, 19.2925), c(-99.1855, 19.2994), c(-99.1860, 19.3044), c(-99.1874, 19.3146), c(-99.1884, 19.3227), c(-99.1899, 19.3408))))


#Esta es la que funciona
linea1.1<- sf::st_wrap_dateline(sf::st_sfc(sf::st_linestring(rbind(c(-99.1691, 19.2793),c(-99.1700, 19.2803), c(-99.1755, 19.2837), c(-99.1744, 19.2883), c(-99.1773, 19.2925), c(-99.1812, 19.2941), c(-99.1855, 19.2994), c(-99.1860, 19.3044), c(-99.1874, 19.3146), c(-99.1884, 19.3227), c(-99.1899, 19.3408))), crs = 4326))
linea1.1 = linea1.1 %>% sf::st_set_crs(4326) %>% sf::st_transform(32214)
sf::st_crs(linea1.1)
sf::st_geometry_type(linea1.1)
sf::write_sf(linea1.1, "D:/Escritorio/gtfs_estatico/linea1.1.shp")

