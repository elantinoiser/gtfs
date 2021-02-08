#Unir con las estaciones de la línea 1. Pero antes, corregir los nombres de las estaciones.
l1.mbus.est <- sf::st_read("D:/Escritorio/gtfs_estatico/est.l1.shp")

#Corregir strings
l1.mbus.est$nombre<- stringr::str_replace(l1.mbus.est$nombre, "Buenavista. LÃ­nea 1.", "Buenavista")
l1.mbus.est$nombre<- stringr::str_replace(l1.mbus.est$nombre, "Plaza de la RepÃºblica", "Plaza de la Republica")
l1.mbus.est$nombre<- stringr::str_replace(l1.mbus.est$nombre, "Ã\u0081lvaro ObregÃ³n", "Alvaro Obregon")
l1.mbus.est$nombre<- stringr::str_replace(l1.mbus.est$nombre, "Dr. GÃ¡lvez", "Dr. Galvez")
l1.mbus.est$nombre<- stringr::str_replace(l1.mbus.est$nombre, "Manuel GonzÃ¡lez", "Manuel Gonzalez")
l1.mbus.est$nombre<- stringr::str_replace(l1.mbus.est$nombre, "RevoluciÃ³n", "Revolucion")
l1.mbus.est$nombre<- stringr::str_replace(l1.mbus.est$nombre, "La Raza. LÃ­nea 1.", "La Raza")
l1.mbus.est$nombre<- stringr::str_replace(l1.mbus.est$nombre, "San SimÃ³n", "San Simon")
l1.mbus.est$nombre<- stringr::str_replace(l1.mbus.est$nombre, "Nuevo LeÃ³n", "Nuevo Leon")
l1.mbus.est$nombre<- stringr::str_replace(l1.mbus.est$nombre, "NÃ¡poles", "Napoles")
l1.mbus.est$nombre<- stringr::str_replace(l1.mbus.est$nombre, "Santa Ãšrsula", "Santa Ursula")
l1.mbus.est$nombre<- stringr::str_replace(l1.mbus.est$nombre, "RÃ­o Churubusco", "Rio Churubusco")
l1.mbus.est$nombre<- stringr::str_replace(l1.mbus.est$nombre, "Villa OlÃ­mpica", "Villa Olimpica")
l1.mbus.est$nombre<- stringr::str_replace(l1.mbus.est$nombre, "JosÃ© MarÃ­a Velasco", "Jose Maria Velasco")


sf::st_write(l1.mbus.est, "D:/Escritorio/gtfs_estatico/l1.mbus.est03.shp")