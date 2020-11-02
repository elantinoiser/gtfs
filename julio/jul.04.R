ggpubr::ggboxplot(cbind, x = "velo", y = "hora", 
          color = "group", palette = c("#00AFBB", "#E7B800"),
          ylab = "velo", xlab = "Groups")

t.test(cbind_10$velo, cbind_8$velo, alternative = "two.sided", var.equal = FALSE)

cbind_2_8 <- cbind_2 %>% select(X1, X1_1, X2, suma, dura, x2, x1, velo, rnames, dia, hora) %>% filter(hora=="8") 

t.test(cbind_2_8$velo, cbind_2_23$velo, alternative = "two.sided", var.equal = FALSE)

boxplot(cbind_10$velo, cbind_18$velo)

est.l1 <-sf::st_read("/Users/85412/Desktop/gtfs_estatico/est.l1.shp")
est.l1<- est.l1 %>% arrange(lat, lon)
est.l1$nombre <- as.character(est.l1$nombre) 
est.l1.coords <- sf::st_coordinates(est.l1$geometry)
names(est.l1.coords) <- c("lon", "lat")
est.l1 <- as.data.frame(est.l1) 
est.l1 <- cbind(est.l1, est.l1.coords)

est.l1<- est.l1 %>% select(est.l1, X, Y) %>% mutate(., y.l=lead(Y, n=1)) %>% mutate(., x.l=lead(X, n=1)) 
est.dist<- TrackReconstruction::CalcDistance(est.l1$Y, est.l1$X, est.l1$y.l, est.l1$x.l)
est.l1.dist <- cbind(est.l1, est.dist)