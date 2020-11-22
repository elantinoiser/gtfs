preg <- THOGAR[,5:47]
preg <- preg[, !sapply(preg, is.character)]
princomp(preg[1:16], cor=T)
factanal(preg[,2:16], factors = 5)


#
preg0 <- preg[,!sapply(preg, is.numeric)]
princomp(na.omit(preg), cor = TRUE)
