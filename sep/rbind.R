X6 <- read_csv("C:/Users/rdelatorre/Desktop/6.csv")
X7 <- read_csv("C:/Users/rdelatorre/Desktop/7.csv")
X8 <- read_csv("C:/Users/rdelatorre/Desktop/8.csv")
X9 <- read_csv("C:/Users/rdelatorre/Desktop/9.csv")
X10 <- read_csv("C:/Users/rdelatorre/Desktop/10.csv")
X11 <- read_csv("C:/Users/rdelatorre/Desktop/11.csv")
X12 <- read_csv("C:/Users/rdelatorre/Desktop/12.csv")
X13 <- read_csv("C:/Users/rdelatorre/Desktop/13.csv")
X14 <- read_csv("C:/Users/rdelatorre/Desktop/14.csv")
X15 <- read_csv("C:/Users/rdelatorre/Desktop/15.csv")
X16 <- read_csv("C:/Users/rdelatorre/Desktop/16.csv")
X17 <- read_csv("C:/Users/rdelatorre/Desktop/17.csv")
X18 <- read_csv("C:/Users/rdelatorre/Desktop/18.csv")
X19 <- read_csv("C:/Users/rdelatorre/Desktop/19.csv")
X20 <- read_csv("C:/Users/rdelatorre/Desktop/20.csv")
X21 <- read_csv("C:/Users/rdelatorre/Desktop/21.csv")
X22 <- read_csv("C:/Users/rdelatorre/Desktop/22.csv")
X23 <- read_csv("C:/Users/rdelatorre/Desktop/23.csv")

####

rbind <- rbind(X6,X7,X8, X9, X10, X11, X12, X22)

write.csv(rbind, "C:/Users/rdelatorre/Desktop/rbind.csv")
