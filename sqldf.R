#https://www.youtube.com/watch?v=VEN1qMcQDsY&feature=emb_title

install.packages("sqldf")
data()
sqldf::sqldf("select mpg, cyl from mtcars where cyl>=6 or cyl=4")