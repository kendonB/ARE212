
library(foreign)
data <- read.csv("auto.csv", header=TRUE)

names(data)

names(data) <- c("price", "mpg", "weight")

head(data)

head(data$mpg)

data.sorted <- data[order(data$price), ]
head(data.sorted)

mean(data$price)

png(filename="inserts/graph1.png",height=300,width=500)
plot(data$weight,data$mpg, main="Weight and MPG", xlab="Car weight", ylab="Miles per gallon")
dev.off()

png(filename="inserts/graph2.png",height=300,width=500)
hist(data$price, main="Car price distribution", xlab="Car price")
dev.off()
