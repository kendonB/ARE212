
require(foreign)
data <- read.csv("auto.csv", header=TRUE)
names(data) <- c("price", "mpg", "weight")

row.names(data)[1:5]
row.names(data) <- 3:76
row.names(data)[1:5]
row.names(data)[1:2]  <- c("myrow1","myrow2")
row.names(data)[1:5]

data$price[2] = NA
weirdmeans1 <- data.frame(Means=rowMeans(data))
weirdmeans2 <- data.frame(Means=rowMeans(data, na.rm=TRUE))
cbind(head(weirdmeans1),head(weirdmeans2))

png(filename="inserts/graph1.png",height=300,width=500)
par(mar=c(5.1,4.1,4.1,5.1)) # just so we can fit the label for the second y axis
plot(data$mpg,data$price,col="red",ylab="price",xlab="mpg")
par(new=TRUE)
plot(data$mpg,data$weight,col="blue",xaxt="n",yaxt="n",xlab="",ylab="")
axis(4)
mtext("weight",side=4,line=3)
dev.off()

(A <- matrix(1:6, ncol=2))

(B <- matrix(1:6, ncol=3, byrow=TRUE))

A == t(B)

all(A == t(B))

dim(A)
dim(B)

B %*% A

B %*% t(A)

B * t(A)

A * 2

e <- matrix(1:3)

apply(A, 2, function(x) {x * e})

whoop <- function(x) {x * e}
apply(A, 2, whoop)

I <- diag(5)

all(solve(I) == I)

library(psych)
tr(I)

ls()

rm(list = ls())
