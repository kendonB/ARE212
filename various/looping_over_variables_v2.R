rm(list = ls())

# load in iris data from base R
iris.df <- iris

# goal: to draw the histogram for some of our variables. 
# let's assume that we have three different variables we want to draw
slength <- iris$Sepal.Length
swidth <- iris$Sepal.Width
pwidth <- iris$Petal.Width

iris.df <- data.frame(slength, swidth, pwidth)

png("test.png")
hist(pwidth)
dev.off()

for (i in 1:3) {
  print(paste0("graphs/",names(iris.df)[i],".png"))
  png(paste0("graphs/",names(iris.df)[i],".png"))
  hist(iris.df[, i], main = paste("M1: Histogram of",names(iris.df)[i]))
  dev.off
}

# method one: put them all into a dataframe, pull from there

iris.df <- data.frame(slength, swidth, pwidth)
names(iris.df)

for (i in 1:3) {
  hist(iris.df[, i], main = paste("M1: Histogram of",names(iris.df)[i]))
}

# method two: using get() on a list of variable names
varlist <- c("slength","swidth","pwidth")
j <- 1
for (i in varlist) {
  hist(get(i), main = paste("M2: Histogram of",varlist[j]))
  j <- j + 1
}

# method three: using eval(parse)
# this might be the most general but it's not really any fun at all
varlist <- c("slength","swidth","pwidth")
for (i in varlist) {
  evalstring = paste0("hist(",i,",main = \"M3: Histogram of ", i,"\")")
  print(evalstring)
  eval(parse(text = evalstring))
}