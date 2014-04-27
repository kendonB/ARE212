
library(maps)
library(fields)
data <- read.csv("farmers-mkts.csv", header = TRUE)
statelist <- c("New Mexico", "Colorado", "Arizona", "Utah")
state.data <- data[is.element(data$State, statelist), ]
state.data <- state.data[state.data$x < -80,]
coords = cbind(state.data$x, state.data$y)

library(RCurl)
library(RJSONIO)
convertCoords <- function(coord.collection) {
  apply(coord.collection, 1, function(x) { paste(x[2], x[1], sep = ",") })
}

getElevation <- function(coord.strings) {
  base.url <- "http://maps.googleapis.com/maps/api/elevation/json?locations="
  params <- "&sensor=false"
  coord.str <- paste(convertCoords(coord.strings), collapse = "|")
  query <- paste(base.url, coord.str, params, sep="")
  gotten <- getURL(query)

  output <- fromJSON(gotten, unexpected.escape = "skip")$results

  elev <- function(x) {
    return(x[1][["elevation"]])
  }

  res <- as.matrix(lapply(output, elev))
  return(res)
}

testmatrix <- matrix(c(-122.27,37.83,-157.49,1.87), nrow = 2, byrow = T)
convertCoords(testmatrix)
getElevation(testmatrix)

partition <- function(df, each = 10) {
  s <- seq(ceiling(nrow(df) / each))
  suppressWarnings(res <- split(df, rep(s, each = each)))
  return(res)
}

elev.split <- lapply(partition(as.data.frame(coords)), getElevation)
elevation <- unlist(elev.split)

library(CartoDB)
cartodb("<insert your username>", api.key = "<insert your key>")

uploadMarket <- function(record, table.name = "markets") {
  cartodb.row.insert(name = table.name,
    columns = list("x", "y", "cluster", "elevation"),
    values = as.list(record))
}

mkts <- data.frame(x = state.data$x, y = state.data$y,
  cluster = cl, elevation = elevation)

apply(mkts, 1, uploadMarket)

library(raster)
boze.img <- brick("LE70390282003100EDC00.jpg")
names(boze.img)
nlayers(boze.img)

png(filename="inserts/boze_sat.png",height=300,width=500)
plotRGB(boze.img)
dev.off()

#ext <- drawExtent()
ext <- extent(matrix(c(3770, 4119, 1337, 1626), nrow = 2, byrow = T))

png(filename="inserts/boze_sat_crop.png",height=300,width=500)
boze.cr <- crop(boze.img, ext)
plotRGB(boze.cr)
dev.off()

boze.df <- as.data.frame(boze.cr)
dim.cr <- dim(boze.cr)
all(dim.cr[1] * dim.cr[2] == dim(boze.df)[1])
summary(boze.df)

numgroups <- 6
set.seed(42)
kmeans <- kmeans(boze.df, numgroups, iter.max = 25, nstart = 10)

png(filename="inserts/boze_km.png",height=300,width=500)
mat.km <- matrix(kmeans$cluster, nrow = boze.cr@nrows, ncol = boze.cr@ncols, byrow = T)
rast.km <- raster(mat.km)
rast.km@extent <- ext
rast.km@crs <- boze.cr@crs
plot(rast.km)
dev.off()

png(filename="inserts/boze_km_col.png",height=300,width=500)
library(RColorBrewer)
brewer.pal(6, "BrBG")
plot(rast.km, col = brewer.pal(numgroups,"Spectral"))
dev.off()
