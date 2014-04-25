
png(filename="inserts/us-mkts.png",height=400,width=800)
library(maps)
data <- read.csv("farmers-mkts.csv", header = TRUE)
map("state", interior = FALSE)
title("Farmers' markets")
map("state", boundary = FALSE, col = "gray", add = TRUE)
points(data$x, data$y, cex = 0.2, col = "blue")
dev.off()

statelist <- c("New Mexico", "Colorado", "Arizona", "Utah")
state.data <- data[is.element(data$State, statelist), ]
state.data <- state.data[state.data$x < -80,]
dim(state.data)
names(state.data)

geom <- cbind(state.data$x, state.data$y)
euc.dist <- dist(geom)
head(euc.dist)
euc.dist.mtx <- as.matrix(euc.dist)
euc.dist.mtx[1:6, 1:6]

library(fields)
gc.dist.mtx <- rdist.earth(geom)
gc.dist.mtx[1:6, 1:6]

nearestneighbor <- apply(gc.dist.mtx, 2, which.min)
head(nearestneighbor)

n <- nrow(gc.dist.mtx)
nearestneighbor <- apply(gc.dist.mtx + diag(999999, n, n), 2, which.min)
head(nearestneighbor)

X <- state.data[, 8:ncol(state.data)]
X <- apply(X, 2, function(col) { ifelse(col == "Y", 1, 0) })
X[1:6, c("Honey", "Jams", "Poultry")]

dum.dist <- dist(X, method = "binary")

png(filename="inserts/dend.png",height=600,width=600)
hclust.res <- hclust(dum.dist)
plot(cut(as.dendrogram(hclust.res), h = 0)$upper, leaflab = "none")
dev.off()

cl <- cutree(hclust.res, k = 5)
head(cl)

png(filename="inserts/zoom.png",height=600,width=600)
assignColor <- function(cl.idx) {
  col.codes <- c("#FF8000", "#0080FF", "#FFBF00", "#FF4000")
  return(col.codes[cl.idx])
}

map("state", interior = FALSE,
    xlim = c(-117, -101), ylim = c(28, 43))
map("state", boundary = FALSE, col="gray", add = TRUE,
    xlim = c(-117, -101), ylim = c(28, 43))
points(state.data$x, state.data$y, cex = 1, pch = 20, col = assignColor(cl))
dev.off()

library(maptools)

segDistance <- function(coord) {
  segment <- cbind(c(-109.047546, -109.047546, -103.002319),
    c(31.33487100, 36.99816600, 36.99816600))
  near.obj <- nearestPointOnSegment(segment, coord)
  return(as.numeric(near.obj[["distance"]]))
}

coords = cbind(state.data$x, state.data$y)
dist.NM <- apply(coords, 1, FUN = segDistance)
dist.NM <- dist.NM * ifelse(state.data[["State"]] == "New Mexico", -1, 1)
head(dist.NM)

png(filename="inserts/disc.png",height=400,width=900)
sel.cl <- cl < 5
plot(dist.NM[sel.cl], cl[sel.cl], pch = 20, col = "blue",
  xlab = "Distance to New Mexico border (in degrees)",
  ylab = "Cluster category", yaxt = "n")
abline(v = 0, lty = 3, col = "red")
axis(2, at = 1:4)
dev.off()

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
cartodb("pbaylis", api.key = "46611aa6e943054dfe074605b7107bdd96a45bb9")

uploadMarket <- function(record, table.name = "markets") {
  cartodb.row.insert(name = table.name,
    columns = list("x", "y", "cluster", "elevation"),
    values = as.list(record))
}

mkts <- data.frame(x = state.data$x, y = state.data$y,
  cluster = cl, elevation = elevation)

apply(mkts, 1, uploadMarket)
