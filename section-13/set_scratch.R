rm(list = ls())

# Load and plot the scanned map
require(raster)


# need to install raster and rgdal

# image from LANDSAT 7: L7 ETM+ SLC-on
# Area around Bozeman, MT 
# Entity ID: LE70390282003100EDC00
# Acquisition Date: 10-APR-03
# Path: 39
# Row: 28

# creates a 3-layer raster brick object. one color per color
boze.img <- brick("LE70390282003100EDC00.jpg")
names(boze.img)
nlayers(boze.img)
plotRGB(boze.img)

# maybe try plot()

# draw an extent one way or another
ext <- drawExtent() #draw a box by clicking upper left and lower right corner in the plot
ext <- extent(matrix(c(3770, 4119, 1337, 1626), nrow = 2, byrow = T))

# ext is a bounding box of pixels

# crop image and plot the result
boze.cr <- crop(boze.img, ext)
plotRGB(boze.cr)

# this is a data frame of all of the pixels. 
boze.df <- as.data.frame(boze.cr)
# we can verify
dim.cr <- dim(boze.cr)
dim.cr[1] * dim.cr[2]

# what happens if we have NA?
summary(boze.df) # to make sure you don't have any NA's
#summary(boze.df.err) # to make sure you don't have any NA's

# do some unsupervised classification (as before) using k means clustering
#kmeans <- kmeans(boze.df.err, 12, iter.max = 100, nstart = 10) # breaks
time.start <- proc.time()
numgroups <- 6
kmeans <- kmeans(boze.df, numgroups, iter.max = 25, nstart = 10)
proc.time() - time.start

# use the k means clusters to create a new raster (by way of a matrix)
mat.km <- matrix(kmeans$cluster, nrow = boze.cr@nrows, ncol = boze.cr@ncols, byrow = T)
rast.km <- raster(mat.km)
rast.km@extent <- ext
rast.km@crs <- boze.cr@crs
plot(rast.km)

library(RColorBrewer)
brewer.pal(6, "BrBG")
plot(rast.km, col = brewer.pal(numgroups,"Spectral"))


# let's get some better colors


