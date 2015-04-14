
## ------------------------------------------------------------------------
library(sp)
library(rgdal)

## ----cache=TRUE, warning=FALSE, message=FALSE----------------------------
# Download a USA shapefile
tmpFile <- tempfile()
download.file("http://www2.census.gov/geo/tiger/GENZ2013/cb_2013_us_state_20m.zip", 
              destfile = tmpFile)
unzipped <- unzip(zipfile = tmpFile, exdir = getwd())

# This is the actual reading stage
states <- readOGR(dsn = getwd(), layer = "cb_2013_us_state_20m")

# Remove all the files on disk
file.remove(tmpFile)
file.remove(unzipped)


## ----fig.height = 4------------------------------------------------------
plot(states)


## ------------------------------------------------------------------------
slotNames(states)


## ------------------------------------------------------------------------
head(states@data)


## ----cache=TRUE----------------------------------------------------------
tmpFile <- tempfile()

url <- paste0("http://www.prism.oregonstate.edu/fetchData.php?type", 
              "=bil&kind=recent&elem=tmax&range=daily&temporal=", 
              "20140125")

# Download the file to fileName
download.file(url = url, mode = "wb", destfile = tmpFile)

# unzip the file to the working directory.
unzippedFiles <- unzip(zipfile = tmpFile, overwrite = TRUE)

# read into R
tMaxGrid <- readGDAL(fname = unzippedFiles[1])

# delete the file
file.remove(tmpFile)
file.remove(unzippedFiles)

## ----fig.height=5, fig.width=6-------------------------------------------
image(tMaxGrid)


## ----cache=TRUE, message=FALSE-------------------------------------------
library(ggmap)
distances <- mapdist(from = "Giannini Hall, UC Berkeley, CA 94720",
                     to = as.character(states@data$NAME),
                     mode = "driving") #$
states$distanceToGiannini <- distances$km


## ------------------------------------------------------------------------
# This gets the simple average of the grid cells whose centres
# are in each state. NOT a good solution for smaller polygons
# like ZIPs. Stored in a SpatialPolygonsDataFrame.
tMax <- aggregate(tMaxGrid, by = states, FUN = mean, na.rm=TRUE)

# Access the temperature and store it in the states object.
states$tMax <- tMax@data$band1


## ------------------------------------------------------------------------
summary(lm(distanceToGiannini ~ tMax, data = states@data))


