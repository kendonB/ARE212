library(sp)
library(rgdal)
library(ggmap)
# SpatialLines
tmpFile <- tempfile()
download.file("http://www2.census.gov/geo/tiger/GENZ2013/cb_2013_us_state_20m.zip",
              destfile = tmpFile)
unzipped <- unzip(zipfile = tmpFile, exdir = getwd())
states <- readOGR(dsn = getwd(), layer = "cb_2013_us_state_20m")
file.remove(tmpFile)
file.remove(unzipped)

class(states)
states$NAME
head(states@data)
slotNames(states)
proj4string(states)

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
image(tMaxGrid)
file.remove(tmpFile)
file.remove(unzippedFiles)

# see for nice color schemes 
library(RColorBrewer)
head(tMaxGrid@data)
image(tMaxGrid)
plot(states, add=TRUE)

library(ggmap)
distances <- mapdist(from = "Giannini Hall, UC Berkeley, CA 94720",
                     to = as.character(states@data$NAME),
                     mode = "driving") #$
states$distanceToGiannini <- distances$km

tMax <- aggregate(tMaxGrid, by = states, FUN = mean, na.rm=TRUE)
class(tMax)

states$tMax <- tMax@data$band1
summary(lm(distanceToGiannini ~ tMax, data = states@data))


