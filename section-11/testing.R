
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


## ------------------------------------------------------------------------
tax <- data.frame(NAME=c("Alabama", "Alaska", "Arizona", "Arkansas", 
                         "California", "Colorado", "Connecticut", 
                         "Delaware", "District of Columbia", "Florida", 
                         "Georgia", "Hawaii", "Idaho", "Illinois", 
                         "Indiana", "Iowa", "Kansas", "Kentucky", 
                         "Louisiana", "Maine", "Maryland", 
                         "Massachusetts", "Michigan", "Minnesota", 
                         "Mississippi", "Missouri", "Montana", 
                         "Nebraska", "Nevada", "New Hampshire", 
                         "New Jersey", "New Mexico", "New York", 
                         "North Carolina", "North Dakota", "Ohio", 
                         "Oklahoma ", "Oregon", "Pennsylvania", 
                         "Rhode Island ", "South Carolina", 
                         "South Dakota", "Tennessee", "Texas", 
                         "Utah", "Vermont", "Virginia", "Washington", 
                         "West Virginia", "Wisconsin", "Wyoming"),
                  taxRate=c(5, 0, 4.54, 6.9, 12.3, 0, 6.7, 6.6, 
                            8.95, 0, 6, 11, 7.4, 0, 0, 8.98, 4.6, 
                            6, 6, 7.95, 5.75, 0, 0, 9.85, 5, 6, 
                            6.9, 6.84, 0, 0, 8.97, 4.9, 8.82, 0, 
                            3.22, 5.333, 5.25, 9.9, 0, 5.99, 7, 
                            0, 0, 0, 0, 8.95, 5.75, 0, 6.5, 7.65, 0))
mergedDf <- merge(states@data, tax, all.x = TRUE)
states@data <- mergedDf

## ------------------------------------------------------------------------
# This gets the simple average of the grid cells whose centres
# are in each state. NOT a good solution for smaller polygons
# like ZIPs. Stored in a SpatialPolygonsDataFrame.
tMax <- aggregate(tMaxGrid, by = states, FUN = mean, na.rm=TRUE)

# Access the temperature and store it in the states object.
states$tMax <- tMax@data$band1


## ------------------------------------------------------------------------
summary(lm(taxRate ~ tMax, data = states@data))

library(ggmap)
distances <- mapdist(from = "Giannini Hall, UC Berkeley, CA 94720",
                     to = as.character(states@data$NAME))$km
