library(sp)
library(rgdal)

# Download a USA shapefile
tmpFile <- tempfile()
download.file("http://www2.census.gov/geo/tiger/GENZ2013/cb_2013_us_state_20m.zip", destfile = tmpFile)
unzipped <- unzip(zipfile = tmpFile, exdir = getwd())

# This is the actual reading stage
states <- readOGR(dsn = getwd(), layer = "cb_2013_us_state_20m")

# Remove all the files on disk
file.remove(tmpFile)
file.remove(unzipped)

plot(states)

slotNames(states)

states <- states[!(states$NAME %in% c("Hawaii", "Puerto Rico", "Alaska")),]


tmpFile <- tempfile()

url <- paste0("http://www.prism.oregonstate.edu/fetchData.php?type", 
              "=bil&kind=recent&elem=tmax&range=daily&temporal=", 
              "20140416")

# Download the file to fileName
download.file(url = url, mode = "wb", destfile = tmpFile)

# unzip the file to the working directory.
unzippedFiles <- unzip(zipfile = tmpFile, overwrite = TRUE)

# read into R
tMaxGrid <- readGDAL(fname = unzippedFiles[1])

# delete the file
file.remove(tmpFile)
file.remove(unzippedFiles)

image(tMaxGrid)
# image(tMaxGrid, col = rev(heat.colors(100, alpha = 1)))
# library(CartoDB)
# 
# cartodb("kmbell56", api.key = "486e96409e3dddc093f05f0e36167ef39c67b1e8")
# 
# uploadTemp <- function(record, table.name = "temperature_on_april_16_2014") {
#   cartodb.row.insert(name = table.name,
#                      columns = list("x", "y", "Temperature"),
#                      values = as.list(record))
# }
# 
# spPointsTMax <- SpatialPointsDataFrame(tMaxGrid, data = tMaxGrid@data)
# spPointsTMax <- spPointsTMax[!is.na(spPointsTMax$band1),]
# 
# temperature <- data.frame(x = spPointsTMax@coords[,1], 
#                           y = spPointsTMax@coords[,2], 
#                           temperature = spPointsTMax$band1[])
# # Literally takes hours
# # apply(temperature, 1, uploadTemp)
# write.csv(temperature, file="temperature.csv")
