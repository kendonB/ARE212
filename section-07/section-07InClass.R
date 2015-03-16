library(ggplot2)
data(iris)

g <- ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width))
g + geom_point()

g <- ggplot(data = iris) + 
  aes(x = Sepal.Length, y = Sepal.Width)

g <- g + geom_point(aes(color = Species))
g
# brewer palette
# RColorbrewer??
g <- g + theme(panel.background = element_blank())
g

install.packages("rgdal")
library(rgdal)
date <- "20140503"
type <- "tmax"
range <- "daily"
getPRISMGrid <- function(date, type, range = "daily") {
  # get a temporary .zip file name
  tmpFile <- tempfile(pattern = paste0(date, type))
  
  fileName <- paste0(tmpFile, ".zip")
  # The url with type, date, and range
  url <- paste0("http://www.prism.oregonstate.edu/fetchData.php?type",
                "=bil&kind=recent&elem=", type, "&range=", range,
                "&temporal=", date)
  
  # Download the file to fileName
  download.file(url = url, mode = "wb", destfile = fileName)
  
  # if a SpatialGridDataFrame is to be returned: unzip the
  # file to the working directory.
  tmpFile <- unzip(zipfile = fileName, overwrite = TRUE)
  # list.files(path = file.path(tmpFile, ".."))
  
  # read into R
  spGrid <- readGDAL(fname = tmpFile[1])
  class(spGrid)
  
  # delete the zip file
  file.remove(tmpFile)
  
  # Add remove all the other junk
  # Exercise.
  return(spGrid)
}
mygrid <- getPRISMGrid(date = "20140503", type = "tmax",
                       range = "daily")

