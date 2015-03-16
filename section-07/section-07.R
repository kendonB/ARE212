
## ----echo=TRUE, eval=1:2, message=FALSE----------------------------------
library(ggplot2)
data(iris)
ggplot(data = iris, ... )


## ------------------------------------------------------------------------
g <- ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width))


## ------------------------------------------------------------------------
g + geom_point()


## ------------------------------------------------------------------------
g + geom_point(aes(color=Species))


## ------------------------------------------------------------------------
g + geom_point(aes(color=Species)) + theme(panel.background = element_blank())


## ----message=FALSE, eval=FALSE, tidy=TRUE, cache=TRUE, tidy.opts=list(width.cutoff=55)----
## library(rgdal)
## getPRISMGrid <- function(date, type, range = "daily") {
##   # get a temporary .zip file name
##   tmpFile <- tempfile(pattern = paste(date, type, sep = ""))
##   fileName <- paste(tmpFile, ".zip", sep = "")
## 
##   # The url with type, date, and range
##   url <- paste0("http://www.prism.oregonstate.edu/fetchData.php?type",
##                "=bil&kind=recent&elem=", type, "&range=", range, "&temporal=",
##                date)
## 
##   # Download the file to fileName
##   download.file(url = url, mode = "wb", destfile = fileName)
## 
##   # if a SpatialGridDataFrame is to be returned:
##   # unzip the file to the working directory.
##   tmpFile <- unzip(zipfile = fileName, overwrite = TRUE)
## 
##   # read into R
##   spGrid <- readGDAL(fname = tmpFile[1])
## 
##   # delete the zip file
##   file.remove(tmpFile)
## 
##   return(spGrid)
## }
## mygrid <- getPRISMGrid(date = "20140503", type = "tmax", range = "daily")

## ----echo=FALSE, message=FALSE-------------------------------------------
library(Hmisc)
date <- "20140503"
type <- "tmax"
range <- "daily"
tmpFile <- tempfile(pattern = paste(date, type, sep = ""))
fileName <- paste(tmpFile, ".zip", sep = "")
  
# The url with type, date, and range
url <- paste0("http://www.prism.oregonstate.edu/fetchData.php?type", 
               "=bil&kind=recent&elem=", type, "&range=", range, "&temporal=", 
               date)


## ----echo=FALSE----------------------------------------------------------
url1 <- substr(url, 1, 51)
url2 <- substr(url, 52, nchar(url))
cat(url1)
cat(url2)


## ----echo=FALSE----------------------------------------------------------
cat("<request url>?<param1Name>=<param1Value>&<param2Name>=<param2Value>&...")


