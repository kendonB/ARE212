setwd("Dropbox/PhD/ARE212GSI/Github/ARE212/section-12/")
library(sp)
library(rgdal)
library(maps)
library(plotrix)

library(maps)
farmersMarkets <- read.csv("farmers-mkts.csv", header = TRUE)

windows() #x11() in Mac

map("state", interior = FALSE)

title("Farmers' markets")
map("state", boundary = FALSE, col = "gray", add = TRUE)

points(farmersMarkets$x, farmersMarkets$y, cex = 0.2, col = "blue")

bayZips <- c("94706", "94707", "94708", "94710",
             "94702", "94720", "94703", "94709",
             "94704", "94705", "94618", "94611",
             "94607", "94612", "94610", "94602",
             "94606", "94609", "94608",
             "94102", "94103", "94104", "94105",
             "94107", "94108", "94109", "94110",
             "94111", "94112", "94114", "94115",
             "94116", "94117", "94118", "94121",
             "94122", "94123", "94124", "94127",
             "94129", "94130", "94131", "94132",
             "94133", "94134", "94158", "94538")
bayFMs <- farmersMarkets[farmersMarkets$Zip %in% bayZips,]

tmpFile <- tempfile()
download.file("http://www2.census.gov/geo/tiger/GENZ2013/cb_2013_us_zcta510_500k.zip",
              destfile = tmpFile)
unzipped <- unzip(zipfile = tmpFile, exdir = getwd())
zipPoly <- readOGR(dsn = getwd(), layer = "cb_2013_us_zcta510_500k")
## OGR data source with driver: ESRI Shapefile
## Source: "C:/Users/Kenny/Dropbox/PhD/ARE212GSI/Github/ARE212/section-12", layer: "cb_2013_us_zcta510_500k"
## with 33144 features and 5 fields
## Feature type: wkbPolygon with 2 dimensions
# Remove all the files on disk
file.remove(tmpFile)
## [1] TRUE
file.remove(unzipped)
## [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE
bayPoly <- zipPoly[zipPoly$ZCTA5CE10 %in% bayZips,]

points(bayFMs$x, bayFMs$y, cex = 1, pch = 20)

head(bayFMs)

bayFMs$qualityScore <- apply(bayFMs, 1, function(row) {
  sum(row == "Y")
})

plot(bayPoly)
library(plotrix)
points(bayFMs$x, bayFMs$y, cex = 1, pch = 20,
       col = color.scale(bayFMs$qualityScore, extremes=c("yellow","red")))

plot(bayPoly)
symbols(bayFMs$x, bayFMs$y, cex = 2, pch = 20, circles = bayFMs$qualityScore,
        inches=1/20, ann=F, bg="steelblue2", fg=NULL, add=TRUE)

populationByZCTA <- read.csv(paste0("http://s3.amazonaws.com/Splitwi",
                                    "seBlogJB/2010+Census+Population+",
                                    "By+Zipcode+(ZCTA).csv"))
names(populationByZCTA)[1] <- "ZCTA5CE10"
# Creates a new SpatialPolygonsDataFrame
# sort = FALSE as we need the polygons to match with their data
bayPoly@data <- merge(bayPoly@data, populationByZCTA,
                      all.x = TRUE, sort=FALSE)
bayPoly$popDens <- bayPoly$X2010.Census.Population /
  bayPoly@data$ALAND10

library(ggplot2)
# Adds a unique id in case it's not there already
bayPoly@data$id <- rownames(bayPoly@data)
bayPolypoints <- fortify(bayPoly, region = "id")
library(plyr) # plyr::join is a bit faster than merge
bayPolyDf <- join(bayPolypoints, bayPoly@data, by = "id")

mapPlot <- ggplot(bayPolyDf, aes(long, lat)) +
  geom_path(aes(group=group)) + theme_minimal()
library(RColorBrewer)
mapPlot <- mapPlot + geom_polygon(aes(fill = popDens, group=group)) +
  scale_fill_gradientn(colours = brewer.pal(5, "Reds")) + geom_path(aes(group=group))
mapPlot

# mapPlot + geom_point(data = bayFMs, aes(x = x, y = y, group = NULL,
#                                         color = qualityScore), size = 2) +
#   scale_colour_gradientn(colours = brewer.pal(5, "Blues"))

mapPlot + geom_point(data = bayFMs, aes(x = x, y = y, group = NULL,
                                        size = qualityScore / 2))
# Google how to change average size of dots in ggplot2

library(ggmap)
ggMap <- ggmap(get_map(location = c(-122.363028, 37.822109),
                       zoom = 11, scale = 4, maptype = "roadmap"))
ggMap <- ggMap +
  geom_polygon(data = bayPolyDf, aes(long, lat, fill = popDens, group=group), alpha = 0.5) +
  scale_fill_gradientn(colours = brewer.pal(5, "Reds")) +
  geom_path(data = bayPolyDf, aes(long, lat, group=group))
ggMap + geom_point(data = bayFMs, aes(x = x, y = y, group = NULL,
                                      color = qualityScore), size = 4) +
  scale_colour_gradientn(colours = brewer.pal(5, "Blues"))





