setwd("../ARE212GSI/Github/ARE212/section-12/")
farmersMarkets <- read.csv("farmers-mkts.csv")
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
             "94133", "94134", "94158")

bayFMs <- farmersMarkets[farmersMarkets$Zip %in% bayZips,]

bayFMs$varietyScore <- apply(bayFMs, 1, function(row) {
  sum(row == "Y")
})

library(sp)
library(rgdal)
tmpFile <- tempfile()
download.file("http://www2.census.gov/geo/tiger/GENZ2013/cb_2013_us_zcta510_500k.zip", 
              destfile = tmpFile)
unzipped <- unzip(zipfile = tmpFile, exdir = getwd())
zipPoly <- readOGR(dsn = getwd(), layer = "cb_2013_us_zcta510_500k")

# Remove all the files on disk
file.remove(tmpFile)
file.remove(unzipped)

bayPoly <- zipPoly[zipPoly$ZCTA5CE10 %in% bayZips,]
plot(bayPoly)

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

plot(bayPoly)
# pch = 20 is a filled dot, cex is dot size
points(bayFMs$x, bayFMs$y, cex = 1, pch = 20)

plot(bayPoly)
library(plotrix)
points(bayFMs$x, bayFMs$y, cex = 2, pch = 20, 
       col = color.scale(bayFMs$varietyScore, extremes=c("yellow","red")))

plot(bayPoly)
symbols(bayFMs$x, bayFMs$y, cex = 2, pch = 20, circles = bayFMs$varietyScore,
        inches=1/10, ann=F, bg="steelblue2", fg=NULL, add=TRUE)

library(ggplot2)

# Adds a unique id in case it's not there already
bayPoly@data$id <- rownames(bayPoly@data)
# Turns the SpatialPolygons object into a data.frame - slow
# simplify with gSimplify if this takes forever. Or consider
# subsetting the polygon to what you actually want to plot.
bayPolypoints <- fortify(bayPoly, region = "id")
library(plyr) # plyr::join is a bit faster than merge
bayPolyDf <- join(bayPolypoints, bayPoly@data, by = "id")

library(RColorBrewer)
mapPlot <- ggplot(bayPolyDf, aes(long, lat)) + 
  geom_path(aes(group=group)) + theme_minimal()
mapPlot <- mapPlot + geom_polygon(aes(fill = popDens, group=group)) + 
  scale_fill_gradientn(colours = brewer.pal(5, "Blues")) + geom_path(aes(group=group))

mapPlot + geom_point(data = bayFMs, aes(x = x, y = y, group = NULL, 
                                        color = varietyScore), size = 4) +
  scale_colour_gradient(low = "white", high = "red")

mapPlot + geom_point(data = bayFMs, aes(x = x, y = y, group = NULL, 
                                        size = varietyScore))

library(ggmap)
ggMap <- ggmap(get_map(location = c(-122.363028, 37.822109), 
                       zoom = 11, scale = 4, maptype = "roadmap"))
ggMap <- ggMap + 
  geom_polygon(data = bayPolyDf, aes(long, lat, fill = popDens, group=group), alpha = 0.5) + 
  scale_fill_gradientn(colours = brewer.pal(5, "Blues")) +
  geom_path(data = bayPolyDf, aes(long, lat, group=group)) 

ggMap + geom_point(data = bayFMs, aes(x = x, y = y, group = NULL, 
                                        color = varietyScore), size = 4) +
  scale_colour_gradient(low = "white", high = "red")
