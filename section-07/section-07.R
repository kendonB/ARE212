
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


## ----echo=TRUE, eval = FALSE, tidy=FALSE---------------------------------
## library(ggvis)
## graphData[1:B,] %>% ggvis(x = ~beta) %>%
##   layer_densities(
##     adjust = input_slider(.1, 2, value = 1,
##                           step = .1,
##                           label = "Bandwidth adjustment"),
##     kernel = input_select(
##     c("Gaussian" = "gaussian",
##       "Epanechnikov" = "epanechnikov",
##       "Rectangular" = "rectangular",
##       "Triangular" = "triangular",
##       "Biweight" = "biweight",
##       "Cosine" = "cosine",
##       "Optcosine" = "optcosine"),
##     label = "Kernel")
## )


