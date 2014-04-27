##############################################################################
## FROM PATRICK'S SECTION 12 NOTES

#install.packages("maps","fields","maptools","RCurl","RJSONIO")
library(maps)
library(fields)
library(maptools)
library(RCurl)
library(RJSONIO)

#png(filename="inserts/us-mkts.png",height=400,width=800)
library(maps)
#setwd("/Users/edwardarubin/Dropbox/School/Berkeley/Spring 2014/ARE 212/Section")
setwd("R Files")
data <- read.csv("farmers-mkts.csv", header = TRUE)
# CREATE MAP
map("state", interior = FALSE)
# ADD TITLE
title("Farmers' markets")
# ADD BOUNDARIES
map("state", boundary = FALSE, col = "gray", add = TRUE)
# ADD THE FARMERS MARKETS
points(data$x, data$y, cex = 0.2, col = "blue")
#dev.off()

# CONSIDER ONLY NEW MEXCIO, COLORADO, ARIZONA, AND UTAH
statelist <- c("New Mexico", "Colorado", "Arizona", "Utah")
state.data <- data[is.element(data$State, statelist), ]
state.data <- state.data[state.data$x < -80,]
dim(state.data)
names(state.data)

# CREATE MAP WITH LIMITED FARMERS MARKETS
map("state", interior = FALSE)
# ADD TITLE
title("Farmers' markets")
# ADD BOUNDARIES
map("state", boundary = FALSE, col = "gray", add = TRUE)
# ADD THE FARMERS MARKETS
points(state.data$x, state.data$y, cex = 0.2, col = "blue")

geom <- cbind(state.data$x, state.data$y)
# DIST() CREATES DISTANCES (DEFAULT IS EUCLIDEAN DISTANCE)
euc.dist <- dist(geom)
head(euc.dist)
euc.dist.mtx <- as.matrix(euc.dist)
euc.dist.mtx[1:6, 1:6]
# NOTE: THE UNITS HERE ARE NOT REALLY INTERPRETABLE

# GREAT CIRCLE APPROXIMATION
library(fields)
gc.dist.mtx <- rdist.earth(geom)
gc.dist.mtx[1:6, 1:6]

nearestneighbor <- apply(gc.dist.mtx, 2, which.min)
head(nearestneighbor)

# HACKY WAY AROUND THE PROBLEM OF GETTING OWN MARKET AS NEAREST NEIGHBOR
n <- nrow(gc.dist.mtx)
nearestneighbor <- apply(gc.dist.mtx + diag(NA, n, n), 2, which.min)
head(nearestneighbor)
# OBSERVATION AND NEAREST NEIGHBOR
cbind(state.data[,1], state.data[nearestneighbor,1])

# GRABBING DUMMY VARIABLES
X <- state.data[, 8:ncol(state.data)]
X <- apply(X, 2, function(col) { ifelse(col == "Y", 1, 0) })
X[1:6, c("Honey", "Jams", "Poultry")]

# BINARY DISTANCE: # BITS THAT MATCH / TOTAL BITS
dum.dist <- dist(X, method = "binary")

# DENDROGRAM BY BINARY DISTANCE
#png(filename="inserts/dend.png",height=600,width=600)
hclust.res <- hclust(dum.dist)
plot(cut(as.dendrogram(hclust.res), h = 0)$upper, leaflab = "none")
#dev.off()

# CUT THE TREE INTO FIVE GROUPS (CLASSIFIES OBS. INTO 5 GROUPS)
cl <- cutree(hclust.res, k = 5)
head(cl)

# FUNCTION TO ASSIGN COLORS BASED UPON GROUP
assignColor <- function(cl.idx) {
  col.codes <- c("#FF8000", "#0080FF", "#FFBF00", "#FF4000", "#004000")
  return(col.codes[cl.idx])
}

#png(filename="inserts/zoom.png",height=600,width=600)
# LIMITING LONGITUDE AND LATITUDE AND PLOTTING STATES
map("state", interior = FALSE,
    xlim = c(-117, -101), ylim = c(28, 43))
map("state", boundary = FALSE, col="gray", add = TRUE,
    xlim = c(-117, -101), ylim = c(28, 43))
points(state.data$x, state.data$y, cex = 1, pch = 20, col = assignColor(cl))
#dev.off()

##############################################################################
## NEW CODE

# CREATE A COLUMN FOR THE MARKETS PRINCIPALLY FOUND IN NM - GROUP 2
my.state <- cbind(state.data, cl)
state.2  <- subset(my.state, cl == 2)

# PLOT
map("state", interior = FALSE,
    xlim = c(-117, -101), ylim = c(28, 43))
map("state", boundary = FALSE, col="gray", add = TRUE,
    xlim = c(-117, -101), ylim = c(28, 43))
points(state.2$x, state.2$y, cex = 1, pch = 20, col = "#0080FF")


# CREATE INDICATOR VARIABLE FOR GROUP OF INTEREST
cl.2 <- data.frame(Group.2 = (cl == 2) )
# JOIN GROUP-2 INDICATOR WITH DUMMY VARIABLES
the.data <- cbind(cl.2,X)

# A PLAIN OLS REGRESSION OF GROUP 2 STATUS ON THE DUMMIES
eqn <- as.formula(
	paste0('Group.2 ~ ', paste(colnames(the.data)[-1],collapse="+"))
		)
mod.ols <- lm( eqn, data = the.data)
summary(mod.ols)

# A LOGIT BERNOULLI MODEL
mod.logit <- glm( formula = eqn, data = the.data, 
	family = binomial(link = logit), maxit = 50)
# PERFECT SEPARATION?

# REGRESSION TREE
library(tree)
mod.tree <- tree(eqn, data = the.data)
#setwd("/Users/edwardarubin/Desktop")
#pdf("A Tree.pdf")
plot(mod.tree)
text(mod.tree,cex=0.75)
#dev.off()

# ADD INTERACTION DUMMY BETWEEN SFMNP AND NO BAKEDGOODS 
# PRESENT DATA HAVE 1 FOR BAKED GOODS; 
# NEW VARIABLE WILL BE 1 FOR NO BAKED GOODS
the.data$SFMNP.NoBakedgoods <- the.data$SFMNP * (the.data$Bakedgoods==0)

# SUMMARY STATISTICS
library(doBy)
# SUMMARY BY 'GROUP 2' VS. 'NOT GROUP 2'
summaryBy(SFMNP + Bakedgoods + SFMNP.NoBakedgoods ~ Group.2, data = the.data)
# SUMMARY BY STATE
the.data$State <- state.data$State
summaryBy(SFMNP + Bakedgoods + SFMNP.NoBakedgoods ~ State, data = the.data)











