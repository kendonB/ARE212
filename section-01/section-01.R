
## ----installForeign, echo=TRUE, message=FALSE, eval=FALSE----------------
## install.packages("foreign")
## library(foreign)

## ----loadForeign, echo=FALSE, message=FALSE------------------------------
library(foreign)


## ----getAuto, echo=TRUE--------------------------------------------------
mydata <- read.dta("auto.dta")


## ----names, echo=TRUE----------------------------------------------------
names(mydata)


## ----names2, echo=TRUE---------------------------------------------------
names(mydata) <- c("price", "mpg", "weight")


## ----head, echo=TRUE-----------------------------------------------------
head(mydata)


## ----headMpg, echo=TRUE--------------------------------------------------
head(mydata$mpg)


## ----installPackage, echo=TRUE, eval=FALSE-------------------------------
## inst


## ----installPackageComplete, echo=TRUE, eval=FALSE-----------------------
## install.packages


## ----insideFun, echo=TRUE, eval=FALSE------------------------------------
## install.packages(


## ----insideFun2, echo=TRUE, eval=FALSE-----------------------------------
## install.packages(pkgs = "foreign")


## ----dta, echo=TRUE, eval=FALSE------------------------------------------
## read.dta("


## ----dta2, echo=TRUE, eval=FALSE-----------------------------------------
## read.dta("../


