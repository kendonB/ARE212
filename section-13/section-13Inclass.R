
## ----echo=FALSE----------------------------------------------------------
opts_chunk$set(message=FALSE)


## ------------------------------------------------------------------------
library(XML)
library(RCurl)
library(stringr)
library(ggplot2)


options(show.error.messages = TRUE)


## ----cache=TRUE----------------------------------------------------------
token <- "characters"
nameslist <- list()

while (is.character(token) == TRUE) {
  baseurl <- "http://oai.crossref.org/OAIHandler?verb=ListSets"
  if (token == "characters") {
    tok.follow <- NULL
  } else {
    tok.follow <- paste("&resumptionToken=", token, sep = "")
  }

  query <- paste(baseurl, tok.follow, sep = "")

  xml.query <- xmlParse(getURL(query))
  set.res <- xmlToList(xml.query)
  names <- as.character(sapply(set.res[["ListSets"]], function(x) x[["setName"]]))
  nameslist[[token]] <- names

  tryCatch(token <- set.res[["request"]][[".attrs"]][["resumptionToken"]], 
           error = function(e){
             message(e) # Original error message here
             # cat() # You should never use cat()
             message("no more data")
             token <<- NULL
           })
}


## ------------------------------------------------------------------------
allnames <- unlist(nameslist)
length(allnames)
head(allnames)


## ------------------------------------------------------------------------
econtitles <- as.character(allnames[str_detect(allnames, 
                                               "^[Ee]con|\\s[Ee]con")])
length(econtitles)
head(econtitles)


## ------------------------------------------------------------------------
sample(econtitles, 10)


## ------------------------------------------------------------------------
url <- "http://www.cmegroup.com/trading/energy/crude-oil/brent-crude-oil.html"
tables <- tryCatch(readHTMLTable(url), error = function(e){
  message("Please check internet connection")
})
n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
oilPrices <- tables[[which.max(n.rows)]]
head(oilPrices)
# Could be better ways to do this - maybe try Quandl package?



