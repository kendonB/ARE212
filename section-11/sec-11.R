rm(list = ls())
library(XML)
library(RCurl)
library(stringr)

options(show.error.messages = FALSE)

token <- "characters"
nameslist <- list()
i <- 1
time <- proc.time()
while (is.character(token) == TRUE & i < 100000) {
  baseurl <- "http://oai.crossref.org/OAIHandler?verb=ListSets"
  if (token == "characters") {
    tok.follow <- NULL
  } else {
    tok.follow <- paste("&resumptionToken=", token, sep = "")
  }

  query <- paste(baseurl, tok.follow, sep = "")

  xml.query <- xmlParse(getURL(query))
  xml.query
  set.res <- xmlToList(xml.query)
  set.res
  names <- as.character(sapply(set.res[["ListSets"]], function(x) x[["setName"]]))
  names
  nameslist[[token]] <- names

  if (class(try(set.res[["request"]][[".attrs"]][["resumptionToken"]])) == "try-error") {
    stop("no more data")
  }
  else {
    token <- set.res[["request"]][[".attrs"]][["resumptionToken"]]
  }
  i <- i + 1
}
(proc.time() - time)

allnames <- do.call(c, nameslist)
length(allnames)
head(allnames)

econtitles <- allnames[str_detect(allnames, "^[Ee]conom|\\s[Ee]conom")]
econtitles2 <- allnames[str_detect(allnames, "[Ee]conomic|\\s[Ee]conomic")]
length(econtitles)
length(econtitles2)

sample(econtitles, 10)

countJournals <- function(regex) {
  titles <- allnames[str_detect(allnames, regex)]
  return(length(titles))
}

subj = c("economic", "business", "politic", "environment", "engineer", "history")
regx = c("^[Ee]conomic|\\s[Ee]conomic", "^[Bb]usiness|\\s[Bb]usiness",
  "^[Pp]olitic|\\s[Pp]olitic", "^[Ee]nvironment|\\s[Ee]nvironment",
  "^[Ee]ngineer|\\s[Ee]ngineer", "^[Hh]istory|\\s[Hh]istory")

subj.df <- data.frame(subject = subj, regex = regx)

subj.df[["count"]] <- sapply(as.character(subj.df[["regex"]]), countJournals)
library(ggplot2)
(g <- ggplot(data = subj.df, aes(x = subject, y = count)) + geom_bar(stat = "identity"))
