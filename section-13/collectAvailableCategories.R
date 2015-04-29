library(dplyr)
library(RSelenium)
appURL <- ("http://quickstats.nass.usda.gov/")
RSelenium::checkForServer()
RSelenium::startServer()
remDr <- RSelenium::remoteDriver()
remDr$open()
remDr$navigate(appURL)

optionsDf <- data.frame(program=character(),
                        sector=character(),
                        group=character(),
                        commodity=character(),
                        category=character(),
                        dataitem=character(),
                        domain=character(),
                        geolevel=character(),
                        progNum=numeric(),
                        secNum=numeric(),
                        groupNum=numeric(),
                        commNum=numeric(),
                        statNum=numeric(),
                        dataItemNum=numeric(),
                        domNum=numeric())
clickOptionOn <- function(num, thisCategory){
  optElem <- remDr$findElement(using = 'xpath', paste0("//*[@id='", thisCategory, "']/option[",num,"]"))
  tries <- 0
  while (!optElem$isElementSelected()[[1]] & tries <= 10) {
    optElem$clickElement()
    # There's a bug where the optElem disappears
    # while this is in a loop and it gets unselected.
    Sys.sleep(1)
    optElem <- remDr$findElement(using = 'xpath', paste0("//*[@id='", thisCategory, "']/option[",num,"]"))
    tries <- tries + 1
  }
}

clickOptionOff <- function(num, thisCategory){
  optElem <- remDr$findElement(using = 'xpath', paste0("//*[@id='", thisCategory, "']/option[",num,"]"))
  tries <- 0
  while (optElem$isElementSelected()[[1]] & tries <= 10) {
    optElem$clickElement()
    tries <- tries + 1
  }
}

clickAndOptions <- function(num, thisCategory, nextCategory){
  if (num > 0) {
    # Tries to click twice
    tryCatch(clickOptionOn(num, thisCategory), error = function(e) {
      Sys.sleep(5)
      print("e")
      print(e)
      tryCatch(clickOptionOn(num, thisCategory), error = function(r) {
        print("r")
        print(r)
      })
    })
    Sys.sleep(8)
  }
  tryCatch(elem <- remDr$findElement("name", nextCategory), error = function(e){
    Sys.sleep(10)
    elem <<- remDr$findElement("name", nextCategory)
  })
  options <- strsplit(elem$getElementText()[[1]], split = "\n")[[1]]
  tries <- 0
  while (length(options) == 0 & tries <= 10) {
    options <- clickAndOptions(num, thisCategory, nextCategory)
    tries <- tries + 1
  }
  options
}

programOptions <- clickAndOptions(0, "", "source_desc")
# Will generate a branched list of options (i.e. a set of nested lists)
# Note that max(numeric()) == -Inf
# load(list.files("../../usdanass/", full.names = TRUE)[2])
if (file.exists(paste0(Sys.info()["nodename"],
                       "optionsDf.RData"))){
  load(file = paste0(Sys.info()["nodename"],
                                "optionsDf.RData"))
}
progStart <- max(1, tail(optionsDf$progNum, 1))
secStart <- max(1, tail(optionsDf$secNum, 1))
groupStart <- max(1, tail(optionsDf$groupNum, 1))
commStart <- max(1, tail(optionsDf$commNum, 1))
statStart <- max(1, tail(optionsDf$statNum, 1))
dataItemStart <- max(1, tail(optionsDf$dataItemNum, 1))
domStart <- max(1, tail(optionsDf$domNum, 1))
for(progNum in progStart:length(programOptions)){
  # Start by looking in Program
  progOption <- programOptions[progNum]
  sectorOptions <- clickAndOptions(progNum, "source_desc", "sector_desc")
  for (secNum in secStart:length(sectorOptions)) {
    secOption <- sectorOptions[secNum]
    groupOptions <- clickAndOptions(secNum, "sector_desc", "group_desc")
    for (groupNum in groupStart:length(groupOptions)) {
      groupOption <- groupOptions[groupNum]
      commOptions <- clickAndOptions(groupNum, "group_desc", "commodity_desc")
      for (commNum in commStart:length(commOptions)){
        commOption <- commOptions[commNum]
        statCatOptions <- clickAndOptions(commNum, "commodity_desc", "statisticcat_desc")
        for (statNum in statStart:length(statCatOptions)) {
          statCatOption <- statCatOptions[statNum]
          dataItemOptions <- clickAndOptions(statNum, "statisticcat_desc", "short_desc")
          for (dataItemNum in dataItemStart:length(dataItemOptions)) {
            dataItemOption <- dataItemOptions[dataItemNum]
            domOptions <- clickAndOptions(dataItemNum, "short_desc", "domain_desc")
            for (domNum in domStart:length(domOptions)) {
              domOption <- domOptions[domNum]
              geoOptions <- clickAndOptions(domNum, "domain_desc", "agg_level_desc")
              newDf <- data.frame(program=progOption,
                                  sector=secOption,
                                  group=groupOption,
                                  commodity=commOption,
                                  category=statCatOption,
                                  dataitem=dataItemOption,
                                  domain=domOption,
                                  geolevel=geoOptions,
                                  progNum=progNum,
                                  secNum=secNum,
                                  groupNum=groupNum,
                                  commNum=commNum,
                                  statNum=statNum,
                                  dataItemNum=dataItemNum,
                                  domNum=domNum,
                                  stringsAsFactors=FALSE)
              optionsDf <- rbind_all(list(optionsDf, newDf))
              clickOptionOff(domNum, "domain_desc")
              Sys.sleep(1)
            }
            domStart <- 1
            clickOptionOff(dataItemNum, "short_desc")
            Sys.sleep(1)
          }
          dataItemStart <- 1
          clickOptionOff(statNum, "statisticcat_desc")
          Sys.sleep(1)
          save(optionsDf, file = paste0(Sys.info()["nodename"],
                                   "optionsDf.RData"))
        }
        statStart <- 1
        clickOptionOff(commNum, "commodity_desc")
        Sys.sleep(1)
      }
      commStart <- 1
      clickOptionOff(groupNum, "group_desc")
      Sys.sleep(1)
    }
    groupStart <- 1
    clickOptionOff(secNum, "sector_desc")
    Sys.sleep(1)
  }
  secStart <- 1
  clickOptionOff(progNum, "source_desc")
  Sys.sleep(1)
}
