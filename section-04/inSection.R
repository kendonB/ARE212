olsRSq(y = mydata$V1, X = cbind(1, mydata$V2))
"../"
lmObj <- lm(V1 ~ V2, data = mydata)
summLm <- summary(lmObj)
summLm$r.squared
