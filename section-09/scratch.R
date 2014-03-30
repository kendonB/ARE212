rm(list = ls())
setwd("C:/Users/pbaylis/Documents/GitHub/ARE212/section-09")

oct12.df <- read.table("EarlyOct12 public.csv", header = T, sep = ",")

oct12.df <- oct12.df[c("int_date","state","q61","q62","q63")]

# cast question answers as factors to save space
oct12.df$q1f <- factor(oct12.df$q61)
oct12.df$q2f <- factor(oct12.df$q62)
oct12.df$q3f <- factor(oct12.df$q63)

# see what the answers are 
levels(oct12.df$q1f)
levels(oct12.df$q2f)
levels(oct12.df$q3f)

# seems like there's a miscode (okay, I made it) in q61 - we want to fix this. best way is manually
# there are many manual ways to fix, but this gives us an opportunity to see the which command
badindex <- which(oct12.df$q61 == c("(VOL.) Mixd/some evidence"))
oct12.df$q61[badindex] <- "(VOL.) Mixed/some evidence"

# now recode as factor to make sure it worked
oct12.df$q1f <- factor(oct12.df$q61)
levels(oct12.df$q1f)

# now let's load the other data and format it the same way
nov11.df <- read.table("Nov11 public.csv", header = T, sep = ",")

# crap - the numbers changed. in this survey, numbers are different - q65, q66, q67
# good idea to keep the documentation on hand (and maybe show the relevant documentation?)
nov11.df <- nov11.df[c("int_date","state","q65", "q66", "q67")]
nov11.df$q1f <- factor(nov11.df$q65)
nov11.df$q2f <- factor(nov11.df$q66)
nov11.df$q3f <- factor(nov11.df$q67)

# see what the answers are 
all(levels(nov11.df$q1f) == levels(oct12.df$q1f))
levels(nov11.df$q1f)
levels(oct12.df$q1f)

# Crap. This is a dumb problem. Can we fix it easily? Yes.
levels(oct12.df$q1f) <- levels(nov11.df$q1f) <- c("Don't know","Mixed","No","Yes")
# hooray! we need to be careful, though - relies on these being in the right order

# now - let's put it together
data.df <- rbind(nov11.df, oct12.df)

# crap again. the names are wrong because of the different question numbers. so let's not include those.
data.df <- rbind(nov11.df[, c(1, 2, 6, 7, 8)], oct12.df[, c(1, 2, 6, 7, 8)])

# POOF! great - we've got a dataset. now - what do we do with these dates?
# down the road we might want to get creative.
# in R, dates are the number of days since 1970-01-01, with negative value sfor earlier dates.
# stata is 1960-01-01. i do not have a good explanation for why they are different

#right now it's an integer
class(data.df$int_date)

data.df$int_date.str <- as.character(data.df$int_date)
data.df$int_date.date <- as.Date(data.df$int_date.str, format = "%y%m%d")
  
# drop variable
data.df$int_date.str <- data.df$int_date <- NULL
  
# rename variable
names(data.df)[names(data.df) == "int_date.date"] <- "int.date"

# reorder variables
data.df <- data.df[c(5,1:4)]

# now we want to get oil and gas employment
# I cheat a bit here and ijust import the finished file
# the original was a bunch of excel sheets.
# imagine hours of work - and we have it. let's party on, wayne

# we don't want to get caught up in mtm variation, we just want the year
# so let's collapse

oilgas.df <- read.table("oilgas_emp.csv", header = T, sep = ",")
oilgas.yearly <- aggregate(oilgas.df$N, by = list(oilgas.df$year, oilgas.df$state), FUN = mean)
names(oilgas.yearly) <- c("year", "state", "emp")

#pary on, garth

# cool. let's merge this. 
# we want to merge on state and. first, observe that the state is in caps in the yearly dataset.
# easiest to fix in our data file
data.df$state <- toupper(data.df$state)

# also need to extract a year variable from our date. n 
# there are a few ways to do this. here is one.
data.df$year <- as.numeric(format(data.df$int.date, "%Y"))

# cool - let's merge this in!
# conveniently, we've named the variables identidically in both data frames so merging is easy.
# but we don't have to do this - stata does require

merged.df <- merge(data.df, oilgas.yearly, by = c("year", "state"), all.x = T)

library(ggplot2)
levels(merged.df$q1f)
merged.df$q1f <- factor(merged.df$q1f, levels = c("Yes", "Mixed", "No", "Don't know"))
ggplot(merged.df, aes(x = state, fill = q1f)) + geom_bar(position = "fill") + coord_flip() + theme(axis.text.y=element_text(size=rel(0.8))) 


# regressions

# code as 1 or 0
merged.df$q1b <- ifelse(merged.df$q1f == "Yes", 1, 0)

bystate.df <- aggregate(cbind(merged.df$q1b,merged.df$emp), by = list(merged.df$year, merged.df$state), FUN = mean)
names(bystate.df) <- c("year","state","yes","emp")
bystate.df <- bystate.df[!is.na(bystate.df$emp), ]

OLS <- function(y,X) {
  n <- nrow(X); k <- ncol(X)
  b <- solve(t(X) %*% X) %*% t(X) %*% y
  e <- y - X %*% b; s2 <- t(e) %*% e / (n - k); XpXinv <- solve(t(X) %*% X)
  se <- sqrt(s2 * diag(XpXinv))
  t <- b / se
  p <- 2 * pt(-abs(t),n-k)
  output <- data.frame(b, se, t, p)
  colnames(output) <- c("Estimate","Std. Error", "t statistic", "p-value")
  return(output)
}

y <- bystate.df$yes
X <- cbind(1, bystate.df$emp)

output <- OLS(y,X)
output
