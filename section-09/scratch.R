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
levels(oct12.df$q1f) <- levels(nov11.df$q1f)
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

# now suppose we want to include some covariates in our regression
# how about per capita income by state-year?
# this was originally an XLS file, and there are ways to load these, but they
# often require external dependencies like Perl or Java and I don't want to waste 
# class time for downloading
# so i convert to csv and party on wayne

# TODO - installing fucking EXCEL so i can convert the xls i have to csv... in meantime
# build up tutorial to this point.