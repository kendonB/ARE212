
rm(list = ls())
oct12.df <- read.table("EarlyOct12 public.csv", header = T, sep = ",")
head(names(oct12.df))

oct12.df <- oct12.df[c("int_date","state","q61")]
head(oct12.df, n = 1)

oct12.df$q1f <- factor(oct12.df$q61)

levels(oct12.df$q1f)

badindices <- which(oct12.df$q61 == c("(VOL.) Mixd/some evidence"))
oct12.df$q61[badindices] <- "(VOL.) Mixed/some evidence"

oct12.df$q1f <- factor(oct12.df$q61)
levels(oct12.df$q1f)

nov11.df <- read.table("Nov11 public.csv", header = T, sep = ",")

nov11.df <- nov11.df[c("int_date","state","q65")]
nov11.df$q1f <- factor(nov11.df$q65)

all(levels(nov11.df$q1f) == levels(oct12.df$q1f))
levels(nov11.df$q1f)
levels(oct12.df$q1f)

levels(nov11.df$q1f) <- levels(oct12.df$q1f) <- c("Don't know","Mixed","No","Yes")

data.df <- rbind(nov11.df, oct12.df)

data.df <- rbind(nov11.df[, c(1, 2, 4)], oct12.df[, c(1, 2, 4)])
head(data.df, n = 3)

class(data.df$int_date)

data.df$int_date.str <- as.character(data.df$int_date)
data.df$int_date.date <- as.Date(data.df$int_date.str, format = "%y%d%m")
class(data.df$int_date.date)

dow <- weekdays(data.df$int_date.date)
head(dow)

data.df$int_date.str <- data.df$int_date <- NULL
names(data.df)[names(data.df) == "int_date.date"] <- "int.date"
data.df <- data.df[c(3, 1, 2)]
head(data.df)

oilgas.df <- read.table("oilgas_emp.csv", header = T, sep = ",")
head(oilgas.df)

oilgas.yearly <- aggregate(oilgas.df$N, by = list(oilgas.df$year, oilgas.df$state), FUN = mean)
names(oilgas.yearly) <- c("year", "state", "emp")

data.df$state <- toupper(data.df$state)

data.df$year <- as.numeric(format(data.df$int.date, "%Y"))

merged.df <- merge(x = data.df, y = oilgas.yearly, by = c("year", "state"), all.x = T)
head(merged.df)

png(filename="inserts/fig1.png",height=600,width=800)
library(ggplot2)
merged.df$q1f <- factor(merged.df$q1f, levels = c("Yes", "Mixed", "No", "Don't know"))
ggplot(merged.df, aes(x = state, fill = q1f)) + geom_bar(position = "fill") +
  coord_flip() + theme(axis.text.y=element_text(size=rel(0.8)))
dev.off()

merged.df$q1b <- ifelse(merged.df$q1f == "Yes", 1, 0)
bystate.df <- aggregate(cbind(merged.df$q1b,merged.df$emp),
  by = list(merged.df$year, merged.df$state), FUN = mean)
names(bystate.df) <- c("year","state","yes","emp")
tail(bystate.df)

bystate.df <- bystate.df[!is.na(bystate.df$emp), ]
tail(bystate.df)

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
(output <- OLS(y,X))
