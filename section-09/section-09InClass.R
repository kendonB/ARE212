data(iris)
# For character and factors
dummiesManual <- sapply(unique(iris$Species), function(x){
  as.numeric(iris$Species == x)
})
dummies <- model.matrix(~Species - 1, data = iris)
all(dummies == dummiesManual)
all.equal(c(dummies), c(dummiesManual))

# numeric
quartiles <- quantile(iris$Sepal.Length)
sapply(1:(length(quartiles) - 1), function(i){
  if (i == 1) {
    ifelse(test = iris$Sepal.Length <= quartiles[i + 1], yes = 1, no = 0)
  } else if (i <= length(quartiles) - 2) {
    ifelse(iris$Sepal.Length > quartiles[i] & 
             iris$Sepal.Length <= quartiles[i + 1], yes = 1, no = 0)
  } else {
    ifelse(iris$Sepal.Length > quartiles[i], 1, 0)
  }
}
# better
dummiesManualNumeric <- sapply(1:(length(quartiles) - 1), function(i){
  if (i == 1) {
    ifelse(test = iris$Sepal.Length <= quartiles[i + 1], yes = 1, no = 0)
  } else {
    ifelse(iris$Sepal.Length > quartiles[i] & 
             iris$Sepal.Length <= quartiles[i + 1], yes = 1, no = 0)
  }
})
# A test 
all(rowSums(dummiesManual) == 1)

# Canned
quartiles <- quantile(x = iris$Sepal.Length)
# Generates a factor - function requires unique breaks
cuts <- cut(x = iris$Sepal.Length, breaks = unique(quartiles), include.lowest = TRUE)
dummiesNumeric <- model.matrix(~ cuts - 1)
all(dummiesNumeric == dummiesManualNumeric)
