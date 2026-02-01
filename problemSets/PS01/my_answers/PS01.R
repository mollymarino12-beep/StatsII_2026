#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################
rm(data)
set.seed(123)
data <- rcauchy(1000, location = 0, scale = 1)
# This ensures data refers to numeric vector and not a function. 
# create empirical distribution of observed data
ks_normal_test <- function(data) {
  n <- length(data)
ECDF <- ecdf(data)
empiricalCDF <- ECDF(data)
# generate test statistic
D <- max(abs(empiricalCDF - pnorm(data)))
k <- 1:100
p_value <- 2 * sum((-1)^(k - 1) * exp(-2 * k^2 * n * D^2))

# ensure p-value is in [0, 1]
p_value <- max(min(p_value, 1), 0)

return(list(
  D = D,
  p.value = p_value
))
}
set.seed(123)
data <- rcauchy(1000, location = 0, scale = 1)

# perform KS test
result <- ks_normal_test(data)

result
# The value of 0.134 is fairly large and indicates that data does not 
# follow a normal distribution. The p value is below the significance level 
# and indicates that we can reject the null hypothesis that this data 
# comes from a normal distribution. 

#####################
# Problem 2
#####################

set.seed (123)
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)