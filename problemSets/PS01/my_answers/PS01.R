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
ks_normal_test <- function(data) {
  n <- length(data) 
ECDF <- ecdf(data) 
empiricalCDF <- ECDF(data)  
D <- max(abs(empiricalCDF - pnorm(data))) 
k <- 1:100
p_value <- 2 * sum((-1)^(k - 1) * exp(-2 * k^2 * n * D^2)) 
p_value <- max(min(p_value, 1), 0)
return(list(
  D = D,
  p.value = p_value))}
set.seed(123)
data <- rcauchy(1000, location = 0, scale = 1)
result <- ks_normal_test(data)
result
 

#####################
# Problem 2
#####################
set.seed (123)
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)
lm_fit <- lm(y ~ x - 1, data = data) 
coef(lm_fit)
ols_objective <- function(beta, y, x) { 
  sum((y - beta * x)^2)}
bfgs_fit <- optim(
  par = 0,                    
  fn = ols_objective,
  y = data$y,
  x = data$x,
  method = "BFGS")
bfgs_fit$par
c(
  lm_estimate   = coef(lm_fit),
  bfgs_estimate = bfgs_fit$par) 
max(abs(
fitted(lm_fit) - data$x * bfgs_fit$par))  


