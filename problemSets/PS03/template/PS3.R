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

lapply(c("nnet", "MASS"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data
gdp_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_2026/main/datasets/gdpChange.csv", stringsAsFactors = F)

#####################
# Problem 2
#####################

# load data
mexico_elections <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_2026/main/datasets/MexicoMuniData.csv")
gdp_data$GDPWdiff <- factor(ifelse(gdp_data$GDPWdiff == 1, "positive",
                                   ifelse(gdp_data$GDPWdiff == 0, "no change",
                                          "negative")),
                            levels = c("no change", "positive", "negative"))

table(gdp_data$GDPWdiff)
# I needed to convert the numbers in GDP w/diff to a factor to convert the numbers to the three different categories. 
#Question 1a
multinom_model <- multinom(GDPWdiff ~ REG + OIL, data = gdp_data)
# This model calculates the log odds of being in each GDP cateogry 
# relative to the reference category of "no change" as a function of 
# REG and OIL. 

summary(multinom_model)
# The result of 0.85 means that being a democracy increases the log-odds 
# of postive GDP relative to no change. The 1.63 means that being a democracy
# increases the log-odds of negative GDP growth relative to no change. Democracy can 
# move GDP either positively or negatively. 7.86 means that being an oil exporter 
# increased the log odds of GDP relative to no change. 7.04 means that being an oil exporter 
# also increased the log odds of negative GDP relative to no change. The SE indicate 
# that oil is not a reliable predictor while democracy is a more reliable predictor. 

#Question 1b
library(MASS)

gdp_data$GDPWdiff_ord <- ordered(gdp_data$GDPWdiff,
                                 levels = c("negative", "no change", "positive"))

ordered_model <- polr(GDPWdiff_ord ~ REG + OIL, data = gdp_data, Hess=TRUE)
summary(ordered_model)
# Created an ordered logit for GDP changes 
# 
