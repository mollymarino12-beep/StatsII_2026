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
gdp_data$GDPWdiff_cat <- ifelse(gdp_data$GDPWdiff < 0, "negative",
                                ifelse(gdp_data$GDPWdiff > 0, "positive",
                                       "no_change"))
gdp_data$GDPWdiff_cat <- factor(gdp_data$GDPWdiff_cat,
                                levels = c("negative","no_change","positive"))
table(gdp_data$GDPWdiff_cat)
gdp_data$GDPWdiff_cat <- relevel(gdp_data$GDPWdiff_cat, ref = "no_change")
multinom_model <- multinom(GDPWdiff_cat ~ REG + OIL, data = gdp_data)
summary(multinom_model)
#The unordered multinomial logit model estimates the effect of regime type (REG) and oil dependence (OIL) on the likelihood of experiencing negative or positive GDP change relative to the reference category of no change. 
#The coefficient for REG is positive for both negative (1.38) and positive (1.77) GDP changes, suggesting that democracies have higher log-odds of experiencing both negative and positive GDP changes relative to no change compared to non-democracies. 
#The estimated standard errors for REG (0.77 for both outcomes) indicate moderate uncertainty around these estimates. The coefficient for OIL is also positive for both negative (4.78) and positive (4.58) GDP changes,
#implying that oil-exporting countries are more likely to experience changes in GDP. However, the standard errors for OIL are very large, suggesting that these estimates are imprecise. 
#Question 1b ordered logit 
gdp_data$GDPWdiff_ord <- factor(gdp_data$GDPWdiff_cat,
                                levels = c("negative","no_change","positive"),
                                ordered = TRUE)
ordered_model <- polr(GDPWdiff_ord ~ REG + OIL, data = gdp_data, Hess = TRUE)
summary(ordered_model)
# The ordered logit model estimates the effect of regime type (REG) and oil dependence (OIL) 
# on the likelihood of experiencing higher GDP changes, with the outcome ordered from 
# negative to no change to positive. 

# The coefficient for REG is 0.3985 (standard error = 0.075), indicating that democracies 
# are more likely than non-democracies to experience higher GDP changes, moving from 
# negative to no change or from no change to positive. 

# The coefficient for OIL is -0.1987 (standard error = 0.116), suggesting that 
# oil-exporting countries may be slightly less likely to experience higher GDP changes. 

#Question 2
#1a
# Load Mexico municipal data
mexico_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_2026/main/datasets/MexicoMuniData.csv")
Poisson_model <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06,
                    family = poisson(link = "log"),
                    data = mexico_data)
summary(Poisson_model)
# The Poisson regression examines the number of visits by the winning PAN presidential candidate in 2006. 
# There is no evidence that swing districts received more visits than safe districts (competitive.district coefficient = -0.081, z = -0.477, p = 0.634). 
# Part B
# marginality.06 (poverty): Higher poverty is associated with fewer visits by the PAN candidate.
# In other words, poorer districts tend to receive less attention.
# PAN.governor.06: Districts in states with a PAN governor tend to get slightly fewer visits
# (coefficient = -0.312), though the effect is only marginally significant (p ≈ 0.062).
#part c 
newdata <- data.frame(
  competitive.district = 1,  
  marginality.06 = 0,        
  PAN.governor.06 = 1        
)
predicted_visits <- predict(Poisson_model, newdata = newdata, type = "response")
predicted_visits
#For a competitive district with average poverty and a PAN governor, the predicted mean number of PAN visits is essentially zero (≈0.015). This reflects that most districts received very few visits
