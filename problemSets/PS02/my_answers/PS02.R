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


load(url("https://github.com/ASDS-TCD/StatsII_2026/blob/main/datasets/climateSupport.RData?raw=true"))
head(climateSupport)
str(climateSupport)
climateSupport$choice_bin <- ifelse(climateSupport$choice == "Supported", 1, 0)
model <- glm(choice_bin ~ countries + sanctions, 
             data = climateSupport, 
             family = binomial)
summary(model)
anova(model, test = "Chisq")
# The number of countries is a strong predictor of policy support. 
# The Chi-Square value indicates a strong effect of the number of countries 
# and policy support. The p value of less than 0.001 is far smaller than 0.05 and 
# there is strong evidence against the null hypothesis. The null hypothesis is rejected that the number of countries has no effect. 
# The level of sanctions also is a strong predictor of policy support. 
# The Chi-square value indicates a strong effect on the level of sanctions and policy support (68)
# The p value of less than 0.001 is far smaller than 0.05 and there is evidence against 
# the null hypothesis. The null hypothesis is rejected that higher sanctions are not associated with higher policy support. 


#A
model <- glm(choice ~ countries + sanctions,
             data = climateSupport,
             family = binomial)
summary(model)
newdata_5 <- data.frame(
  countries = factor("160 of 192", levels = levels(climateSupport$countries)),
  sanctions = factor("5%", levels = levels(climateSupport$sanctions))
)

newdata_15 <- data.frame(
  countries = factor("160 of 192", levels = levels(climateSupport$countries)),
  sanctions = factor("15%", levels = levels(climateSupport$sanctions))
)
log_odds_5 <- predict(model, newdata_5, type = "link")
log_odds_15 <- predict(model, newdata_15, type = "link")
odds_ratio <- exp(log_odds_15 - log_odds_5)
odds_ratio
# For a policy with 160 participating countries, increasing sanctions from 5% to 15% reduces the odds of supporting the policy by about 28%, holding the number of countries constant.

#B
newdata_5_b <- data.frame(
  countries = factor("20 of 192", levels = levels(climateSupport$countries)),
  sanctions = factor("5%", levels = levels(climateSupport$sanctions))
)

newdata_15_b <- data.frame(
  countries = factor("20 of 192", levels = levels(climateSupport$countries)),
  sanctions = factor("15%", levels = levels(climateSupport$sanctions))
)
log_odds_5_b <- predict(model, newdata_5_b, type = "link")
log_odds_15_b <- predict(model, newdata_15_b, type = "link")
odds_ratio_b <- exp(log_odds_15_b - log_odds_5_b)
odds_ratio_b
log_odds_5_b <- predict(model, newdata_5_b, type = "link")
log_odds_15_b <- predict(model, newdata_15_b, type = "link")
odds_ratio_b <- exp(log_odds_15_b - log_odds_5_b)
odds_ratio_b
# For a policy with 20 participating countries, increasing sanctions from 5% to 15% reduces the odds of supporting the policy by about 28%, holding the number of countries constant.
#C
newdata <- data.frame(
  countries = "80 of 192",
  sanctions = "None"
)
predicted_prob <- predict(model, newdata, type = "response")
predicted_prob
#Given 80 countries participate and there are no sanctions, the model predicts that an individual has approximately a 52.5% chance of supporting the policy.
#3
model_additive <- glm(choice ~ countries + sanctions, 
                      data = climateSupport, family = binomial)
model_interaction <- glm(choice ~ countries * sanctions, 
                         data = climateSupport, family = binomial)
anova(model_additive, model_interaction, test = "Chisq")
# The likelihood ratio test showed that the interaction between countries and sanctions was not significant (p = 0.786), indicating that the effect of sanctions is roughly the same regardless of the number of countries. Therefore, the answers to 2a and 2b do not change in our dataset.
