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

# load data
load(url("https://github.com/ASDS-TCD/StatsII_2026/blob/main/datasets/climateSupport.RData?raw=true"))
head(climateSupport)
str(climateSupport)
climateSupport$choice_bin <- ifelse(climateSupport$choice == "Supported", 1, 0)
climateSupport$countries <- factor(climateSupport$countries, 
                                   levels = c("20 of 192", "80 of 192", "160 of 192"))
climateSupport$sanctions <- factor(climateSupport$sanctions, 
                                   levels = c("None", "5%", "15%", "20%"))
model <- glm(choice_bin ~ countries + sanctions, 
             data = climateSupport, 
             family = binomial)
summary(model)
anova(model, test = "Chisq")

# 1️⃣ Fit additive logistic regression model
model <- glm(choice ~ countries + sanctions,
             data = climateSupport,
             family = binomial)

# 2️⃣ View model summary
summary(model)

# 3️⃣ Compute odds ratio for increasing sanctions 5% → 15%
# First, extract coefficients
coefs <- coef(model)

# Difference in log-odds between 15% and 5% sanctions
log_odds_diff <- coefs["sanctions15%"] - coefs["sanctions5%"]

# Convert to odds ratio
odds_ratio <- exp(log_odds_diff)
odds_ratio

# 4️⃣ Predicted probability for 80 of 192 countries with no sanctions
newdata <- data.frame(
  countries = "80 of 192",
  sanctions = "None"
)

predicted_prob <- predict(model, newdata, type = "response")
predicted_prob


