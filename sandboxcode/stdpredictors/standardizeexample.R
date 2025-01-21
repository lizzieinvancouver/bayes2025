## Started 20 January 2025 ##
## Comparing models using non-standardized and standarized variable ##
## Mostly copied from hotstatsmisc/hotstats_before2025/examples/exampleday4/exampleday4.R ##

# We'll focus on a model of weight as a f(x) of height and age #

rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(rstanarm)

if(length(grep("lizzie", getwd()))>0) { 
setwd("~/Documents/git/teaching/hotstats/hotstatsmisc/bayesdatatsetsnmore/analyses/misc/diagnosticsday")
}

d <- read.delim("input/Howell1.csv", sep=";")

# Standardize our predictors
?scale
d$heightz <- scale(d$height, center=TRUE, scale=TRUE)
d$agez <- scale(d$age, center=TRUE, scale=TRUE)

# Run model with non-standardized and standarized variables
modnonstd <- stan_glm(weight~height+age, data=d)
modstd <- stan_glm(weight~heightz+agez, data=d)

# Compare output
coef(modnonstd) # given in natural units with the intercept at age and height=0
coef(modstd) # given in z-score units (per SD) with intercept at median

par(mfrow=c(2,2))
plot(weight~height, data=d, pch=16, ylim=c(-40, 70), xlim=c(-2, 190),
     xlab="Height in natural units", ylab="Weight in natural units")
abline(coef(modnonstd)[1], coef(modnonstd)[2])
plot(weight~age, data=d, pch=16, ylim=c(-40, 70), xlim=c(-2, 90),
     xlab="Age in natural units", ylab="Weight in natural units")
abline(coef(modnonstd)[1], coef(modnonstd)[3])
plot(weight~heightz, data=d, pch=16, 
     xlab="Height in standarized units", ylab="Weight in natural units")
abline(coef(modstd)[1], coef(modstd)[2])
plot(weight~agez, data=d, pch=16, 
     xlab="Age in standarized units", ylab="Weight in natural units")
abline(coef(modstd)[1], coef(modstd)[3])
