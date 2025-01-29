## Started 28 January 2025 ##
## By Lizzie ##

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## set working directory if you need to
setwd("~/Documents/git/teaching/hotstats/hotstatsbayes/sandboxcode/retrochecks")

## flags
# You must set to true and RUN the models once for setting this to FALSE to work
runmodels <- FALSE

## libraries
library(rstan)
options(mc.cores = parallel::detectCores())


# get the data
rawlong.tot2 <- read.csv("input/rawlong.tot2.csv")

# Formatting for R stan (several ways to do this, this is one)
N <- nrow(rawlong.tot2)
y <- rawlong.tot2$phenovalue
Nspp <- length(unique(rawlong.tot2$species)) #newid is character !
species <- as.numeric(as.factor(rawlong.tot2$species))
year <- rawlong.tot2$yr1981

if(runmodels){
# See the stan code on this model for notes on what it does
syncmodelhis <- stan("..//stan/twolevelhierslopeint.stan", data=c("N","Nspp","y","species","year"),
                   iter=4000, warmup=3000, chains=4, cores=4)
save(syncmodelhis, file="output/syncmodelhis.Rdata")
}

if(!runmodels){
load("output/syncmodelhis.Rdata")
}

################################################
## Retrodictive (posterior predictive) checks ##
################################################

Nreal <- nrow(rawlong.tot2)
yreal <- rawlong.tot2$phenovalue


# First, plot the real data used in the model
# pdf("graphs/realdata_formodel.pdf", height=4, width=6)
par(mar=c(3,3,1,1), mgp=c(1.5,.5,0), tck=-.01)
plot(range(year), range(yreal), type="n", xlab="Year",
     ylab="Day of year", bty="l", main="Raw real data")
for (j in 1:Nspp){
  lines(year[species==j], yreal[species==j])
}
hist(yreal, xlab="Day of year", main="Real data")
# dev.off()

# GOAL: 
# A similar plot but using the model output from one iteration?
# Here's the full posterior to get you started....
syncmodelhispost <- extract(syncmodelhis) 
str(syncmodelhispost)
