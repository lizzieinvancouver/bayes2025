## Started 22 January 2025 ##
## Snippet from hierarchicalday.R ##

rm(list=ls()) 
options(stringsAsFactors = FALSE)

# libraries
library(rstan)
options(mc.cores = parallel::detectCores()) # use all my cores, go for it!

if(length(grep("lizzie", getwd()) > 0)) {
  setwd("~/Documents/git/teaching/hotstats/hotstatsbayes/analyses/hierarchical")
} else{
  setwd("/home/boomdittyboom")
}

## Build your test data here!


## Fit Stan model to your test data 

fit <- stan("..//stan/twolevelhierslopeint.stan", data=c("N","y","Nspp","species","year"), iter=1000, chains=4, seed=377)

# grep stan output
sumer <- summary(fit)$summary
muparams <- sumer[grep("mu", rownames(sumer)), c("mean", "2.5%", "25%", "50%", "75%", "97.5%")]
sigmaparams <- sumer[grep("sigma", rownames(sumer)), c("mean", "2.5%","25%", "50%", "75%", "97.5%")]

# compare given versus modeled
paramsgiven # here's the parameters we set
muparams # estimated mu parameters
sigmaparams # estimate sigma parameters

spslopes <- sumer[grep("b\\[", rownames(sumer)), "mean"]

plot(spslopes~slopespp, xlab="Given species-level slopes", ylab="Modeled species-level slopes", col="darkblue")
abline(0,1)
