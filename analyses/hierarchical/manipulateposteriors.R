## Started 27 January 2025 ##
## By Lizzie ##
## Example problem to try manipulating posteriors ##

rm(list=ls()) 
options(stringsAsFactors = FALSE)

# libraries
library(rstan)
library(rstanarm)
options(mc.cores = parallel::detectCores()) # use all my cores, go for it!

if(length(grep("lizzie", getwd()) > 0)) {
  setwd("~/Documents/git/teaching/hotstats/hotstatsbayes/analyses/hierarchical")
} else{
  setwd("/home/boomdittyboom")
}

# doy is day of year of the various phenological events
d <- read.csv("..//input/loughnan2024/phenologyData.csv")
covariates1 <- read.csv("..//input/loughnan2024/lifeHistoryData.csv")

# this should merge better! But this is enough for our example.
dcov <- merge(d, covariates1, by.x=c("species", "phenophase"), 
  by.y=c("species.name", "phenophase"), all.x=TRUE)

# To be safe, adjust the year -- here relative to 1900
dcov$yearadj <- dcov$year-1900

## Goal: estimate the posterior for primary producer vs. primary consumer species...
## Aim to plot it; bonus estimate the 10% and 90% quantiles for each group of species. 

# Code snippets you might want ... 
if(FALSE){
  arm1sp <- stan_lmer(doy~yearadj|species, data=dcov)
  # grab Stan output rstanarm
  head(summary(arm1sp))
  arm1sppost <- as.data.frame(arm1sp) # full posterior
  primaryprod <- subset(dcov, trophic.level=="primary producer")
  ppspp <- unique(primaryprod$species)
  primarycons <- subset(dcov, trophic.level=="primary consumer")
  pcspp <- unique(primarycons$species)
}

