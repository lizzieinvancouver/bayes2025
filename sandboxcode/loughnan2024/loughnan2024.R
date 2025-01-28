## Started 25 January 2025 ##
## By Lizzie ##
## Working on examples to manipulate posteriors ##

rm(list=ls()) 
options(stringsAsFactors = FALSE)

# libraries
library(rstan)
library(rstanarm)
options(mc.cores = parallel::detectCores()) # use all my cores, go for it!

if(length(grep("lizzie", getwd()) > 0)) {
  setwd("~/Documents/git/teaching/hotstats/hotstatsbayes/sandboxcode/loughnan2024")
} else{
  setwd("/home/boomdittyboom")
}

## Set flags 
runmodels <- FALSE # set to TRUE if you want to run the models

# get the data so we can think through simulation
d1 <- read.csv("input/Kharouba_et_al_2018_spp_phenodata.csv")
d <- read.csv("input/phenologyData.csv")
covariates1 <- read.csv("input/lifeHistoryData.csv")

dcov <- merge(d, covariates1, by.x=c("species", "phenophase"), 
  by.y=c("species.name", "phenophase"), all.x=TRUE)

dcov$yearadj <- dcov$year-1900

## RStanarm (arm)
if(runmodels){
  arm1sp <- stan_lmer(doy~yearadj|species, data=dcov)
  save(arm1sp, file="..//output/arm1sp.Rdata")
}

if(!runmodels){
  load("..//output/arm1sp.Rdata")
}

# Should we add study ID?
dcovspstudy <- aggregate(dcov[c("doy")], dcov[c("species","studyid")], FUN=length)
aggregate(dcovspstudy["studyid"], dcovspstudy[c("species")], FUN=length)

if(runmodels){
  arm1spstud <- stan_lmer(doy~1 + (1|studyid) + (yearadj|species), data=dcov)
  save(arm1spstud, file="..//output/arm1spstud.Rdata")
}

if(!runmodels){
  load("..//output/arm1spstud.Rdata")
}

## RStan

# Formatting for R stan (way faster than rstanarm here)
dcovnoNA <- subset(dcov, is.na(doy)==FALSE)
N <- nrow(dcovnoNA)
y <- dcovnoNA$doy
Nspp <- length(unique(dcovnoNA$species)) 
species <- as.numeric(as.factor(dcovnoNA$species))
year <- dcovnoNA$yearadj

if(runmodels){
  fit <- stan("..//stan/twolevelhierslope.stan", data=c("N","y","Nspp","species","year"), 
    iter=4000, chains=4)
  save(fit, file="..//output/rstan1sp.Rdata")

}
if(!runmodels){
  load("..//output/rstan1sp.Rdata")
}

# grab Stan output rstanarm
head(summary(arm1sp))
arm1sppost <- as.data.frame(arm1sp)
spslopes <- arm1sppost[, grep("b\\[yearadj species", colnames(arm1sppost))]

# estimate slopes for primary consumers vs. primary producers
table(dcov$trophic.level)
primaryprod <- subset(dcov, trophic.level=="primary producer")
ppspp <- unique(primaryprod$species)
primarycons <- subset(dcov, trophic.level=="primary consumer")
pcspp <- unique(primarycons$species)
# I don't recommend doing things by numbers like below, but this is quick for now
which(sort(unique(dcov$species)) %in% ppspp)
which(sort(unique(dcov$species)) %in% pcspp)
spslopespp <- spslopes[, which(sort(unique(dcov$species)) %in% ppspp)]
spslopespc <- spslopes[, which(sort(unique(dcov$species)) %in% pcspp)]

par(mfrow=c(1,2))
hist(rowMeans(spslopespp), main="", xlab="1ary producers")
hist(rowMeans(spslopespc), main="", xlab="1ary consumers")
quantile(rowMeans(spslopespp), c(0.1, 0.9))
quantile(rowMeans(spslopespc), c(0.1, 0.9))

# grep Stan output RStan
# Starter code, not finished for this output!
# watch out, below uses summary, but you should work from full posteriors 
rstan1sppost <- extract(fit) # posterior!
sumer <- summary(fit)$summary
muparams <- sumer[grep("mu", rownames(sumer)), c("mean", "2.5%", "25%", "50%", "75%", "97.5%")]
sigmaparams <- sumer[grep("sigma", rownames(sumer)), c("mean", "2.5%","25%", "50%", "75%", "97.5%")]

spslopes <- sumer[grep("b\\[", rownames(sumer)), "mean"]

