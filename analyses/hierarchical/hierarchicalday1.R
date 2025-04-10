## Started 21 January 2025 ##
## By Lizzie ##
## Trying to make a simplified version of example from bayesianflowsexample.rmd ##

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

## Set flags 
runinstan <- TRUE # set to FALSE if you want to run in rstanarm, but ...
# It's slow and the underlying model is not the one I write fake data for below ... 

# get the data so we can think through simulation
d <- read.csv("..//input/rawlong.tot2.csv")
d$X <- NULL # delete the old row numbers 

# We have species!
table(d$species)

# And year, yr1981 and phenovalue
# year1981 looks better than year given numerical issues 
head(d)

# For simplicty, let's fit a linear regression of 
# phenovalue as a f(x) of year1981 and ...
# group by species for the intercept and slope

Nspp <- 80

# Set up parameters (mostly hyper-parameters)
alphamu <- 180
alphasigma <- 20
slopemu <- -0.05
slopesigma <- 1.5
sigmay <- 5

alphaspp <- rnorm(Nspp, alphamu, alphasigma)
slopespp <- rnorm(Nspp, slopemu, slopesigma)

# Keep the parameters together to compare to model output
paramsgiven <- c(alphamu, alphasigma, slopemu, slopesigma, sigmay)

# Build the data ...
# get x, first how many years for each spp?
yearsdataperspp <- round(runif(Nspp, 5, 40))
# now build a spp vector and related years ...
species <- rep(1:Nspp, yearsdataperspp)
N <- length(species)
year <- rep(NA, N)

for (sp in 1:Nspp){
  year[species==sp] <- 1:(yearsdataperspp[sp])
}

# Now we are ready to build the y data!
ypred <- length(N)

for (n in 1:N){
  s <- species[n]
  ypred[n] <- alphaspp[s] + slopespp[s]*year[n]
}

y <- rnorm(N, ypred, sigmay)

# I end up with some days beyond 365 but I am okay with this for now
hist(y)

# Plot the data
par(mar=c(3,3,1,1), mgp=c(1.5,.5,0), tck=-.01)
plot(range(year), range(y), type="n", xlab="Year", ylab="Day of year",
     bty="l", main="Test data")
for (sp in 1:Nspp)
  lines(year[species==sp], y[species==sp], col="darkblue")

##
## Models fit in rstanarm 
## These are slow ...
if(runinstan==FALSE){
# Test data
memodelranint <- stan_lmer(y ~ 1 + year + (1|as.factor(species))) # adding this one does not seem to matter
memodelranintslopes <- stan_lmer(y ~ year|(as.factor(species)))

save(memodelranint, file="..//output/memodelranint.Rdata")
save(memodelranintslopes, file="..//output/memodelranint.Rdata")

# Real data
empmodelranint <- stan_lmer(phenovalue ~ yr1981 + (1|species), data=d, iter=4000, warmup=3000)
empodelranintslopes <- stan_lmer(phenovalue ~ yr1981|species, data=d, iter=4000, warmup=3000)

save(empmodelranint, file="..//output/empmodelranint.Rdata")
save(empodelranintslopes, file="..//output/empmodelranint.Rdata")
}

# Code for extracting rstanarm posterior, but not updated for above
if(FALSE){
mypars <- c("(Intercept)", "xhere", "treatx", "xhere:treatx", "sigma")
mepost <- as.data.frame(memodel, pars = mypars)
colMeans(mepost)
}


##
## Run in Stan
## 
if(runinstan==TRUE){

## Test data 

fit <- stan("..//stan/twolevelhierslopeint.stan", data=c("N","y","Nspp","species","year"), 
  iter=1000, chains=4, seed=377)

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

## Empirical data 

# Formatting for R stan
N <- nrow(d)
y <- d$phenovalue
Nspp <- length(unique(d$species)) #newid is character !
species <- as.numeric(as.factor(d$species))
year <- d$yr1981

fitemp <- stan("..//stan/twolevelhierslopeint.stan", data=c("N","y","Nspp","species","year"), iter=1000, chains=4, seed=377)

}




## Plotting
# See also new code (as of March 2025): plotshrikagesync.R #
# If time allows, follow models_stan_plotting_pp.R (OSPREE)
# These figures are modelscompare_pp_force.pdf etc.
