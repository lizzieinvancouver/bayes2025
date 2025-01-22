## Started 21 January 2025 ##
## By Lizzie ##
## Trying to make a simplified version of example from bayesianflowsexample.rmd ##

rm(list=ls()) 
options(stringsAsFactors = FALSE)

# libraries
library(rstanarm)
options(mc.cores = parallel::detectCores()) # use all my cores, go for it!

if(length(grep("lizzie", getwd()) > 0)) {
  setwd("~/Documents/git/teaching/hotstats/hotstatsbayes/sandboxcode")
} else{
  setwd("/home/boomdittyboom")
}

# get the data so we can think through simulation
d <- read.csv("input/rawlong.tot2.csv")
d$X <- NULL # delete the old row numbers 

# We have species!
table(d$species)

# And year, yr1981 and phenovalue
# year1981 looks better than year given numerical issues 
head(d)

# For simplicty, let's fit a linear regression of 
# phenovalue as a f(x) of year1981 and ...
# group by species for the intercept and slope

nspp <- 80

# Set up parameters (mostly hyper-parameters)
alphamu <- 180
alphasigma <- 20
slopemu <- -0.05
slopeaigma <- 1.5
sigmay <- 5

alphaspp <- rnorm(nspp, alphamu, alphasigma)
slopespp <- rnorm(nspp, slopemu, slopeaigma)

# Build the data ...
# get x, first how many years for each spp?
yearsdataperspp <- round(runif(nspp, 5, 40))
# now build a spp vector and related years ...
species <- rep(1:nspp, yearsdataperspp)
N <- length(species)
year <- rep(NA, N)

for (sp in 1:nspp){
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
for (sp in 1:nspp)
  lines(year[species==sp], y[species==sp], col="darkblue")

# Test data
memodelranint <- stan_lmer(y ~ 1 + year + (1|as.factor(species)) # adding this one does not seem to matter
memodelranintslopes <- stan_lmer(y ~ year|(as.factor(species))

save(memodelranint, file="output/memodelranint.Rdata")
save(memodelranintslopes, file="output/memodelranint.Rdata")

# Real data
empmodelranint <- stan_lmer(phenovalue ~ yr1981 + (1|species), data=d)
empodelranintslopes <- stan_lmer(phenovalue ~ yr1981|species, data=d)

save(empmodelranint, file="output/empmodelranint.Rdata")
save(empodelranintslopes, file="output/empmodelranint.Rdata")

if(FALSE){
mypars <- c("(Intercept)", "xhere", "treatx", "xhere:treatx", "sigma")
mepost <- as.data.frame(memodel, pars = mypars)
colMeans(mepost)
}

## Plotting
# If time allows, follow models_stan_plotting_pp.R (OSPREE)
# These figures are modelscompare_pp_force.pdf etc.
