## Started 28 January 2025 ##
## By Lizzie ##
## Snippets from workflow in Bayesian flows ## 
## With extra examples and notes on retrodictive checks ##

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


# First, we'll plot the real data used in the model
# pdf("graphs/realdata_formodel.pdf", height=4, width=6)
par(mar=c(3,3,1,1), mgp=c(1.5,.5,0), tck=-.01)
plot(range(year), range(yreal), type="n", xlab="Year",
     ylab="Day of year", bty="l", main="Raw real data")
for (j in 1:Nspp){
  lines(year[species==j], yreal[species==j])
}
hist(yreal, xlab="Day of year", main="Real data")
# dev.off()

# What does a similar plot look like using the model output?
# Let's grab the model output and do a really simple example using ...
# just the mean values of the posteriors
syncmodelhispost <- extract(syncmodelhis) 
# hist(syncmodelhispost$mu_b, xlab="Change per year")

# extract means for now 
sigma_y <- mean(syncmodelhispost$sigma_y) 
sigma_a <- mean(syncmodelhispost$sigma_a) 
sigma_b <- mean(syncmodelhispost$sigma_b) 
mu_b <- mean(syncmodelhispost$mu_b) 
mu_a <- mean(syncmodelhispost$mu_a) 

a <- rnorm(Nspp, mean=mu_a, sd=sigma_a)
b <- rnorm(Nspp, mean=mu_b, sd=sigma_b)

N <- Nreal

ypred <- length(N) 
for (n in 1:N){
    s <- species[n]
    ypred[n] <- a[s] + b[s]*year[n]
}
y <- rnorm(N, ypred, sigma_y)

#pdf("graphs/onepredictivecheck.pdf", height=4, width=8)
par(mfrow=c(1,3))
par(mar=c(3,3,1,1), mgp=c(1.5,.5,0), tck=-.01)
plot(range(year), range(y), type="n", xlab="Year", ylab="Day of year",
    bty="l", main="Data from posterior means")
for (j in 1:Nspp)
  lines(year[species==j], y[species==j])
hist(y, xlab="Day of year", main="Data from posterior means")
hist(yreal, xlab="Day of year", main="Real data")
# dev.off()


# The below is comparing the time-series, and prints directly to PDF
pdf("graphs/rawvsonepredictivecheck.pdf", height=8, width=6)
par(mfrow=c(2,1))
par(mar=c(3,3,1,1), mgp=c(1.5,.5,0), tck=-.01)
plot(range(year), range(yreal), type="n", xlab="Year",
      ylab="Day of year (empirical data)", bty="l", main="")
for (j in 1:Nspp){
  lines(year[species==j], yreal[species==j], col="pink3")
 }
plot(range(year), range(y), type="n", xlab="Year", ylab="Day of year (simulated from posterior means)",
     bty="l", main="")
for (j in 1:Nspp)
   lines(year[species==j], y[species==j], col="plum4")
dev.off()

# Okay, but that's just one new draw and it's from the mean values ... 
# PPCs should be done with many draws from the posterior 
# But then you need to decide on what summary statistics matter because you cannot just look at each plot
# Below I do: SD of y (using the means, I should also consider using other draws of the posterior)

# Create the data using new a and b for each of the species, simshere times
simshere <- 1000
# Randomly sample the iterations (we have 4000 here).
iterations <- sample(1:4000, simshere, replace = FALSE)
# Create an empty matrix to get the resulting output 
y.sd100 <- matrix(0, ncol=simshere, nrow=Nspp)
# For this version, let's use the species-level intercepts and slopes (e.g., syncmodelhispost[["b"]]) ...
for (i in 1:simshere){
    iterhere <- iterations[i]
    for (n in 1:N){
        s <- species[n]
        ypred[n] <- syncmodelhispost[["a"]][iterhere,s]+ syncmodelhispost[["b"]][iterhere,s]*year[n] 
    }
  y <- rnorm(N, ypred, syncmodelhispost[["sigma_y"]][iterhere])
  y.df <- as.data.frame(cbind(y, species))
  y.sd <- aggregate(y.df["y"], y.df["species"], FUN=sd)
  y.sd100[,i] <- y.sd[,2] 
}



# For this version, let's draw a new collection of species from mu_a and mu_b, using the full posteriors
# Also, I use $ to index the list here instead of [[]]
y.sd100.muab <- matrix(0, ncol=simshere, nrow=Nspp)
for (i in 1:simshere){
    iterhere <- iterations[i]
    a <- rnorm(Nspp, mean=syncmodelhispost$mu_a[iterhere], sd=syncmodelhispost$sigma_a[iterhere])
    b <- rnorm(Nspp, mean=syncmodelhispost$mu_b[iterhere], sd=syncmodelhispost$sigma_b[iterhere])
    for (n in 1:N){
        s <- species[n]
        ypred[n] <- a[s] + b[s]*year[n]
    }
  y <- rnorm(N, ypred, syncmodelhispost[["sigma_y"]][iterhere])
  y.df <- as.data.frame(cbind(y, species))
  y.sd <- aggregate(y.df["y"], y.df["species"], FUN=sd)
  y.sd100.muab[,i] <- y.sd[,2] 
}


# ... and here's the real data, includes studyid -- which we discussed adding to model
real.sd <- aggregate(rawlong.tot2["phenovalue"], rawlong.tot2[c("studyid", "species")],
    FUN=sd)

par(mfrow=c(1,2))
par(mar=c(5,3,1,1), mgp=c(3,.5,0), tck=-.01)
hist(colMeans(y.sd100), col="lightblue", breaks=20, xlim=c(10,15), 
    main="Using species-level posteriors",
    xlab="Mean SD of response from 1000 sim. datasets (light blue) \n versus empirical data (dark blue line)")
abline(v = mean(real.sd$phenovalue), col = "darkblue", lwd = 2)
hist(colMeans(y.sd100.muab), col="lightblue", breaks=20, xlim=c(10,15), 
    main="Drawing new species from posteriors",
    xlab="Mean SD of response from 1000 sim. datasets (light blue) \n versus empirical data (dark blue line)")
abline(v = mean(real.sd$phenovalue), col = "darkblue", lwd = 2)


##
## Other things we looked at (in reality)
##


# Okay, let's look at other aspects of the model
comppool <- lm(phenovalue~yr1981, data=rawlong.tot2)

# no pooling
uniquespp <- unique(rawlong.tot2$species)
slopefits <- rep(NA< length(uniquespp))
varfits <- rep(NA< length(uniquespp))
intfits <- rep(NA< length(uniquespp))
for(eachsp in 1:length(uniquespp)){
	lmhere <- lm(phenovalue~yr1981, data=subset(rawlong.tot2, species==uniquespp[eachsp]))
	slopefits[eachsp] <- coef(lmhere)[2]
	varfits[eachsp] <- (summary(lmhere)$sigma)**2
	intfits[eachsp] <- coef(lmhere)[1]
}
dr <- data.frame(cbind(uniquespp, slopefits, varfits, intfits))
dr$slopefits <- as.numeric(dr$slopefits)
dr$intfits <- as.numeric(dr$intfits)
dr$varfits <- as.numeric(dr$varfits)

# get 'new' species intercepts and slopes
# this is one way to create fake data from the Stan output to use in the PP check
a <- rnorm(Nspp, mean=mu_a, sd=sigma_a)
b <- rnorm(Nspp, mean=mu_b, sd=sigma_b) 

# compare a few things on this single new dataset
par(mfrow=c(1,2))
hist(b, main="slopes (b) from the stan model with mean from the raw data in blue")
abline(v = mean(dr$slopefits), col = "blue", lwd = 2) # less negative, slopes are generally pooled towards center which makes sense
hist(dr$varfits, main="sigma y (b) from the raw data with sigma_y from the model in blue")
abline(v=sigma_y, col = "blue", lwd = 2) 

par(mfrow=c(1,2))

hist(dr$intfits, breaks=20, main="No pool intercepts", xlab="intercept")
hist(a, breaks=20, main="Partial pool intercepts")

