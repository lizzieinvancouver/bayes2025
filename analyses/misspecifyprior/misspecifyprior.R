## Copied from projects/misc/miscmisc/bayesianflows/examples/misspecifiedmodel.R ##
## Err, which I copied from git/teaching/stan/priorpost/priorpost.R
## 13 Jan 2025 ##

rm(list=ls()) 
options(stringsAsFactors = FALSE)

setwd("~/Documents/git/teaching/hotstats/hotstatsbayes/analyses/misspecifyprior/")

library(rstan)

# params for fake data
priormean_a <- 25 
priorsigma_a <- 2 
a <- 5
sigma_y <- 2
N <- 5

# Plot true value and base prior
sampleprior <- rnorm(10000, priormean_a, priorsigma_a)
xrange <- c(1,38)
ylimhere <- c(0, 0.25)
hist(sampleprior, prob=TRUE, xlim=xrange, xlab="", main="the prior and the true value",
    col = "white", border = "white") 
lines(density(sampleprior), col="dodgerblue3", lwd=2, lty=3)
abline(v=a, col="dodgerblue3", lwd=2)

pdf("priorpostforflows.pdf", height=8, width=4)
par(mfrow=c(3, 1))
for (i in c(5, 10, 40)){
    N <- i
    usedat <- rnorm(N, a, sigma_y)
    # run the model 
    fit  <- stan("simple.stan", data=list(N=N, y=usedat), iter=2000, chains=4)
    posterior <- extract(fit, pars = c("a"))
    if(N<100){
    hist(posterior$a, xlab="", prob=TRUE, xlim=xrange, ylim=ylimhere, 
        col="lightsalmon", border = FALSE,
        main=paste("With ", N, " data points", sep=""))
    lines(density(sampleprior), col="dodgerblue3", lwd=2, lty=3)
    } else {
        hist(posterior$a, xlab="", prob=TRUE, xlim=xrange, ylim=c(0,2), 
        col="lightsalmon", border = FALSE,
        main=paste("With ", N, " data points", sep=""))
        lines(density(sampleprior), col="dodgerblue3", lwd=2, lty=3)
    }
    abline(v=a, col="dodgerblue3", lwd=2) 
    }
dev.off()