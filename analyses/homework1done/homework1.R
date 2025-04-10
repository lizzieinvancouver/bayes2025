## Started 18 January 2025 ##
## By Lizzie ## 
# This took me about an hour total # 

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# libraries
library(rstanarm)
options(mc.cores = parallel::detectCores()) # use all my cores, go for it!

setwd("~/Documents/git/teaching/hotstats/hotstatsmisc/homeworkbayes2025/homeworksubmitted/lizzie/homework1done")

# Set up a regression problem: a plant grows in response to increasing water
n <- 100 # in my testing, I put this as high as 10000 to check my code for the test data
alphaintercept  <- 5 # height with low light
beta <- 1.5 # slope per unit light
sigma <- 3.8
xhere <- runif(n, 5, 20)
ypred <- alphaintercept + beta*xhere
yobs <- rnorm(n, ypred, sigma) 

plot(yobs~xhere) # still seems maybe hopeful on error, but okay

# Add a 0/1 (binary) treatment
betatreat <- 1
betaintxn <- beta/2 
ntreat <- 2 # number of treatments
control <- rep(0, n/ntreat)
treated <- rep(1, n/ntreat)
treatx <- c(control, treated)

# I'll just write over what I did before
ypred <- alphaintercept + beta*xhere + betatreat*treatx + betaintxn*(xhere*treatx)
yobs <- rnorm(n, ypred, sigma) 

# Look at your data (visualize, visualize, visualize)
# Visualizing yourself visualizing, then make it happen!
plot(yobs[which(treatx==0)]~xhere[which(treatx==0)], 
	ylab="height", xlab="light", ylim=c(0, max(yobs))) 
points(yobs[which(treatx==1)]~xhere[which(treatx==1)], col="red") 

memodel <- stan_glm(yobs~xhere*treatx)
mypars <- c("(Intercept)", "xhere", "treatx", "xhere:treatx", "sigma")
mepost <- as.data.frame(memodel, pars = mypars)
colMeans(mepost)

# I'll functionalize part of the above now to make the rest of the homework easier ...

getmodeloutput <- function(numseq, alphaintercept, beta, sigma, betatreat, betaintxn){ 
	saveme <- data.frame(n=numeric(), 
		alphaintercept=numeric(),
		beta1=numeric(), 
		beta2=numeric(), 
		betaintxn=numeric(),
		sigma=numeric())
	for(i in numseq){
		# Simulated data
		n  <- i
		alphaintercept  <- alphaintercept # height with low light
		beta <- beta # slope per unit light
		sigma <- sigma
		xhere <- runif(n, 5, 20)
		betatreat <- betatreat
		betaintxn <- betaintxn
		control <- rep(0, n/ntreat)
		treated <- rep(1, n/ntreat)
		treatx <- c(control, treated)
		ypred <- alphaintercept + beta*xhere + betatreat*treatx + betaintxn*(xhere*treatx)
		yobs <- rnorm(n, ypred, sigma) 
		# Run in rstanarm
		memodel <- stan_glm(yobs~xhere*treatx)
		mypars <- c("(Intercept)", "xhere", "treatx", "xhere:treatx", "sigma")
		mepost <- as.data.frame(memodel, pars = mypars)
		savemeadd <- data.frame(n=i, 
			alphaintercept=colMeans(mepost)["(Intercept)"],
			beta1=colMeans(mepost)["xhere"], 
			beta2=colMeans(mepost)["treatx"], 
			betaintxn=colMeans(mepost)["xhere:treatx"],
			sigma=colMeans(mepost)["sigma"])
		saveme <- rbind(saveme, savemeadd)
		remove(memodel) # I am not sure if this is needed (esp. inside a f(x)), but cannot hurt
		remove(mepost)
	}
	return(saveme)
}


versionfullsigma  <- getmodeloutput(numseq=rep(seq(from=10, to=100, by=20), each=1), 
	alphaintercept=5, 
	beta=1.5,
	sigma=3.8,
	betatreat=1,
	betaintxn=beta/2)
warnings() # none of my warnings about divergences so I skipped creating a catch for that in the f(x)
write.csv(versionfullsigma, "output/versionfullsigma.csv")

pdf("figures/versionfullsigma.pdf", height=6, width=9)
par(mfrow=c(2,3))
plot(versionfullsigma[["alphaintercept"]]~versionfullsigma[["n"]],
	xlab="n", ylab="alphaintercept")
abline(h=alphaintercept)
plot(versionfullsigma[["beta1"]]~versionfullsigma[["n"]],
	xlab="n", ylab="beta main effect 1")
abline(h=beta)
plot(versionfullsigma[["beta2"]]~versionfullsigma[["n"]],
	xlab="n", ylab="beta main effect 2 (0/1)")
abline(h=betatreat)
plot(versionfullsigma[["betaintxn"]]~versionfullsigma[["n"]],
	xlab="n", ylab="beta for interaction")
abline(h=betaintxn)
plot(versionfullsigma[["sigma"]]~versionfullsigma[["n"]],
	xlab="n", ylab="sigma")
abline(h=sigma)
dev.off()

versionhalfsigma  <- getmodeloutput(numseq=rep(seq(from=10, to=100, by=20), each=1), 
	alphaintercept=5, 
	beta=1.5,
	sigma=1.4,
	betatreat=1,
	betaintxn=beta/2)
warnings() # none of my warnings about divergences so I skipped creating a catch for that in the f(x)
write.csv(versionhalfsigma, "output/versionhalfsigma.csv")

# Would be best to z-score here to compare, but for now:
pdf("figures/versionhalfandfullsigma.pdf", height=6, width=9)
par(mfrow=c(2,3))
plot(versionfullsigma[["alphaintercept"]]~versionfullsigma[["n"]],
	xlab="n", ylab="alphaintercept")
points(versionhalfsigma[["alphaintercept"]]~versionhalfsigma[["n"]],
	xlab="n", ylab="alphaintercept", col="dodgerblue")
abline(h=alphaintercept)
plot(versionfullsigma[["beta1"]]~versionfullsigma[["n"]],
	xlab="n", ylab="beta main effect 1")
points(versionhalfsigma[["beta1"]]~versionhalfsigma[["n"]], col="dodgerblue")
abline(h=beta)
plot(versionfullsigma[["beta2"]]~versionfullsigma[["n"]],
	xlab="n", ylab="beta main effect 2 (0/1)")
points(versionhalfsigma[["beta2"]]~versionhalfsigma[["n"]], col="dodgerblue")
abline(h=betatreat)
plot(versionfullsigma[["betaintxn"]]~versionfullsigma[["n"]],
	xlab="n", ylab="beta for interaction")
points(versionhalfsigma[["betaintxn"]]~versionhalfsigma[["n"]], col="dodgerblue")
abline(h=betaintxn)
plot(versionfullsigma[["sigma"]]~versionfullsigma[["n"]],
	xlab="n", ylab="sigma", ylim=c(0, 7))
points(versionhalfsigma[["sigma"]]~versionhalfsigma[["n"]], col="dodgerblue")
abline(h=sigma)
abline(h=1.4, col="dodgerblue")
dev.off()


## Code to help with challenges A-B ... 

# Challenge A
n <- 99
treat3levels <- rep(1:3, each=99/3)
treat3levelsdummy1 = ifelse(treat3levels == 2, 1, 0) 
treat3levelsdummy2 = ifelse(treat3levels == 3, 1, 0) 

# Challenge B
n <- 100
reps <- 100/4 # 4 total unique treatments in a full factorial of two 0/1 treatments
treat1 = gl(2, reps, length = n)
treat1 = ifelse(treat1 == 2, 1, 0) 
treat2 = gl(2, reps*2, length = n)
treat2 = ifelse(treat2 == 2, 1, 0) 
treatcombo = paste(treat1, treat2, sep = "_")
table(treatcombo)

getmodeloutputchallengeb <- function(numseq, alphaintercept, beta, sigma, betatreat, betaintxn){ 
	saveme <- data.frame(n=numeric(), 
		alphaintercept=numeric(),
		beta1=numeric(), 
		beta2=numeric(), 
		betaintxn=numeric(),
		sigma=numeric())
	for(i in numseq){
		# Simulated data
		n  <- i
		alphaintercept  <- alphaintercept # height with low light
		beta <- beta # slope per unit light
		sigma <- sigma
		betatreat <- betatreat
		betaintxn <- betaintxn
		reps <- n/4 # 4 total unique treatments in a full factorial of two 0/1 treatments
		treat1 = gl(2, reps, length = n)
		treat1 = ifelse(treat1 == 2, 1, 0) 
		treat2 = gl(2, reps*2, length = n)
		treat2 = ifelse(treat2 == 2, 1, 0) 
		ypred <- alphaintercept + beta*treat1 + betatreat*treat2 + betaintxn*(treat1*treat2)
		yobs <- rnorm(n, ypred, sigma) 
		# Run in rstanarm
		memodel <- stan_glm(yobs~treat1*treat2)
		mypars <- c("(Intercept)", "treat1", "treat2", "treat1:treat2", "sigma")
		mepost <- as.data.frame(memodel, pars = mypars)
		savemeadd <- data.frame(n=i, 
			alphaintercept=colMeans(mepost)["(Intercept)"],
			beta1=colMeans(mepost)["treat1"], 
			beta2=colMeans(mepost)["treat2"], 
			betaintxn=colMeans(mepost)["treat1:treat2"],
			sigma=colMeans(mepost)["sigma"])
		saveme <- rbind(saveme, savemeadd)
		remove(memodel) 
		remove(mepost)
	}
	return(saveme)
}

versionchallengeb  <- getmodeloutputchallengeb(numseq=rep(seq(from=10, to=100, by=20), each=1), 
	alphaintercept=5, 
	beta=3,
	sigma=1.2,
	betatreat=3,
	betaintxn=3/2)
write.csv(versionchallengeb, "output/versionchallengeb.csv")

pdf("figures/versionchallengeb.pdf", height=6, width=9)
par(mfrow=c(2,3))
plot(versionchallengeb[["alphaintercept"]]~versionchallengeb[["n"]],
	xlab="n", ylab="alphaintercept")
abline(h=5)
plot(versionchallengeb[["beta1"]]~versionchallengeb[["n"]],
	xlab="n", ylab="beta main effect 1 (0/1)")
abline(h=3)
plot(versionchallengeb[["beta2"]]~versionchallengeb[["n"]],
	xlab="n", ylab="beta main effect 2 (0/1)")
abline(h=3)
plot(versionchallengeb[["betaintxn"]]~versionchallengeb[["n"]],
	xlab="n", ylab="beta for interaction")
abline(h=3/2)
plot(versionchallengeb[["sigma"]]~versionchallengeb[["n"]],
	xlab="n", ylab="sigma")
abline(h=1.2)
dev.off()