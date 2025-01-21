## Started 18 January 2022 ##
## Let's simulate some data ... ##
## exampleday2_fakedata.R


# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## Let's simulate the height of wheat in a nitrogen experiment ...
# In a real situation, we'd have a little more info on how tall, what values make sense etc.
# So, keep that in mind. 

nplants <- 100 # total samples (plants)
ntreat <- 2 # number of treatments


##
## VERSION 1: An experiment with treatments
##

# build the x data, in this case the treatments (0 or 1)
control <- rep(0, nplants/ntreat)
treated <- rep(1, nplants/ntreat)
treatvec <- c(control, treated)

# What else do we need? We need to get y data
# y <- ax + b + error
# ypred = ax + b

baseheight <- 0.1 # the intercept (from notation above: b)...
# in this case, note the math here that we already know for the controls: y = b*0 + b
# so our intercept will the height of plants when the treatment is 0 (no nitrogen)
treateffect <- 0.1 # treatment effect (from notation above: a) 
sigma_y <- 0.03 # error
ypred <- treateffect*treatvec + baseheight

par(mfrow=c(1,2)) # make two plot spaces in one window
plot(ypred~treatvec, ylab="wheat height (m) with no error", xlab="Treatments")

# Okay, we're close, we just need to add the error term
# either of these works (equally well) to add the sigma_y 
heightalt <- ypred + rnorm(nplants, 0, sigma_y)
height <- rnorm(nplants, ypred, sigma_y)

plot(height~treatvec, ylab="wheat height (m) with no error", xlab="Treatments")



##
## VERSION 2: Nitrogen as a continuous variable that I measured
##

# What if I just measured the nitrogen in the soil?
# I updated this a touch after class

interceptwithnonitro <- 0 # if plants really cannot grow without N we should set this to 0
# Not shown ... rethink the effect of nitrogen (treateffect)?
nitroinsoil <- rnorm(nplants, 5, 5) # some amount per hectare

# Now we have new x data and sensible intercept ...
yprednsoil <- treateffect*nitroinsoil + baseheight
heightnsoil <- rnorm(nplants, yprednsoil, sigma_y)


# Plotting both examples
par(mfrow=c(1,2))
plot(height~treatvec, ylab="wheat height in m", xlab="Treatments")
plot(heightnsoil~nitroinsoil, ylab="wheat height in m", xlab="Nitrogen in soil")

# Expand to see the data in context ... 
plot(heightnsoil~nitroinsoil, ylab="wheat height in m",
     xlim=c(0,100), ylim=c(0,10))
abline(lm(heightnsoil~nitroinsoil))

##
## EXTRA: Fit the secon model in rstanarm
##

# rstanarm model
library(rstanarm)

# We have a fairly simple model and so will use stan_glm
# Here's a quick overview of the major model commands (this is what I could not find in class)
# https://cran.rstudio.com/web/packages/rstanarm/
whmodel <- stan_glm(heightnsoil~nitroinsoil)

library(shinystan)
launch_shinystan(whmodel)

