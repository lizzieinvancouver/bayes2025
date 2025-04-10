## Started 20 January 2025 ##
## By Lizzie ##
## Some stuff copied from exampleday2_fakedata.R ##

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## Let's simulate the height of wheat in a nitrogen experiment ...
# Let's consider an example where I just measured the nitrogen in the soil

nplants <- 100 # total samples (plants)
baseheight <- 0.2 # the intercept ...
# our intercept will the height of plants when the treatment is 0 (no nitrogen)
treateffect <- 0.15 # treatment effect  
sigma_y <- 0.03 # error

interceptwithnonitro <- 0 # if plants really cannot grow without N we should set this to 0
# Not shown ... rethink the effect of nitrogen (treateffect)?
nitroinsoil <- rnorm(nplants, 10, 15) # some amount per hectare

# Now we have new x data and sensible intercept ...
yprednsoil <- treateffect*nitroinsoil + baseheight
heightnsoil <- rnorm(nplants, yprednsoil, sigma_y)

# Plotting the data
par(mfrow=c(1,1))
plot(heightnsoil~nitroinsoil, ylab="wheat height in m", xlab="Nitrogen in soil")

# Expand to see the data in context ... 
plot(heightnsoil~nitroinsoil, ylab="wheat height in m",
     xlim=c(0, max(nitroinsoil)), ylim=c(0, max(heightnsoil)))
abline(lm(heightnsoil~nitroinsoil))

##
## Fit the model in rstanarm
##

# rstanarm model
library(rstanarm)

# We have a fairly simple model and so will use stan_glm
whmodel <- stan_glm(heightnsoil~nitroinsoil)

library(shinystan)
launch_shinystan(whmodel)

