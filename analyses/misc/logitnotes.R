## Started 13 Jan 2025 ## 
## By Lizzie ##

## Taken from my notes: logit_whatiknow.txt (dealing with some back conversion issues)
## And code from 2019 class ... just my overall notes; need to set up rest of the code! ##

# linear model
z <- a + ba*agri + bu*urban
# inverse logit -- this is the reverse of logit(p) in your model code
p <- 1/(1+exp(-z))
# now, can get bernoulli
y <- rbinom(ndata, 1, p)

# I always wonder about error in this model, see:
# https://stats.stackexchange.com/questions/124818/logistic-regression-error-term-and-its-distribution

# Also, nice overview of how to simulate logistic 
# https://stats.stackexchange.com/questions/12857/generate-random-correlated-data-between-a-binary-and-a-continuous-variable/12858#12858