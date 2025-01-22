## Started 21 January 2025 ##
## By Lizzie, but cribbing off Olympic1932_example.R ##

rm(list=ls()) 
options(stringsAsFactors = FALSE)

# libraries
library(rstanarm)
options(mc.cores = parallel::detectCores()) # use all my cores, go for it!

if(length(grep("lizzie", getwd()) > 0)) {
  setwd("~/Documents/git/teaching/hotstats/hotstatsbayes/sandboxcode/figureskating")
} else{
  setwd("/home/boomdittyboom")
}

dat <- read.csv("input/OlympicGames_1932.csv")
head(dat)

prog <- subset(dat, criterion == "Program")

memodel <- stan_lmer(score ~ (1 | judge) + (1 |pair), data = prog)

save(memodel, file="output/memodel.Rdata")

library(shinystan)
launch_shinystan(memodel)