## Started 27 January 2025 ##
## By Lizzie ##

# Consider the two datasets (below) and report on the model you recommend fitting. 
# Would you fit a hierarchical model? Yes or no and why?
# If you would fit a hierarchical model, what exact type and why? Give the notation. 
# Once you finish that, fit the model and pull out the most relevant metric for the aim stated. 

rm(list=ls()) 
options(stringsAsFactors = FALSE)

# libraries
library(rstanarm)
options(mc.cores = parallel::detectCores()) # use all my cores, go for it!

## Dataset 1
# Data on judges' scores for pairs figure skaters across two programs: short (program) and artistic (performance). 
# Aim: You want to award an Olympic medal in figure skating to the best pair in the short program. 

if(length(grep("lizzie", getwd()) > 0)) {
  setwd("~/Documents/git/teaching/hotstats/hotstatsbayes/analyses/hierarchical")
} else{
  setwd("/home/boomdittyboom")
}

dat <- read.csv("..//input/OlympicGames_1932.csv")
head(dat)

prog <- subset(dat, criterion == "Program")


## Dataset 2
# You have gathered meta-analytic data on the day of various phenological events across diverse species, collected in different studies. 
# Aim: You want to best estimate the change over time in phenology across all species.

# doy is day of year of the various phenological events
d <- read.csv("..//input/loughnan2024/phenologyData.csv")
