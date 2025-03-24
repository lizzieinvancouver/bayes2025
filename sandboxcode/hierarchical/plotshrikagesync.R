## Started 24 March 2025 ##
## By Lizzie ##

## Plotting shrinkage in synchrony interactions (2018 published data)

# Built off models_stan_plotting_pp.R (OSPREE) #
# Which was built off https://www.tjmahr.com/plotting-partial-pooling-in-mixed-effects-models/ #
# And see also: https://m-clark.github.io/posts/2019-05-14-shrinkage-in-mixed-models/ #

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## set working directory if you need to
setwd("~/Documents/git/teaching/hotstats/hotstatsbayes/sandboxcode/hierarchical")

## flags
# You must set to true and RUN the models once for setting this to FALSE to work
runmodels <- TRUE

## libraries
library(rstan)
options(mc.cores = parallel::detectCores())

##
## get the data and run the pp (partial pooling) model
## 
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

##
## Partial pooling plot (following from models_stan_plotting_pp.R in OSPREE repo)
##
d <- rawlong.tot2

# Get the complete pooling model
com.pool.mod <- lm(phenovalue~yr1981, data=d)
spp <- sort(unique(d$species))

# Set up some empty stuff to fill in
no.pool <- data.frame(species=rep(NA, length(spp)),
                 intercept=rep(NA, length(spp)), 
                 slope=rep(NA, length(spp)))
with.pool <- no.pool
df.gravity <- no.pool[2:3]
with.pool$model <- "partial pooling"
no.pool$model <- "no pooling"


for (sp in c(1:length(spp))){
    no.pool$species[sp] <- spp[sp]
    subby <- subset(d,  species==spp[sp])
    lmfit <- lm(phenovalue~yr1981, data=subby)
    no.pool$intercept[sp] <- coef(lmfit)["(Intercept)"]
    no.pool$slope[sp] <- coef(lmfit)["yr1981"]
    }

modhere <- summary(syncmodelhis)$summary
for (sp in c(1:length(spp))){
    with.pool$species[sp] <- spp[sp]
    with.pool$intercept[sp] <- modhere[grep("a\\[", rownames(modhere)),1][sp]
    with.pool$slope[sp] <- modhere[grep("b\\[", rownames(modhere)),1][sp]
    }

with.pool$species <- sort(unique(d$species)) # CHECK ME!
df.pulled <- rbind(no.pool, with.pool)

df.gravity$model <- NA
df.gravity$intercept[1] <-coef(com.pool.mod)["(Intercept)"]
df.gravity$slope[1] <-coef(com.pool.mod)["yr1981"]
df.gravity$model[1] <- "complete pooling"
df.gravity$intercept[2] <- modhere[grep("mu_a", rownames(modhere)),1]
df.gravity$slope[2] <- modhere[grep("mu_b", rownames(modhere)),1]
df.gravity$model[2] <- "partial pooling (mu)"


pdf(file.path("graphs/skrinkageplot.pdf"), width = 9, height = 6)
ggplot(df.pulled) + 
  aes(x = intercept, y = slope, color = model) + 
  geom_point(size = 2) + 
  geom_point(data = df.gravity, size = 5) + 
  # Draw an arrow connecting the observations between models
  geom_path(aes(group = as.character(species), color = NULL), 
            arrow = arrow(length = unit(.02, "npc"))) + 
  # Use ggrepel to jitter the labels away from the points
  ggrepel::geom_text_repel(
    aes(label = species, color = NULL), 
    data = no.pool, size=2) + 
  theme(legend.position = "bottom") + 
  ggtitle("Pooling of regression parameters: Int and slope") + 
  xlab("Intercept estimate") + 
  ylab("Slope estimate") + 
  scale_color_brewer(palette = "Dark2") 
dev.off()
