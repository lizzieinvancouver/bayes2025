############################################
# Script by Hoai Huong to simulate data using Stan
#   - Question: How many girls in 400 births?
#   - Note that the data simulation is in the generated quantities function only
#   - The remaining parts are to predict theta value
############################################

# HOUSEKEEPING
library(rstan)
library(tidyr)
library(dplyr)
rstan_options(auto_write = TRUE)

############################################
# STAN
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # Choosing mean_theta, sd_theta value so that theta is approximately 0.488
    # Actually is unnecessary in this case but might be helpful if theta is of other distribution (eg. beta)
investigate <- crossing(mean_theta = seq(0.4,0.6,0.001), sd_theta = seq(0.01,0.2,0.02)) %>%
                  mutate(
                  lower = qnorm(0.025, mean_theta, sd_theta),
                  upper = qnorm(0.975, mean_theta, sd_theta)
                  ) %>%
                  mutate(sse = (lower - 0.488)^2 + (upper - 0.488)^2) %>%
                  arrange(sse)

gender_prediction <- stan(file = "BabyGender.stan",
                          data = list(N = 400, n_girls = 196, mean_theta = 0.488,sd_theta = 0.001,
                                      iter = 2000, chains = 4 # Default Values
                                      ))

############################################
# Summarize the results
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
shinystan::launch_shinystan(gender_prediction)

  # We only care about n_girls_sim
posterior <- as.matrix(gender_prediction)

jpeg(file="DataSimulation.jpeg")
hist(posterior[,2], main = "Number of Baby Girls in 400 Births", xlab = "Num Girls")
abline(v = mean(posterior[,2]), col='red', lwd = 3)
dev.off()


