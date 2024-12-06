/*
  QUESTION: WHAT IS THE PROBABILITY OF HAVING BABY GIRLS, GIVING THAT THE BIRTHS ARE ALL SINGLE?
*/

/*
  DATA: Input to the model
  - Basically everything that you feed into the model when running from your main script
  - Can be either be a dataset, sample size, number of predictors, data scales, etc.
  - Note that vector is of real data type only
  */
data {
  int<lower=0> N; // Number of births (observations)
  int n_girls;
  real<lower=0> mean_theta;
  real<lower=0> sd_theta;
}

/*
  PARAMETERS: The parameters of the model to be estimated
*/
parameters {
  real <lower=0, upper = 1> theta;
}

/*
  MODEL: includes definition of priors for each parameter, and the likelihood for the data
*/
model {
  // prior
  theta ~ normal(mean_theta, sd_theta);
  // likelihood
  n_girls ~ binomial(N, theta);
}

/*
  SIMULATION OF DISCRETE PROBABILITY MODELS
  Question: How many girls in 400 births?
*/
/*
  GENERATED QUANTITIES: Any quantities that are not part of the model but can be computed from the parameters for every iteration.
*/
generated quantities {
  int n_girls_sim;
  n_girls_sim = binomial_rng(N, theta);
}

