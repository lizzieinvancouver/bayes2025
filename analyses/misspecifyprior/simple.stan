// Intercept only model

data {
	int<lower=1> N;
	vector[N] y; 		
	}

parameters {
  real<lower=0> sigma_y; 
  real a; // intercept
	}

transformed parameters {
   real yhat[N];
   // intercept only model
   for(i in 1:N){
      yhat[i] = a;
		}

	}

model {
   // Priors
   a ~ normal(25, 2);
   sigma_y ~ normal(2, 2);
   // Normal error model
	y ~ normal(yhat, sigma_y);

}
