// Two-level (1 hierarchical grouping) `random' slope model
// Does not have partial pooling on intercept, but fits unique intercepts to each species
// adapted from synchrony repo (twolevelrandomslope2.stan) in February 2022

data{
int<lower=0> N; 	// number of total observations
int<lower=0> Nspp; 	// number of species (grouping factor)
int species[N]; 	// species identity, coded as int
vector[N] year; 	// year of data point (predictor for slope)
real y[N]; 		// day of year of phenological event (response)
}

parameters{	
real mu_b;		// mean slope across species
real<lower=0> sigma_b;	// variation of slope across species
real<lower=0> sigma_y; 	// measurement error, noise etc. 
real a[Nspp]; 		// the intercept for each species
real b[Nspp]; 		// the slope for each species 

}

transformed parameters{
real ypred[N];
for (i in 1:N){
	ypred[i]=a[species[i]]+b[species[i]]*year[i];
}	
}

model{	
b ~ normal(mu_b, sigma_b); // this creates the partial pooling on slopes
y ~ normal(ypred, sigma_y); // this creates an error model where error is normally distributed
// Priors ...
mu_b ~ normal(0,5);
sigma_b ~ normal(0,15);
sigma_y ~ normal(0,15);
}	
