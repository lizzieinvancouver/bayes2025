// Two-level (1 hierarchical grouping) `random' slope and intercept model
// Partial pooling on intercepts and slopes 
// Updated for new version of Stan (2025!)

data{
int<lower=0> N; 	// number of total observations
int<lower=0> Nspp; 	// number of species (grouping factor)
array[N] int species; 	// species identity, coded as int
vector[N] year; 	// year of data point (predictor for slope)
array[N] real y; 		// day of year of phenological event (response)
}

parameters{
real mu_a;		// mean intercept across species
real<lower=0> sigma_a;	// variation of intercept across species	
real mu_b;		// mean slope across species
real<lower=0> sigma_b;	// variation of slope across species
real<lower=0> sigma_y; 	// measurement error, noise etc. 	
vector[Nspp] a; 		//the intercept for each species
vector[Nspp] b; 		//the slope for each species 

}

transformed parameters{
array[N] real ypred;
for (i in 1:N){
    ypred[i]=a[species[i]]+b[species[i]]*year[i];
}
}

model{	
b ~ normal(mu_b, sigma_b); // this creates the partial pooling on slopes
a ~ normal(mu_a, sigma_a); // this creates the partial pooling on intercepts
y ~ normal(ypred, sigma_y); // this creates an error model where error is normally distributed
// Priors ...
mu_a ~ normal(100,30);
sigma_a ~ normal(0,20);
mu_b ~ normal(0,5);
sigma_b ~ normal(0,15);
sigma_y ~ normal(0,15);
}	
