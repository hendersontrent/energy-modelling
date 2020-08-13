//
// This Stan program defines a Bayesian state space model
// for gas usage data at the State/Territory level
//

//
// Author: Trent Henderson, 13 August 2020
//

data {
  int n_months;             // number of months
  real y_values[n_months]; // actual values in months
  real mu_start;           // first value in series
  real sigma;              // standard deviation of series
}

parameters {
  real<lower=0> mu[n_months]; // don't define an upper limit as this can throw off modelling
}


model {
  
// state model

mu[1] ~ normal(mu_start, sigma);

for (i in 2:n_months) 
mu[i] ~ normal(mu[i - 1], sigma);

// measurement model

for(t in 1:n_months)
y_values[t] ~ normal(mu[t], sigma);

}
