data {
  int<lower=0> N; // Number of observations
  array[N] int<lower=0, upper=1> y_obs; // Observed outcome
  array[N] int<lower=0, upper=1> x; // Binary predictor
}

parameters {
  real a; // Intercept (logodds scale)
  real b; // Coefficient (logodds scale)
  real<lower=0, upper=1> se; // Sensitivity
  real<lower=0, upper=1> sp; // Specificity
}

model {
  // Priors
  a ~ normal(0, 2.5); // Weakly informative
  b ~ normal(0, 2.5); // Weakly informative
  se ~ beta(221.6, 85.61); // Highly informative
  sp ~ beta(1037, 182.1); // Highly informative
  
  // Likelihood
  for (n in 1:N) {
    y_obs[n] ~ bernoulli((1 - sp) + (se + sp - 1) * inv_logit(a + b * x[n]));
  }
}

generated quantities {
  real oddsratio = exp(b);
}
