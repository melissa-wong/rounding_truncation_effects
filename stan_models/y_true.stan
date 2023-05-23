data {
  int<lower=0> N;   // number of data items
  vector[N] x;      // predictor vector
  vector[N] y;      // outcome vector
}

parameters {
  real alpha;           // intercept
  real beta;            // coefficients for predictor
  real<lower=0> sigma;  // error scale
}

model {
  // Likelihood
  //print("log density before =", target());
  //y ~ normal(x * beta + alpha, sigma);
  target += normal_lpdf(y | x * beta + alpha, sigma);
  //print("log density after =", target());
  // Priors
  //alpha ~ normal(0, 5);
  target += normal_lpdf(alpha | 0, 5);
  //beta ~ normal(0, 5);
  target += normal_lpdf(beta | 0, 5);
  //sigma ~ exponential(1);
  target += exponential_lpdf(sigma | 1);
}
