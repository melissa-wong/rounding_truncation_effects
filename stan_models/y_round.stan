functions {
  
  real rounded_normal_lpdf(real y, real mu, real sigma, int iflag) {
    // Likelihood
    if (iflag == 1) {
      return log(Phi((y + 0.5 - mu)/sigma) - Phi((y - 0.5 - mu)/sigma));
    }
    else {
      return normal_lpdf(y | mu, sigma);
    }
  }
  real rounded_normal_rng(real mu, real sigma, int iflag) {
    if (iflag == 1) {
      return round(normal_rng(mu, sigma));    }
    else {
      return normal_rng(mu, sigma);
    }
    
  }

}

data {
  int<lower=0> N;   // number of data items
  vector[N] x;      // predictor vector
  vector[N] y;      // outcome vector
  int iflag[N];
}

parameters {
  real alpha;           // intercept
  real beta;            // coefficients for predictor
  real<lower=0> sigma;  // error scale
}

model {
  vector [N] mu;
  mu = x * beta + alpha;
  for (i in 1:N) {
      target += rounded_normal_lpdf(y[i] | mu[i], sigma, iflag[i]);
  }
  // Priors
  //alpha ~ normal(0, 1);
  target += normal_lpdf(alpha | 0, 5);
  //beta ~ normal(0, 1);
  target += normal_lpdf(beta | 0, 5);
  //sigma ~ exponential(1);
  target += exponential_lpdf(sigma | 1);
}
