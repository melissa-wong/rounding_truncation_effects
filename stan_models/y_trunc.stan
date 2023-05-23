functions {
  real truncated_normal_lpdf(real y, real mu, real sigma, int iflag) {
  
    // Likelihood
    if (iflag == 1) {
      if(y < 0.0) {
       return log(Phi((y - mu) / sigma)
          - Phi((y - 1.0 - mu) / sigma));     
      }
      else {
        return log(Phi((y + 1.0 - mu) / sigma)
            - Phi((y - mu) / sigma));
      }    
    }
    else {
      return normal_lpdf(y | mu, sigma);
    }
  }
  
  real truncated_normal_rng(real mu, real sigma, int iflag) {
    if (iflag == 1) {
        return trunc(normal_rng(mu, sigma));
    }
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
  vector[N] mu;
  mu = x * beta + alpha;
  // Likelihood
  //print("log density before =", target());
  for(i in 1:N) {
    target += truncated_normal_lpdf(y[i] | mu[i], sigma, iflag[i]);
  }
  //print("log density after =", target());
  // Priors
  //alpha ~ normal(0, 5);
  target += normal_lpdf(alpha | 0, 5);
  //beta ~ normal(0, 5);
  target += normal_lpdf(beta | 0, 5);
  //sigma ~ exponential(1);
  target += exponential_lpdf(sigma | 1);
}
