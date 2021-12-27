library(brms)

## Define Rounded Likelihood

rounded_normal <- custom_family(
  "rounded_normal", 
  dpars = c("mu", "sigma"),
  lb = c(NA, 0),
  links = c("identity"), 
  type = "real",
  vars = "vint1[n]"
)

stan_funs_rounded_normal <- "
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
"

posterior_predict_rounded_normal <- function(i, prep, ...) {
  mu <- prep$dpars$mu[, i]
  sigma <- prep$dpars$sigma
  iflag <- prep$data$vint1[i]
  rounded_normal_rng(mu, sigma, iflag)
}

posterior_epred_rounded_normal <- function(prep) {
  mu <- prep$dpars$mu
}

log_lik_rounded_normal <- function(i, prep) {
  mu <- prep$dpars$mu[, i]
  sigma <- prep$dpars$sigma
  iflag <- prep$data$vint1[i]
  y <- prep$data$Y[i]
  rounded_normal_lpdf(y, mu, sigma, iflag)
}


## Define Truncated Likelihood

truncated_normal <- custom_family(
  "truncated_normal", 
  dpars = c("mu", "sigma"),
  lb = c(NA, 0),
  links = c("identity"), 
  type = "real",
  vars = "vint1[n]"
)

stan_funs_truncated_normal <- "
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
    // Likelihood
    
  }
  
  real truncated_normal_rng(real mu, real sigma, int iflag) {
    if (iflag == 1) {
        return trunc(normal_rng(mu, sigma));

    }
    else {
        return normal_rng(mu, sigma);
    }
  }
"

posterior_predict_truncated_normal <- function(i, prep, ...) {
  mu <- prep$dpars$mu[, i]
  sigma <- prep$dpars$sigma
  iflag <- prep$data$vint1[i]
  truncated_normal_rng(mu, sigma, iflag)
}

posterior_epred_truncated_normal <- function(prep) {
  mu <- prep$dpars$mu
}

log_lik_truncated_normal <- function(i, prep) {
  mu <- prep$dpars$mu[, i]
  sigma <- prep$dpars$sigma
  iflag <- prep$data$vint1[i]
  y <- prep$data$Y[i]
  truncated_normal_lpdf(y, mu, sigma, iflag)
}
