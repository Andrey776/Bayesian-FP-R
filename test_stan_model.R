T <- length(rv_vix_subset$vix_lin_ret)-1
r <- rv_vix_subset$vix_lin_ret[-(c(1))]
sigma1 <- 0.1
T_out <- 10

stan_dat_test = list(T=T, r = r, sigma1 = sigma1, T_out = T_out)
stan_model_test = "
data {
  int<lower=2> T; 
  real r[T];
  real<lower=0> sigma1;
  int<lower=0> T_out;
}
parameters {
  real mu; 
  real<lower=0> alpha0;          
  real<lower=0> alpha1;  
  real<lower=0> beta1; 
}
transformed parameters {
  real<lower=0> sigma[T];
  sigma[1] = sigma1;
  for (t in 2:T)
    sigma[t] = sqrt(alpha0 + alpha1 * square(r[t - 1] - mu) + beta1  * square(sigma[t - 1]));
}
model {
  r ~ normal(mu,sigma);
}
generated quantities {
  real r_out[T_out];
  real sigma_out[T_out];
  sigma_out[1] = sqrt(alpha0 + alpha1 * square(r[T] - mu) + beta1 * square(sigma[T]));
  r_out[1] = normal_rng(mu, sigma_out[1]);
  for (i in 2:T_out) {
    sigma_out[i] = sqrt(alpha0 + alpha1 * square(r_out[i - 1] - mu) + beta1 * square(sigma_out[i - 1]));
    r_out[i] = normal_rng(mu, sigma_out[i]);
  }
}
"
r_fit = stan(model_code = stan_model_test, data = stan_dat_test, iter = 10^4, chains = 1)
#summary(r_fit)$summary[,"Rhat"]
summary(r_fit)$summary[,c("mean", "50%","2.5%", "97.5%")][c("mu", "sigma[33]","r_out[1]", "alpha0", "alpha1", "beta1", "r_out[5]", "r_out[6]", "r_out[7]", "r_out[8]", "r_out[9]", "r_out[10]"),]
