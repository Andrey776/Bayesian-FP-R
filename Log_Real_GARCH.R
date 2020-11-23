T <- length(subset$vix_lin_ret)-1
y <- subset$vix_lin_ret[-(c(1))]
r <- subset$rv5[-(c(1))]
sigma_1 <- 0.1 # it is sqrt(h[1])
T_out <- 5

stan_dat_real_garch_log = list(T = T, r = r, y = y, sigma_1 = sigma_1, T_out = T_out)
real_garch_log = "
data {
  int<lower=2> T;
  vector[T] y;              // returns (r in HH20212)
  vector<lower=0>[T] r;     // realized volatility (x in HH2012)
  real<lower=0> sigma_1;  // 
  int<lower=0> T_out;
}

parameters {
  real mu_y;
  real beta1;  
  real alpha1;           // gamma in HH2012   
  real delta;           //phi;
  real eta_1;           // tau_1 in HH2012
  real eta_2;           // tau_2 in HH2012
  real<lower=0> lambda; // sigma_u in HH2012
  real omega;          
  real xi;

}

transformed parameters {
  vector[T] mu_log_r;
  vector[T] z;
  vector<lower=0>[T]  sigma;
  sigma[1] = sigma_1;  
  for (t in 2:T)
    sigma[t] = exp((omega + alpha1 * log(r[t - 1]) + beta1  * 2 * log(sigma[t - 1])) / 2);

  for (t in 1:T)
    z[t] = (y[t] - mu_y)/ sigma[t]; 
    
  for (t in 1:T)
    mu_log_r[t] = xi + delta*2*log(sigma[t]) + eta_1*z[t] + eta_2*(square(z[t]) - 1); 
}

model { 
  mu_y ~ normal(0, 1) ;   
  omega ~ normal(-1, 4);          
  alpha1 ~ normal(0, 1) ;   
  beta1 ~ normal(0, 1);   
  xi ~ normal(6, 10);
  delta ~ normal(5, 5);
  eta_1 ~ normal(0.2, 0.5);
  eta_2 ~ normal(0, 0.2);
  lambda ~ normal(0.4, 0.2);
  
  y ~ normal(mu_y, sigma);
  
  log(r) ~ normal(mu_log_r, lambda);
}
generated quantities {
  real y_out[T_out];
  real<lower=0> sigma_out[T_out]; // latent variable
  real r_out[T_out];              // realized volatility
  real z_out[T_out];              // technical
  real mu_log_r_out[T_out];       // technical
  
  sigma_out[1] = exp((omega + alpha1 * log(r[T]) + beta1 * 2 * log(sigma[T]))/2);
  y_out[1] = normal_rng(mu_y, sigma_out[1]);
  z_out[1] = y_out[1] / sigma_out[1];
  mu_log_r_out[1] = xi + delta*2*log(sigma_out[1]) + eta_1*z_out[1] + eta_2*(square(z_out[1]) - 1);
  r_out[1] = exp(normal_rng(mu_log_r_out[1], lambda));
  
  for (i in 2:T_out) {
    sigma_out[i] = exp((omega + alpha1 * log(r_out[i-1]) + beta1 * 2 * log(sigma_out[i-1]))/2);
    y_out[i] = normal_rng(mu_y, sigma_out[i]);
    z_out[i] = y_out[i] / sigma_out[i];
    mu_log_r_out[i] = xi + delta*2*log(sigma_out[i]) + eta_1*z_out[i] + eta_2*(square(z_out[i]) - 1);
    r_out[i] = exp(normal_rng(mu_log_r_out[i], lambda));
  }
}
"
r_fit = stan(model_code = real_garch_log, data = stan_dat_real_garch_log, 
             iter = 2*10^3, chains = 2, control = list(max_treedepth = 10))

summary(r_fit)$summary[,c("mean", "2.5%", "97.5%")][c("mu_y", "omega", "alpha1", "beta1", 
                                                      "eta_1", "eta_2",
                                                      "xi", "delta", "lambda", 
                                                      "sigma[10]", "sigma[20]", "sigma[100]",
                                                      "y_out[1]", "y_out[4]")]

### some useful commands
#save(r_fit, file = "real_garch_test_3.rda", compress = "xz")
sso <- launch_shinystan(r_fit)
#t = extract(r_fit)
#plot(t$r_out[1])