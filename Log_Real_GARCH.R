T <- length(subset$vix_lin_ret)-1
r <- subset$vix_lin_ret[-(c(1))]
x <- subset$rv5[-(c(1))]
sigma_r_1 <- 0.1 # it is sqrt(h[1])
T_out <- 5

stan_dat_real_garch = list(T = T, x = x, r = r, h_1 = (sigma_r_1)^2, T_out = T_out)
real_garch = "
data {
  int<lower=2> T;
  vector[T] <lower=0> y;    // returns (r in HH20212)
  vector[T] <lower=0> r[T]; // realized volatility (x in HH2012)
  real<lower=0> sigma_u_1;  // 
  int<lower=0> T_out;
}

parameters {
  vector[T] mu_y;
  real<lower=0.05> omega;          
  real<lower=0.1,upper=1>  beta1;  
  real<lower=0.1, upper=0.6> alpha1; // gamma in HH2012   
  real xi;
  real delta          //phi;
  real eta_1;         // tau_1 in HH2012
  real eta_2;         // tau_2 in HH2012
  real<lower=0> lambda// sigma_u in HH2012
}

transformed parameters {
  vector[T] mu_log_r;
  vector[T] z;
  vector[T] <lower=0.000001> sigma;
  sigma[1] = sigma_1;  
  for (t in 2:T)
    log(sigma[t]) = omega + alpha1 * log(r[t - 1]) + beta1  * log(sigma[t - 1]);

  for (t in 1:T)
    z[t] = (y[t] - mu_x[t])/ sigma[t]; 
    
  for (t in 1:T)
    mu_r[t] = xi + delta*2*log(sigma[t]) + eta_1*z[t] + eta_2*(square(z[t]) - 1); 
}

model { 
  omega ~ normal(0,10);          
  beta ~ normal(0,1);   
  gamma ~ normal(0.4,0.15) ;   
  xi ~ normal(0, 100);
  delta ~ normal(0, 100);
  tau_1 ~ normal(0, 1);
  tau_2 ~ normal(0, 1);
  lambda ~ normal(0.05, 0.25);
  
  y ~ normal(mu_y, sigma);
  log(r) ~ normal(mu_log_r, lambda);
}
generated quantities {
  real y_out[T_out];
  real<lower=0> sigma_out[T_out]; // latent variable
  real r_out[T_out];              // realized volatility
  real z_out[T_out];              // technical
  real mu_log_r_out[T_out];           // technical
  
  sigma_out[1] = sqrt(exp(omega + alpha1 * log(r[T]) + beta1 * log(square(sigma[T]));
  y_out[1] = normal_rng(mu_y, sigma_out[1]);
  z_out[1] = y_out[1] / sigma_out[1];
  mu_log_r_out[1] = xi + delta*2*log(sigma_out[1]) + eta_1*z[1] + eta_2*(square(z[1]) - 1);
  r_out[1] = exp(normal_rng(mu_log_r_out[1], lambda));
  
  for (i in 2:T_out) {
    sigma_out[i] = sqrt(exp(omega + alpha1 * log(r_out[i-1]) + beta1 * log(square(sigma_out[i-1]));
    y_out[i] = normal_rng(mu_y, sigma_out[i]);
    z_out[i] = r_out[i] / sqrt(h_out[i]);
    mu_x_out[i] = xi + phi*h_out[i] + tau_1*z_out[i] + tau_2*(square(z_out[i]) - 1);
    x_out[i] = normal_rng(mu_x, sigma_u);
  }
}
"
r_fit = stan(model_code = real_garch, data = stan_dat_real_garch, iter = 3*10^3, chains = 3, 
             control = list(max_treedepth = 14))
summary(r_fit)$summary[,c("mean", "2.5%", "97.5%")][c("omega", "beta", "gamma", 
                                                      "xi", "phi", "tau_1", 
                                                      "tau_2", "sigma_u",
                                                      "h[10]", "h[20]", "h[100]", "h[200]",
                                                      "r_out[1]", "r_out[4]"),]

### some useful commands
#save(r_fit, file = "real_garch_test_3.rda", compress = "xz")
sso <- launch_shinystan(r_fit)
#t = extract(r_fit)
#plot(t$r_out[1])