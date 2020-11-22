T <- length(subset$vix_lin_ret)-1
r <- subset$vix_lin_ret[-(c(1))]
x <- subset$rv5[-(c(1))]
sigma_r_1 <- 0.1 # it is sqrt(h[1])
T_out <- 5

stan_dat_real_garch = list(T = T, x = x, r = r, h_1 = (sigma_r_1)^2, T_out = T_out)
real_garch = "
data {
  int<lower=2> T;
  real<lower=0> x[T];
  real r[T];
  real<lower=0> h_1;
  //real<lower=0> sigma_u;
  int<lower=0> T_out;
}

parameters {
  real<lower=0.05> omega;          
  real<lower=0.2,upper=1>  beta;  
  real<lower=0.2, upper=0.7> gamma;   
  real xi;
  real phi;
  real tau_1;
  real tau_2;
  real<lower=0.000001> sigma_u;          
}

transformed parameters {
  real mu_x[T];
  real z[T]; 
  real<lower=0.000001> h[T];
  h[1] = h_1;  
  for (t in 2:T)
    h[t] = omega + beta * h[t - 1] + gamma  * x[t - 1];

  for (t in 1:T)
    z[t] = r[t] / sqrt(h[t]); 
    
  for (t in 1:T)
    mu_x[t] = xi + phi*h[t] + tau_1*z[t] + tau_2*(square(z[t]) - 1); 
}

model { 
  omega ~ normal(0,1);          
  beta ~ normal(0,1);   
  gamma ~ normal(0.4,0.15) ;   
  xi ~ normal(0.0, 1);
  phi ~ normal(1, 20);
  tau_1 ~ normal(0.1,0.15);
  tau_2 ~ normal(0.1,0.3);
  sigma_u ~ normal(0.05, 0.25);
  
  r ~ normal(0, sqrt(h));
  x ~ normal(mu_x, sigma_u);
}
generated quantities {
  real r_out[T_out];
  real<lower=0> h_out[T_out];
  real x_out[T_out];
  real z_out[T_out];
  real mu_x_out[T_out];
  
  h_out[1] = omega + beta * h[T] + gamma * x[T];
  r_out[1] = normal_rng(0, sqrt(h_out[1]));
  z_out[1] = r_out[1] / sqrt(h_out[1]);
  mu_x_out[1] = xi + phi*h_out[1] + tau_1*z_out[1] + tau_2*(square(z_out[1]) - 1);
  x_out[1] = normal_rng(mu_x_out[1], sigma_u);
  
  for (i in 2:T_out) {
    h_out[i] = omega + beta * h_out[i - 1] + gamma * x_out[i - 1];
    r_out[i] = normal_rng(0, sqrt(h_out[i]));
    z_out[i] = r_out[i] / sqrt(h_out[i]);
    mu_x_out[i] = xi + phi*h_out[i] + tau_1*z_out[i] + tau_2*(square(z_out[i]) - 1);
    x_out[i] = normal_rng(mu_x_out[i], sigma_u);
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