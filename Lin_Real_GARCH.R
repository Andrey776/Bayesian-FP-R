T <- length(subset$vix_lin_ret)-1
r <- subset$vix_lin_ret[-(c(1))]
x <- subset$rv5[-(c(1))]
sigma_r_1 <- 0.1 # it is sqrt(h[1])
T_out <- 5

stan_dat_real_garch = list(T = T, x = x, r = r, h_1 = (sigma_r_1)^2, T_out = T_out)
real_garch = "
data {
  int<lower=2> T;
  vector <lower=0> [T] x;
  vector [T] r;
  real<lower=0> h_1;
  int<lower=0> T_out;
}

parameters {
  real mu_r;
  real<lower=0.0> omega;          
  real<lower=0.0,upper = 1>  beta;  
  real<lower=0.0, upper= 1 - beta> gamma;   
  real xi;
  real phi;
  real <lower= -1.0, upper = 1 - beta - gamma> tau_1;
  real <lower= -1.0, upper = 1 - beta - gamma - tau_1> tau_2;
  real<lower=0.000001> sigma_u;          
}

transformed parameters {
  vector [T] mu_x;
  vector [T] z; 
  vector <lower=0.000001> [T] h;
  h[1] = h_1;  
  for (t in 2:T)
    h[t] = omega + beta * h[t - 1] + gamma  * x[t - 1];

  for (t in 1:T)
    z[t] = (r[t] - mu_r)/ sqrt(h[t]); 
    
  for (t in 1:T)
    mu_x[t] = xi + phi*h[t] + tau_1 * z[t] + tau_2 * (square(z[t]) - 1); 
}

model { 
  mu_r ~ normal(0, 1);   
  omega ~ normal(0, 1);   
  beta ~ normal(0, 1);   
  gamma ~ normal(0, 1);   
  omega ~ normal(0, 1);   
  xi ~ normal(0, 1);   
  phi ~ normal(0, 1);   
  tau_1 ~ normal(0, 1);   
  tau_2 ~ normal(0, 1);   

  r ~ normal(mu_r, sqrt(h));
  x ~ normal(mu_x, sigma_u);
}
generated quantities {
  vector [T_out] r_out;
  vector [T_out] h_out;
  vector [T_out] x_out;
  vector [T_out] z_out;
  vector [T_out] mu_x_out;
  
  h_out[1] = omega + beta * h[T] + gamma * x[T];
  r_out[1] = normal_rng(mu_r, sqrt(h_out[1]));
  z_out[1] = r_out[1] / sqrt(h_out[1]);
  mu_x_out[1] = xi + phi*h_out[1] + tau_1*z_out[1] + tau_2*(square(z_out[1]) - 1);
  x_out[1] = normal_rng(mu_x_out[1], sigma_u);
  
  for (i in 2:T_out) {
    h_out[i] = omega + beta * h_out[i - 1] + gamma * x_out[i - 1];
    r_out[i] = normal_rng(mu_r, sqrt(h_out[i]));
    z_out[i] = r_out[i] / sqrt(h_out[i]);
    mu_x_out[i] = xi + phi*h_out[i] + tau_1*z_out[i] + tau_2*(square(z_out[i]) - 1);
    x_out[i] = normal_rng(mu_x_out[i], sigma_u);
  }
}
"
r_fit = stan(model_code = real_garch, data = stan_dat_real_garch, iter = 2*10^3, chains = 3, 
             control = list(max_treedepth = 10))
summary(r_fit)$summary[,c("mean", "2.5%", "97.5%")][c("omega", "beta", "gamma", 
                                                      "xi", "phi", "tau_1", 
                                                      "tau_2", "sigma_u",
                                                      "h[10]", "h[20]", "h[100]",
                                                      "r_out[1]", "r_out[4]"),]

### some useful commands
#save(r_fit, file = "real_garch_test_3.rda", compress = "xz")
sso <- launch_shinystan(r_fit)
#t = extract(r_fit)
#plot(t$r_out[1])