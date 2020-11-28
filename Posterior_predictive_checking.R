# Number of observations 
N <- 100

# Model matrix (with column of 1s for intercept and one covariate)
X <- cbind(Const = 1, X1 = rnorm(N))
K <- ncol(X)

# Generate fake outcome y
beta <- c(2, 1/2) # pick intercept and coefficient
sigma <- 1 # standard deviation
y <- rnorm(N, mean = X %*% beta, sd = sigma) # generate data
stan_mod_TEST = "
data {
  int           N ; # integer, number of observations
  int           K ; # integer, number of columns in model matrix
  matrix[N,K]   X ; # N by K model matrix
  vector[N]     y ; # vector of N observations
}

parameters {
  real<lower=0> sigma ; # real number > 0, standard deviation
  vector[K]     beta ;  # K-vector of regression coefficients
}

model {
  beta ~ normal(0, 5) ;       # prior for betas
  sigma ~ cauchy(0, 2.5) ;    # prior for sigma
  y ~ normal(X*beta, sigma) ; # vectorized likelihood
}

generated quantities {
  # Here we do the simulations from the posterior predictive distribution
  vector[N] y_rep ; # vector of same length as the data y
  for (n in 1:N) 
    y_rep[n] <- normal_rng(X[n]*beta, sigma) ;
}
"
library(rstan)
library(shinystan)

# Prepare the data we'll need as a list
stan_data <- list(y = y, X = X, N = N, K = K)

# Fit the model
stanfit <- stan(model_code = stan_mod_TEST, data = stan_data)

# Launch ShinyStan
launch_shinystan(stanfit)