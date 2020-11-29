library(shinystan)

setwd('/Users/AM/Documents/_CU Masters/2020 fall Bayesian_7393/Final_Project/output')
temp = readRDS("ARCH1 2020-11-28 for 2017-01-22 2017-07-22 .rda")
sso <- launch_shinystan(temp)

log_lik = extract_log_lik(temp, merge_chains = FALSE)
r_eff = exp(relative_eff(log_lik))
waic = waic(log_lik)
waic$waic
looic = loo(log_lik, r_eff = r_eff)
looic$looic

i=1
start_date = "2017-05-22"
step_size = 2
subset_duration = 6
t_0 = as.Date(start_date) + months((i-1) * step_size)
t_1 = as.Date(t_0) + months(subset_duration + 1) #interim end_date, for subset_long
subset_long = build_vix9_rv_subset(t_0, t_1)
t_1 = as.Date(t_0) + months(subset_duration) #stored end_date, for subset
subset = build_vix9_rv_subset(t_0, t_1)
y = subset$vix_lin_ret 

temp_name = gsub(" ", "", paste("r_out[", as.character(i),"]", ""))
r_out=extract(temp, pars=temp_name)[[1]]