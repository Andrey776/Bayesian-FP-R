library(shinystan)

setwd('/Users/AM/Documents/_CU Masters/2020 fall Bayesian_7393/Final_Project/data')
temp = readRDS("RealGARCH11 2020-11-25 for 2017-01-22 2017-07-22 .rda")
sso <- launch_shinystan(temp)