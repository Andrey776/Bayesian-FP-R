library(data.table)
library(dplyr)
library(doParallel) 
library(quantmod) 
library(moments) 
library(rstan) 
library(xts)
library(lubridate)
library(ggplot2)
library(hrbrthemes)
library(shinystan)
library(corrplot)
devtools::install_github("stan-dev/loo")
library(loo)
library(xlsx)

#library(reshape2)
#launch_shinystan_demo()



```{r DATA upload and rv for SPX creation and formatting, include=FALSE}
### DATA ###
setwd('/Users/AM/Documents/_CU Masters/2020 fall Bayesian_7393/Final_Project/data')

SP_500 = fread("^GSPC.csv")

VIX_9d_CBOE = fread("vix9ddailyprices.csv")
VIX_30d_CBOE = fread("vixcurrent.csv")
VIX_3m_CBOE = fread("vix3mdailyprices.csv")
VIX_6m_CBOE = fread("vix6mdailyprices.csv")

realized = fread("oxfordmanrealizedvolatilityindices 2.csv")

SP_500 = SP_500[,c("Date", "Close", "Volume")]
colnames(SP_500) = c("date", "close", "volume")
SP_500$date = as.Date(SP_500$date)
#remove(SP_500)


vix_9d = VIX_9d_CBOE[-c(1,2,3,4), c("V1", "V5")]
colnames(vix_9d) = c("date", "vix_9d") 
vix_9d$date = as.Date(mdy(vix_9d$date))
vix_9d$vix_9d = as.numeric(vix_9d$vix_9d)
remove(VIX_9d_CBOE)

vix_30d = VIX_30d_CBOE[-c(1,2), c("V1", "V5")]
colnames(vix_30d) = c("date", "vix_30d") 
vix_30d$date = as.Date(mdy(vix_30d$date))
vix_30d$vix_30d = as.numeric(vix_30d$vix_30d)
remove(VIX_30d_CBOE)

vix_3m = VIX_3m_CBOE[-c(1,2,3), c("V1", "V5")]
colnames(vix_3m) = c("date", "vix_3m") 
vix_3m$date = as.Date(mdy(vix_3m$date))
vix_3m$vix_3m = as.numeric(vix_3m$vix_3m)
remove(VIX_3m_CBOE)

vix_6m = VIX_6m_CBOE[-c(1,2,3), c("V1", "V5")]
colnames(vix_6m) = c("date", "vix_6m") 
vix_6m$date = as.Date(mdy(vix_6m$date))
vix_6m$vix_6m = as.numeric(vix_6m$vix_6m)
remove(VIX_6m_CBOE)


temp = left_join(vix_9d, vix_30d, by = "date")
temp = left_join(temp, vix_3m, by = "date")
temp = left_join(temp, vix_6m, by = "date")
vix_df = temp
rm("vix_30d", "vix_3m", "vix_6m", "vix_9d", "temp")

realized$date = as_datetime(realized$V1)
realized = realized[,-c("V1")]
realized$date=realized$date+hours(2) #needed it as RV$V1 has 23:00 because of the daylight time (which wrongly shifts date to the previous one) 
realized$date = as.Date(realized$date)

temp = realized[realized$Symbol==".SPX",c("date", "close_price")]

#unique(realized$Symbol)
rv = realized[realized$Symbol==".SPX",]

### some examples of data upload and preprocessing
#VIXY = fread("^VIX.csv") # from Yahoo
#vix_cl=VIXY[,c("Date", "Close")]
#colnames(vix_cl) = c("date", "vix")
#vix_cl$date = as.Date(vix_cl$date)

#SP500_Y <- getSymbols( "^GSPC",
#src = "yahoo",
#from = "2015-01-01", to = "2020-11-12", auto.assign = FALSE
#)

```

```{r plot data violins}

temp_long = melt(vix_df, id="date")
ggplot(temp_long, aes(y = value, x = variable)) + 
  geom_violin(trim = F, show.legend = F, fill = "steel blue", alpha = .1) + 
  geom_boxplot(width=0.21, fill="steel blue", outlier.color="steel blue", alpha = .5,
               outlier.shape=3, color="steel blue", outlier.size=1) + 
  ylim(0,60) + 
  theme_ipsum() +
  ylab("VIX distribution")
summary(temp_long$value)
rm(temp_long)

temp_long = rv_vix_subset[, c("date", "vix_lin_ret")]
ggplot(temp_long, aes(y = vix_lin_ret, x = "return")) + 
  geom_violin(trim = F, show.legend = F, fill = "steel blue", alpha = .1) + 
  geom_boxplot(width=0.21, fill="steel blue", outlier.color="steel blue", alpha = .5,
               outlier.shape=3, color="steel blue", outlier.size=1) + 
  ylim(-.7, 1) + 
  theme_ipsum() +
  ylab("return distribution")
rm(temp_long)

temp_long = rv_vix_subset[, c("date", "rv5")]
ggplot(temp_long, aes(y = rv5, x = "realized volatility")) + 
  geom_violin(trim = F, show.legend = F, fill = "steel blue", alpha = .1) + 
  geom_boxplot(width=0.21, fill="steel blue", outlier.color="steel blue", alpha = .5,
               outlier.shape=3, color="steel blue", outlier.size=1) + 
  ylim(0, 0.0004) + 
  theme_ipsum() +
  ylab("realized volatility")
summary(temp_long$rv5)
rm(temp_long)
```

```{r #+#FUNCTION return }
  ##+++## add a normal return finction
  
  ## lin return ##
  lin_ret = function (time_ser) { #the same as Delt(x, type = 'log') from Quantmod lib
    l_r = ROC(time_ser, type = "discrete")
    return(l_r)
  }
  # function check
  lin_ret(c(10,15,18,27))
  
  ## log return ##
  
  log_ret = function (time_ser) { #the same as Delt(x, type = 'log') from Quantmod lib
    log_time_ser = log(time_ser)
    l_ret = momentum(log_time_ser)
    return(l_ret)
  }
  # function check
  log_ret(c(exp(1), exp(3), exp(4), exp(2), exp(5)))
  log_ret(c(1,1.01,1.01^2))
  Delt(c(exp(1), exp(3), exp(4), exp(2), exp(5)), type = 'log')
  Delt(c(1,1.01,1.01^2))
  
  ``` 
  
  ```{r #+# RV vs VIX with returns build up and visual inspection }
    ##++## plot together with VIX
    ##++## why VIX has NA? # solved (daylight time and 23:00 which resulted in  "date-1")
    ##+## (i.e. no visible prediction power in VIX, just repeats RV5 dynamic)
    
    rv_vix = left_join(rv[,c("date", "rv5")], vix_df[,c("date", "vix_9d")], by = "date")
    rv_vix_subset = rv_vix[((rv_vix$date>"2015-01-01") & (rv_vix$date<"2016-06-30")),]
    
    rv_vix_subset$vix_log_ret = log_ret(rv_vix_subset$vix_9d)
    rv_vix_subset$vix_lin_ret = lin_ret(rv_vix_subset$vix_9d)
    
    
    coeff = max(rv_vix_subset$vix_9d, na.rm = TRUE)/ max(rv_vix_subset$rv5, na.rm = TRUE)
    ggplot(rv_vix_subset, aes(date)) + 
      geom_line(aes(y = rv5, colour = "rv5")) + 
      geom_line(aes(y = vix_9d/ coeff, colour = "vix")) +
      scale_y_continuous(
        # Features of the first axis
        name = "Realized variance (5-min)",
        # Add a second axis and specify its features
        sec.axis = sec_axis(~.*coeff, name="vix (same day)")) + 
      theme_ipsum() +
      theme(
        axis.title.y = element_text(color = "red", size=12),
        axis.title.y.right = element_text(color = "cyan3", size=12)) +
      ggtitle("some inference") 
    
    ```
    
    ```{r #+# another plotting (two y axis)}
      ##+## plot rv vs vix_log_ret #found it USELESS
      ##+## double check that the dates of VIX and RV5 are really the same 
      ##+## (i.e. AGAIN no visible prediction power in VIX, just repeats RV5 dynamic)
      
      #rv_vix_subset$vix_log_ret = log_ret(rv_vix_subset$vix_9d)
      #rv_vix_subset$vix_lin_ret = lin_ret(rv_vix_subset$vix_9d)
      
      coeff = 2*max(rv_vix_subset$vix_lin_ret, na.rm = TRUE)/ max(rv_vix_subset$rv5, na.rm = TRUE)
      ggplot(rv_vix_subset, aes(date)) + 
        geom_line(aes(y = rv5, colour = "rv5"), alpha=.5) + 
        geom_line(aes(y = vix_lin_ret/ coeff, colour = "vix_log_ret"), alpha=.5) +
        scale_y_continuous(
          # Features of the first axis
          name = "realized variance (5-min)",
          # Add a second axis and specify its features
          sec.axis = sec_axis(~.*coeff, name="vix_lin_ret (same day)")) + 
        theme_ipsum() +
        theme(
          axis.title.y = element_text(color = "red", size=12),
          axis.title.y.right = element_text(color = "cyan3", size=12)) +
        ggtitle("some inference") 
      
      ```
      
      ```{r #+-+# exploratory data analysis SP_500/ VIX/ rv joint DF creation, include=FALSE}
        ##+++## take vix(9/30/180/..), rv5_forward_lag(0/5/9/30...).. SPX_volat(9/30/...) as of vix_date+9/30/180
        ##---## add autocorrelation plot
        
        SP_500_corr_df = inner_join(vix_df, rv[,c("date","rv5")], by = "date")
        #colnames(SP_500_corr_df)[6] = "rv5"
        # realized volatility rv5 today (i.e. as of (lead_0d)=0 days in the future from "date")
        SP_500_corr_df$rv5_lead_9d = lead(SP_500_corr_df$rv5, 9)
        SP_500_corr_df$rv5_lead_30d = lead(SP_500_corr_df$rv5, 30)
        SP_500_corr_df$rv5_lead_3m = lead(SP_500_corr_df$rv5, 91)
        SP_500_corr_df$rv5_lead_6m = lead(SP_500_corr_df$rv5, 182)
        
        SP_500_corr_df = left_join(SP_500_corr_df, SP_500, by = "date")
        SP_500_corr_df$volume = SP_500_corr_df$volume / 10^9
        # historical volatility for the last (sd_nd) days as of "date" 
        SP_500_corr_df$sd_9d = runSD(SP_500_corr_df$close, 9) 
        SP_500_corr_df$sd_30d = runSD(SP_500_corr_df$close, 30)
        SP_500_corr_df$sd_3m = runSD(SP_500_corr_df$close, 91)
        SP_500_corr_df$sd_6m = runSD(SP_500_corr_df$close, 182)
        
        # create temp columns with non leading na
        SP_500_corr_df$temp_lead_9d = lead(SP_500_corr_df$close, 9) 
        SP_500_corr_df$temp_lead_30d = lead(SP_500_corr_df$close,30)
        SP_500_corr_df$temp_lead_3m = lead(SP_500_corr_df$close,91)
        SP_500_corr_df$temp_lead_6m = lead(SP_500_corr_df$close,182)
        
        SP_500_corr_df = SP_500_corr_df[SP_500_corr_df$date < "2020-01-01", ]
        
        # historical volatility for the last (sd_nd) days as of "date"+ lead_9d (i.e. as of 9 days in the future from "date") 
        SP_500_corr_df$sd_9d_lead_9d = runSD(SP_500_corr_df$temp_lead_9d, 9) 
        SP_500_corr_df$sd_30d_lead_9d = runSD(SP_500_corr_df$temp_lead_9d, 30) 
        SP_500_corr_df$sd_3m_lead_9d = runSD(SP_500_corr_df$temp_lead_9d, 91) 
        SP_500_corr_df$sd_6m_lead_9d = runSD(SP_500_corr_df$temp_lead_9d, 182) 
        
        # historical volatility for the last (sd_nd) days as of "date"+ lead_30d (i.e. as of 30 days in the future from "date")  
        SP_500_corr_df$sd_9d_lead_30d = runSD(SP_500_corr_df$temp_lead_30d, 9) 
        SP_500_corr_df$sd_30d_lead_30d = runSD(SP_500_corr_df$temp_lead_30d, 30) 
        SP_500_corr_df$sd_3m_lead_30d = runSD(SP_500_corr_df$temp_lead_30d, 91) 
        SP_500_corr_df$sd_6m_lead_30d = runSD(SP_500_corr_df$temp_lead_30d, 182) 
        
        # historical volatility for the last (sd_nd) days as of "date"+ lead_3m (i.e. as of 91 days in the future from "date")  
        SP_500_corr_df$sd_9d_lead_3m = runSD(SP_500_corr_df$temp_lead_3m, 9) 
        SP_500_corr_df$sd_30d_lead_3m = runSD(SP_500_corr_df$temp_lead_3m, 30) 
        SP_500_corr_df$sd_3m_lead_3m = runSD(SP_500_corr_df$temp_lead_3m, 91) 
        SP_500_corr_df$sd_6m_lead_3m = runSD(SP_500_corr_df$temp_lead_3m, 182) 
        
        # historical volatility for the last (sd_nd) days as of "date"+ lead_6m (i.e. as of 182 days in the future from "date")  
        SP_500_corr_df$sd_9d_lead_6m = runSD(SP_500_corr_df$temp_lead_6m, 9) 
        SP_500_corr_df$sd_30d_lead_6m = runSD(SP_500_corr_df$temp_lead_6m, 30) 
        SP_500_corr_df$sd_3m_lead_6m = runSD(SP_500_corr_df$temp_lead_6m, 91) 
        SP_500_corr_df$sd_6m_lead_6m = runSD(SP_500_corr_df$temp_lead_6m, 182) 
        
        # remove temp columns 
        SP_500_corr_df = SP_500_corr_df[,-c("temp_lead_9d", "temp_lead_30d", "temp_lead_3m", "temp_lead_6m")]
        
        sum(is.na(SP_500_corr_df))
        
        SP_500_corr_df = SP_500_corr_df[SP_500_corr_df$date > "2012-01-01", ]
        
        
        ### take-aways
        # vix does not forecast a point estimate of RV
        # vix has good positive correlation with interval volatility (especially 9 days)
        # there is some correlation vix~volume to be explored
        # there is some correlation vix_6m~close to be explored
        # let's try to build the model around sd_9/30d_lead9/30d ~ vix9/30 
        
        ### code to double-check runSD
        #temp = SP_500_corr_df$close[1:91]
        #temp1 = 0
        #for (i in (1:length(temp))){temp1 = temp1 + (temp[i]-mean(temp))^2}
        #sqrt(temp1/(length(temp)-1))
        ```
        
        ```{r SP_500/ VIX/ rv correlation}
        M<-cor(SP_500_corr_df[,-c(1)])
        corrplot(M, type = "upper", tl.col = "steel blue")
        
        ```
        
        ```{r #-# STAN model AR(1), include=FALSE}
          ## - ## pass mu[1] as data
          ##---## play with different priors for GARCH(1,1)
          ##---## shiny stan - what Yrep and PPcheck do??
          
          stan_mod_AR1 = "
data {
  int T;
  vector[T] y;
  int T_out;
  real<lower=0> v;  // sample variance of y
  real mu_1;        // the first mu

}

parameters {
  real alpha;
  real beta;
  //real<lower=-1,upper=1> beta;
  real<lower=0> sigma;
}
transformed parameters {
  vector[T] mu;
  for (i in 2:T) {
    mu[i] = alpha + beta * y[i-1];
  }
  mu[1] = mu_1;
}
model {
  sigma ~ inv_gamma(1, 2);
  alpha ~ normal(0, 1);
  beta ~ normal(0, 1);
  for (n in 2:T)
    y[n] ~ normal(mu[n], sigma);
}
generated quantities {
  vector[T_out] r_out;
  vector[T] log_lik;
  //vector[T] lik;
  real Rbsq;              // goodness-of-fit
  Rbsq = 1 - square(sigma)/v;
  r_out[1] = normal_rng(alpha + beta * y[T], sigma);
  
  for (i in 2:T_out) {
    r_out[i] = normal_rng(alpha + beta * y[i-1], sigma);
  }
  for (i in 1:T) {
    log_lik[i] = normal_lpdf(y[i] | mu[i], sigma);
    //lik[i] = exp(log_lik[i]);
  }

}
"
#r_fit_AR1 = stan(model_code = stan_mod_AR1, 
#                 data = stan_dat_AR1, 
#                 iter = 3 * 10^3, chains = 3)

#summary(r_fit_AR1)$summary[,"Rhat"]
#summary(r_fit_AR1)$summary[,c("mean", "2.5%", "97.5%")][c("alpha", "beta", "r_out[1]", "r_out[5]", "r_out[9]"),]

#sigma_out_2 = extract(y_fit, pars="sigma_out[2]")[[1]]
#summary(sigma_out_2)
#plot(density(sigma_out_2))

#Rbsq=extract(r_fit_AR1, pars="Rbsq")[[1]]
#plot(density(Rbsq))

### some useful commands
#save(r_fit, file = "real_garch_test_3.rda", compress = "xz")
#sso <- launch_shinystan(r_fit_AR1)
#t = extract(r_fit)
#plot(t$r_out[1])
```

```{r STAN model ARCH(1), include=FALSE}

stan_mod_ARCH1 = "
data {
  int<lower=0> T;               // number of time points
  vector[T] y;                  // return at time t
  real sigma_1;                  // the first sigma
  int T_out;                    // forecast depth, days
}
parameters {
  real mu;                       // average return
  real<lower=0> alpha0;          // noise intercept
  real<lower=0,upper=1> alpha1;  // noise slope
  
}
transformed parameters {
  vector[T] sigma;
  for (i in 2:T) {
    sigma[i] = sqrt(alpha0 + alpha1 * pow(y[i-1] - mu,2));
  }
  sigma[1] = sigma_1;
}

model {
  mu ~ normal(0.01, 0.2);
  alpha0 ~ normal(0, 1);
  alpha1 ~ beta(1.1, 1.1);
  for (i in 2:T)
    y[i] ~ normal(mu, sigma[i]);
}

generated quantities {
  vector[T_out] r_out;
  vector[T_out] sigma_out;
  vector[T] log_lik;
  
  sigma_out[1] = sqrt(alpha0 + alpha1 * pow(y[T] - mu,2));
  r_out[1] = normal_rng(mu, sigma_out[1]);
  for (i in 2:T_out) {
    sigma_out[i] = sqrt(alpha0 + alpha1 * pow(r_out[i-1] - mu,2));
    r_out[i] = normal_rng(mu, sigma_out[i]);
  }
  
  for (i in 1:T) {
    log_lik[i] = normal_lpdf(y[i] | mu, sigma[i]);
  }

}

"
```

```{r #...# STAN model GARCH(1,1) include=FALSE}
  ##++## analyze correlation RV and VIX for the same date #they almost repeat each other
  ##++## choose what to model (RV point or f(RV[interval])?) # return
  ##++## read papers about prediction of rv (Shepard and Shepard? etc)
  ##++## add Rbsq
  ##++## add log lik
  ##++## add err
  
  stan_mod_GARCH11 = "
data {
  int<lower=2> T; 
  vector[T] y;
  real<lower=0> sigma_1;
  int<lower=0> T_out;
}
parameters {
  real mu;
  real<lower=0> alpha0;
  real<lower=0,upper=1> alpha1;
  real<lower=0,upper=(1-alpha1)> beta1;
}
transformed parameters {
  real<lower=0> sigma[T];
  sigma[1] = sigma_1;
  for (i in 2:T)
    sigma[i] = sqrt(alpha0
                     + alpha1 * pow(y[i-1] - mu, 2)
                     + beta1 * pow(sigma[i-1], 2));
}
model {
  mu ~ normal(0.01, 0.2);
  alpha0 ~ normal(0, 1);
  alpha1 ~ beta(1.1, 1.1);
  beta1 ~ beta(1.1, 1.1);
  for (i in 2:T)
    y[i] ~ normal(mu, sigma[i]);
}
generated quantities {
  vector[T_out] r_out;
  vector[T_out] sigma_out;
  vector[T] log_lik;
  vector[T] y_rep;
  
  sigma_out[1] = sqrt(alpha0
                     + alpha1 * pow(y[T] - mu, 2)
                     + beta1 * pow(sigma[T], 2));
  r_out[1] = normal_rng(mu, sigma_out[1]);
  for (i in 2:T_out) {
    sigma_out[i] = sqrt(alpha0
                     + alpha1 * pow(r_out[i-1] - mu, 2)
                     + beta1 * pow(sigma_out[i-1], 2));
    r_out[i] = normal_rng(mu, sigma_out[i]);
  }
  
  for (i in 1:T) {
    log_lik[i] = normal_lpdf(y[i] | mu, sigma[i]);
    y_rep[i] = normal_rng(mu, sigma[i]);
  }
  
}
"

#sso <- launch_shinystan(r_fit)
```

```{r STAN model RealGARCH(1,1) include=FALSE}
##++## analyze correlation RV and VIX for the same date #they almost repeat each other
##++## choose what to model (RV point or f(RV[interval])?) # return
##++## read papers about prediction of rv (Shepard and Shepard? etc)
##++## add log lik
##++## add err
##---## adjust and fine tune prior


stan_mod_RealGARCH11 = "
functions {
    real normal_lb_rng(real muu, real siigma, real lb) {
      real p = normal_cdf(lb, muu, siigma);  // cdf for bounds
      real u = uniform_rng(p, 1);
      return (siigma * inv_Phi(u)) + muu;  // inverse cdf for value
    }     
}
data {
  int<lower=2> T; 
  vector[T] y;                  // return
  vector<lower=0>[T] rv;                 // it is x in HH2012 (realized volatility estimate)               
  real<lower=0> sigma_1;
  int<lower=0> T_out;
}
parameters {
  real mu;
  real<lower=0>           alpha0;   // it is omega in HH2012
  real<lower=0, upper=1>   beta1;   // it is the same beta in HH2012
  real<lower=0,upper=(1-beta1)>   gamma; 
  real xi;
  real phi;
  real tau_1;
  real tau_2;
  
  //real<lower=0,upper=1-beta1-gamma> tau_1;
  //real<lower=0,upper=1-beta1-gamma-tau_1> tau_2;
  
  real<lower=0> sigma_rv;
  
}
transformed parameters {
  vector<lower=0>[T] sigma;       // it is h in HH2012
  vector[T] z;
  vector[T] mu_rv;
  sigma[1] = sigma_1;
  z[1] = (y[1]-mu)/sigma[1];
  mu_rv[1] = xi + phi * pow(sigma[1], 2) + tau_1 * z[1] + tau_2 * (pow(z[1], 2) - 1);
  for (i in 2:T) {
    sigma[i] = sqrt(alpha0
                     + beta1 * pow(sigma[i-1], 2)
                     + gamma * rv[i-1]);
    z[i] = (y[i]-mu)/sigma[i];
    mu_rv[i] = xi + phi * pow(sigma[i], 2) + tau_1 * z[i] + tau_2 * (pow(z[i], 2) - 1);
  }  
}
model {
  mu ~ normal(0.0084, 0.01);
  alpha0 ~ normal(0, 1);
  beta1 ~ beta(1.01, 1.01);
  gamma ~ beta(1.01, 1.01);
  xi ~ normal(0, 1.1);
  phi ~ normal(0, 1.5);
  tau_1 ~ normal(0, 0.5);
  tau_2 ~ normal(0, 0.5);
  sigma_rv ~ inv_gamma(10, 1);
  for (i in 1:T) {
    rv[i] ~ normal(mu_rv[i], sigma_rv);
  }
}

generated quantities {
  vector[T] log_lik;
  vector[T] y_rep;

  
  for (i in 1:T) {
    log_lik[i] = normal_lpdf(y[i] | mu, sigma[i]);
    y_rep[i] = normal_rng(mu, sigma[i]);
  }
}
"
#sso <- launch_shinystan(r_fit)
```

```{r add after vector loglik and for loop }
vector[T_out] r_out;
vector<lower=0>[T_out] rv_out;
vector<lower=0>[T_out] sigma_out;
vector[T_out] z_out;
vector[T_out] mu_rv_out;

sigma_out[1] = sqrt(alpha0
                    + beta1 * pow(sigma[T], 2)
                    + gamma * rv[T]);                   
r_out[1] = normal_lb_rng(mu, sigma_out[1], 0); 
z_out[1] = (r_out[1] - mu) / sigma_out[1];

mu_rv_out[1] = xi + phi * pow(sigma_out[1], 2) 
+ tau_1 * z_out[1] 
+ tau_2 * (pow(z_out[1], 2) - 1);
rv_out[1] = normal_lb_rng(mu_rv_out[1], sigma_rv, 0);

for (i in 2:T_out) {
  sigma_out[i] = sqrt(alpha0
                      + beta1 * pow(sigma_out[i-1], 2)
                      + gamma * rv_out[i-1]);                  
  r_out[i] = normal_lb_rng(mu, sigma_out[i], 0); 
  z_out[i] = (r_out[i] - mu)/sigma_out[i];
  mu_rv_out[i] = xi + phi * pow(sigma_out[i], 2) 
  + tau_1 * z_out[i] 
  + tau_2 * (pow(z_out[i], 2) - 1);
  rv_out[i] = normal_lb_rng(mu_rv_out[i], sigma_rv, 0); 
}
```

```{r #+# plots sd_9/30d_lead9/30d ~ vix9/30 two y axis}
  ##to_do### to analyze possible dependence of the current rv and future vix values
  # basically this is exactly what the model does
  
  
  subset_plot = SP_500_corr_df[((SP_500_corr_df$date>"2019-01-30") & (SP_500_corr_df$date<"2019-08-30")),
                               c("date", "vix_9d", "vix_30d", "sd_9d","sd_9d_lead_9d", "sd_30d_lead_9d")] 
  
  coeff = 4*max(subset_plot$vix_9d)/ max(subset_plot$sd_9d)
  ggplot(subset_plot, aes(date)) + 
    geom_line(aes(y = vix_9d, colour = "VIX_9d"), alpha = .6) + 
    geom_line(aes(y = sd_9d_lead_9d / coeff, colour = "sd_9d_lead_9d"), alpha = .3) +
    geom_line(aes(y = sd_9d/ coeff, colour = "sd_9d"), alpha = .6) +
    
    scale_y_continuous(
      # Features of the first axis
      name = "VIX_9d",
      # Add a second axis and specify its features
      sec.axis = sec_axis(~.*coeff, name="SD")) + 
    theme_ipsum() +
    theme(
      axis.title.y = element_text(color = "blue", size=12),
      axis.title.y.right = element_text(color = "red", size=12)) +
    ggtitle("some inference") 
  
  remove(subset_plot)
  ### take-aways
  # vix seems to be not forecasting/ leading even a point estimate of future RV
  # but instead current SD and RV seems to be forecasting vix
  
  
  ```
  
  ```{r FUNCTION build_vix9_rv_subset}
  build_vix9_rv_subset = function(start_date, end_date) { ### end_date including 1 extra month for T_out ###
    rv_vix = left_join(rv[,c("date", "rv5")], vix_df[,c("date", "vix_9d")], by = "date")
    rv_vix_subset = rv_vix[((rv_vix$date>=(as.Date(start_date) - days(1))) & (rv_vix$date<=end_date)),]
    rv_vix_subset$vix_log_ret = log_ret(rv_vix_subset$vix_9d)
    rv_vix_subset$vix_lin_ret = lin_ret(rv_vix_subset$vix_9d)
    return(rv_vix_subset[-c(1),])
  }
  ```
  
  ```{r FUNCTION quality_of_fit}
  quality_of_fit = function(fit, T_out, end_date, subset, subset_long, Rbsq_presence = FALSE) {
    qf_df = data.frame(matrix(vector(), T_out, 6, 
                              dimnames = list(c(), 
                                              c("date" ,"r_out", "r_truth", "price_r_out", 
                                                "price_r_truth", "price_truth"))), 
                       stringsAsFactors=F)
    for (i in 1:T_out) {
      temp_name = gsub(" ", "", paste("r_out[", as.character(i),"]", ""))
      r_out=extract(fit, pars=temp_name)[[1]]
      qf_df$r_out[i] = mean(r_out)
      
    }
    d = max(which(subset_long$date <= end_date))
    qf_df$date = subset_long$date[((d+1):(d+T_out))]
    qf_df$r_truth = subset_long$vix_lin_ret[((d+1):(d+T_out))]
    qf_df$price_truth = subset_long$vix_9d[((d+1):(d+T_out))]
    
    qf_df$price_r_truth[1] = (1 + qf_df$r_truth[1]) * subset_long$vix_9d[d]
    for (i in 2:T_out) {
      qf_df$price_r_truth[i] = (1 + qf_df$r_truth[i]) * qf_df$price_r_truth[i-1]
    }
    
    qf_df$price_r_out[1] = (1 + qf_df$r_out[1]) * subset_long$vix_9d[d]
    for (i in 2:T_out) {
      qf_df$price_r_out[i] = (1 + qf_df$r_out[i]) * qf_df$price_r_out[i-1]
    }
    
    MAE_ret = sum(abs(qf_df$r_truth - qf_df$r_out)) / T_out
    MAPE_ret = sum((100*abs(qf_df$r_truth - qf_df$r_out)/ qf_df$r_truth)) / T_out
    MSE_ret = sum((qf_df$r_truth - qf_df$r_out)^2) / T_out
    
    MAE_ind = sum(abs(qf_df$price_truth - qf_df$price_r_out)) / T_out
    MAPE_ind = sum((100*abs(qf_df$price_truth - qf_df$price_r_out)/ qf_df$price_truth)) / T_out
    MSE_ind = sum((qf_df$price_truth - qf_df$price_r_out)^2) / T_out
    ### to add MSE for actual and forecasted prices
    
    # WAIC and LOOC and Rbsq
    log_lik = extract_log_lik(fit, merge_chains = FALSE)
    r_eff = exp(relative_eff(log_lik))
    if (Rbsq_presence == TRUE) {
      RBSQ=extract(fit, pars="Rbsq")[[1]]
      Rbsq = mean(RBSQ)
    } else {Rbsq = NA}
    
    qf = list(qf_df, summary(fit), waic(log_lik), loo(log_lik, r_eff = r_eff), Rbsq,
              MAE_ret, MAPE_ret, MSE_ret, MAE_ind, MAPE_ind, MSE_ind) # add MSE
    (names(qf) = c("returns", "summary", "waic", "looic", "Rbsq",
                   "MAE_ret", "MAPE_ret", "MSE_ret", "MAE_ind", "MAPE_ind", "MSE_ind")) #add MSE
    return(qf)
    
  }
  
  ```
  
  ```{r FUNCTION forecast_vix}
  ##+## find a metric to assess quality of prediction
  ##+## code the metric to assess quality of prediction
  ##+## build a loop
  #+# chose a time interval
  #+# build a model
  #+# compare the model prediction with the truth (9d after the end of the interval)
  
  forecast_vix = function (model_code, start_date, subset_duration, 
                           step_size, n_steps, T_out, 
                           sigma_1, mu_1, iter, chains, 
                           Rbsq_presence, max_treedepth) {
    qf_list <- list()
    for (i in (1:n_steps)) {
      t_0 = as.Date(start_date) + months((i-1) * step_size)
      t_1 = as.Date(t_0) + months(subset_duration + 1) #interim end_date, for subset_long
      subset_long = build_vix9_rv_subset(t_0, t_1)
      t_1 = as.Date(t_0) + months(subset_duration) #stored end_date, for subset
      subset = build_vix9_rv_subset(t_0, t_1)
      
      T <- length(subset$vix_lin_ret)
      r <- subset$vix_lin_ret
      rv <- subset$rv5
      v = var(r)
      file_name = paste(model_name, as.character(Sys.Date()), 
                        "for", as.character(t_0), as.character(t_1), ".rda", sep=" ")
      
      stan_dat = list(T = T, rv = rv, y = r, mu_1 = mu_1, sigma_1 = sigma_1, T_out = T_out, v = v)
      r_fit = stan(model_code = model_code, data = stan_dat, 
                   iter = iter, chains = chains,
                   control = list(max_treedepth = max_treedepth))
      
      setwd('/Users/AM/Documents/_CU Masters/2020 fall Bayesian_7393/Final_Project/output')
      #temp_1 = readRDS("AR1 2020-11-23 for 2017-01-22 2017-07-22 .rda")
      saveRDS(r_fit, file = file_name, compress = "xz")
      
      qf = quality_of_fit(fit = r_fit, T_out = T_out, end_date = t_1, 
                          subset = subset, subset_long = subset_long, Rbsq_presence = Rbsq_presence)
      qf_list[[i]] <- qf
      names(qf_list)[i] = as.character(t_1) #end of sample and start of the forecast
    }
    return(qf_list)
  }  
  #sso <- launch_shinystan(r_fit)
  
  ```
  
  ```{r chain of forecasts AR1, include=FALSE}
  #### compare tables for different subset durations 
  
  model_code = stan_mod_AR1
  model_name = "AR1"
  start_date = "2017-01-22"
  subset_duration = 6 # in months, incl 1 extra month for T_out (validation)
  step_size = 2 # in months
  n_steps = 9 # 
  T_out <- 9 # forecast depth, days
  
  iter =10^4
  chains = 4
  max_treedepth = 10
  
  sigma_1 <- 0.1 
  mu_1 = 0
  
  f = forecast_vix(model_code = model_code, start_date = start_date, 
                   subset_duration = subset_duration, 
                   step_size = step_size,n_steps = n_steps, T_out = 9, 
                   sigma_1 = 0.1, mu_1 = mu_1, iter = iter, chains = chains, 
                   Rbsq_presence = TRUE, max_treedepth = max_treedepth) 
  
  p = data.frame(matrix(vector(), n_steps + 5, 20, 
                        dimnames = list(c(), 
                                        c("window_start", "window_end",
                                          "alpha", "alpha_2.5", "alpha_97.5",
                                          "beta", "beta_2.5", "beta_97.5",
                                          "sigma", "sigma_2.5", "sigma_97.5",
                                          "WAIC", "LOOIC", "Rbsq", 
                                          "MAE_ret", "MAPE_ret", "MSE_ret", 
                                          "MAE_ind", "MAPE_ind", "MSE_ind"))),
                 stringsAsFactors=F)
  
  for (i in 1:n_steps){
    p[i, "window_start"] = as.character(as.Date(names(f)[i]) - months(subset_duration))
    p[i, "window_end"] = as.character(as.Date(names(f)[i]))
    p[i, c("alpha", "alpha_2.5", "alpha_97.5")] = f[[i]]$summary$summary[,c("mean", "2.5%", "97.5%")][c("alpha"),]
    p[i, c("beta", "beta_2.5", "beta_97.5")] = f[[i]]$summary$summary[,c("mean", "2.5%", "97.5%")][c("beta"),]
    p[i, c("sigma", "sigma_2.5", "sigma_97.5")] = f[[i]]$summary$summary[,c("mean", "2.5%", "97.5%")][c("sigma"),]
    p[i, c("WAIC")] = f[[i]]$waic$waic
    p[i, c("LOOIC")] = f[[i]]$looic$looic
    p[i, c("Rbsq")] = f[[i]]$Rbsq
    p[i, c("MAE_ret", "MAPE_ret", "MSE_ret", "MAE_ind", "MAPE_ind", "MSE_ind")] = f[[i]][c("MAE_ret", "MAPE_ret", "MSE_ret", "MAE_ind", "MAPE_ind", "MSE_ind")]
    
  }
  
  p[n_steps + 1, "window_end"] = "MEAN"
  p[n_steps + 2, "window_end"] = "SE"
  p[n_steps + 3, "window_end"] = "SE, %"
  
  standard_error <- function(x, ...) sqrt(var(x, ...)/length(x))
  options(digits=3)
  for (i in 3:dim(p)[2]) {
    p[(n_steps + 1), i] = mean(p[1:n_steps, i])
    p[(n_steps + 2), i] = standard_error(p[1:n_steps, i])
    if (i>=3) {p[(n_steps + 3), i] = round(100 * p[(n_steps + 2), i] / p[(n_steps + 1), i], digits = 2)}
  }
  p[(n_steps + 4), 1] = "iter"
  p[(n_steps + 4), 2] = iter
  p[(n_steps + 5), 1] = "chais"
  p[(n_steps + 5), 2] = chains
  
  setwd('/Users/AM/Documents/_CU Masters/2020 fall Bayesian_7393/Final_Project/output')
  write.xlsx(p, 
             paste(model_name, as.character(Sys.time()), ".xlsx", sep="_"),
             sheetName=model_name)
  ```
  
  ```{r chain of forecasts ARCH1, include=FALSE}
  #### compare tables for different subset durations 
  
  model_code = stan_mod_ARCH1
  model_name = "ARCH1"
  start_date = "2017-01-22"
  subset_duration = 6 # in months, incl 1 extra month for T_out (validation)
  step_size = 2 # in months
  n_steps = 9 # number of different time windows (to build different models)
  T_out <- 9 # forecast depth, days
  
  iter = 10^4
  chains = 4
  max_treedepth = 10
  
  sigma_1 <- 0.1 
  mu_1 = 0
  
  f = forecast_vix(model_code = model_code, start_date = start_date, 
                   subset_duration = subset_duration, 
                   step_size = step_size,n_steps = n_steps, T_out = T_out, 
                   sigma_1 = sigma_1, mu_1 = mu_1, iter = iter, chains = chains, 
                   Rbsq_presence = FALSE, max_treedepth = max_treedepth) 
  p_col_namses = c("window_start", "window_end",
                   "mu", "mu_2.5", "mu_97.5",
                   "alpha0", "alpha0_2.5", "alpha0_97.5",
                   "alpha1", "alpha1_2.5", "alpha1_97.5",
                   "WAIC", "LOOIC", "Rbsq",
                   "MAE_ret", "MAPE_ret", "MSE_ret",
                   "MAE_ind", "MAPE_ind", "MSE_ind")
  p = data.frame(matrix(vector(), n_steps + 5, length(p_col_namses), 
                        dimnames = list(c(),p_col_namses)),
                 stringsAsFactors=F)
  
  for (i in 1:n_steps){
    p[i, "window_start"] = as.character(as.Date(names(f)[i]) - months(subset_duration))
    p[i, "window_end"] = as.character(as.Date(names(f)[i]))
    p[i, c("mu", "mu_2.5", "mu_97.5")] = f[[i]]$summary$summary[,c("mean", "2.5%", "97.5%")][c("mu"),]
    p[i, c("alpha0", "alpha0_2.5", "alpha0_97.5")] = f[[i]]$summary$summary[,c("mean", "2.5%", "97.5%")][c("alpha0"),]
    p[i, c("alpha1", "alpha1_2.5", "alpha1_97.5")] = f[[i]]$summary$summary[,c("mean", "2.5%", "97.5%")][c("alpha1"),]
    p[i, c("WAIC")] = f[[i]]$waic$waic
    p[i, c("LOOIC")] = f[[i]]$looic$looic
    #p[i, c("Rbsq")] = f[[i]]$Rbsq
    p[i, c("MAE_ret", "MAPE_ret", "MSE_ret", "MAE_ind", "MAPE_ind", "MSE_ind")] = f[[i]][c("MAE_ret", "MAPE_ret", "MSE_ret", "MAE_ind", "MAPE_ind", "MSE_ind")]
    
  }
  
  p[n_steps + 1, "window_end"] = "MEAN"
  p[n_steps + 2, "window_end"] = "SE"
  p[n_steps + 3, "window_end"] = "SE, %"
  
  standard_error <- function(x, ...) sqrt(var(x, ...)/length(x))
  options(digits=3)
  for (i in 3:dim(p)[2]) {
    p[(n_steps + 1), i] = mean(p[1:n_steps, i])
    p[(n_steps + 2), i] = standard_error(p[1:n_steps, i])
    if (i>=3) {p[(n_steps + 3), i] = round(100 * p[(n_steps + 2), i] / p[(n_steps + 1), i], digits = 2)}
  }
  p[(n_steps + 4), 1] = "iter"
  p[(n_steps + 4), 2] = iter
  p[(n_steps + 5), 1] = "chais"
  p[(n_steps + 5), 2] = chains
  setwd('/Users/AM/Documents/_CU Masters/2020 fall Bayesian_7393/Final_Project/output')
  write.xlsx(p, 
             paste(model_name, as.character(Sys.time()), ".xlsx", sep="_"),
             sheetName=model_name)
  ```
  
  ```{r chain of forecasts GARCH11, include=FALSE}
  #### compare tables for different subset durations 
  
  model_code = stan_mod_GARCH11
  model_name = "GARCH11"
  start_date = "2017-01-22"
  subset_duration = 6 # in months, incl 1 extra month for T_out (validation)
  step_size = 2 # in months
  n_steps = 2 # number of different time windows (to build different models)
  T_out <- 9 # forecast depth, days
  
  iter = 10^4
  chains = 4
  max_treedepth = 12
  
  sigma_1 <- 0.1 
  mu_1 = 0
  
  f = forecast_vix(model_code = model_code, start_date = start_date, 
                   subset_duration = subset_duration, 
                   step_size = step_size,n_steps = n_steps, T_out = T_out, 
                   sigma_1 = sigma_1, mu_1 = mu_1, iter = iter, chains = chains, 
                   Rbsq_presence = FALSE, max_treedepth = max_treedepth)
  p_col_namses = c("window_start", "window_end",
                   "mu", "mu_2.5", "mu_97.5",
                   "alpha0", "alpha0_2.5", "alpha0_97.5",
                   "alpha1", "alpha1_2.5", "alpha1_97.5",
                   "beta1", "beta1_2.5", "beta1_97.5",
                   "WAIC", "LOOIC", "Rbsq",
                   "MAE_ret", "MAPE_ret", "MSE_ret",
                   "MAE_ind", "MAPE_ind", "MSE_ind")
  p = data.frame(matrix(vector(), n_steps + 5, length(p_col_namses), 
                        dimnames = list(c(),p_col_namses)),
                 stringsAsFactors=F)
  
  for (i in 1:n_steps){
    p[i, "window_start"] = as.character(as.Date(names(f)[i]) - months(subset_duration))
    p[i, "window_end"] = as.character(as.Date(names(f)[i]))
    p[i, c("mu", "mu_2.5", "mu_97.5")] = f[[i]]$summary$summary[,c("mean", "2.5%", "97.5%")][c("mu"),]
    p[i, c("alpha0", "alpha0_2.5", "alpha0_97.5")] = f[[i]]$summary$summary[,c("mean", "2.5%", "97.5%")][c("alpha0"),]
    p[i, c("alpha1", "alpha1_2.5", "alpha1_97.5")] = f[[i]]$summary$summary[,c("mean", "2.5%", "97.5%")][c("alpha1"),]
    p[i, c("beta1", "beta1_2.5", "beta1_97.5")] = f[[i]]$summary$summary[,c("mean", "2.5%", "97.5%")][c("beta1"),]
    p[i, c("WAIC")] = f[[i]]$waic$waic
    p[i, c("LOOIC")] = f[[i]]$looic$looic
    #p[i, c("Rbsq")] = f[[i]]$Rbsq
    p[i, c("MAE_ret", "MAPE_ret", "MSE_ret", "MAE_ind", "MAPE_ind", "MSE_ind")] = f[[i]][c("MAE_ret", "MAPE_ret", "MSE_ret", "MAE_ind", "MAPE_ind", "MSE_ind")]
    
  }
  
  p[n_steps + 1, "window_end"] = "MEAN"
  p[n_steps + 2, "window_end"] = "SE"
  p[n_steps + 3, "window_end"] = "SE, %"
  
  standard_error <- function(x, ...) sqrt(var(x, ...)/length(x))
  options(digits=3)
  for (i in 3:dim(p)[2]) {
    p[(n_steps + 1), i] = mean(p[1:n_steps, i])
    p[(n_steps + 2), i] = standard_error(p[1:n_steps, i])
    if (i>=3) {p[(n_steps + 3), i] = round(100 * p[(n_steps + 2), i] / p[(n_steps + 1), i], digits = 2)}
  }
  p[(n_steps + 4), 1] = "iter"
  p[(n_steps + 4), 2] = iter
  p[(n_steps + 5), 1] = "chais"
  p[(n_steps + 5), 2] = chains
  
  setwd('/Users/AM/Documents/_CU Masters/2020 fall Bayesian_7393/Final_Project/output')
  write.xlsx(p, 
             paste(model_name, as.character(Sys.time()), ".xlsx", sep="_"),
             sheetName=model_name)
  ```
  
  ```{r chain of forecasts RealGARCH11, include=FALSE}
  #### compare tables for different subset durations 
  
  model_code = stan_mod_RealGARCH11
  model_name = "RealGARCH11"
  start_date = "2017-05-22"
  subset_duration = 6 # in months, incl 1 extra month for T_out (validation)
  step_size = 2 # in months
  n_steps = 1 # number of different time windows (to build different models)
  T_out <- 9 # forecast depth, days
  
  iter = 10^4
  chains = 3
  max_treedepth = 35
  
  sigma_1 <- 0.1 # sqrt(var(lin_vix_ret)) > sqrt(var(rv_vix_subset$vix_lin_ret[-c(1)]))
  mu_1 = 0
  
  f = forecast_vix(model_code = model_code, start_date = start_date, 
                   subset_duration = subset_duration, 
                   step_size = step_size,n_steps = n_steps, T_out = T_out, 
                   sigma_1 = sigma_1, mu_1 = mu_1, iter = iter, chains = chains, 
                   Rbsq_presence = FALSE, max_treedepth = max_treedepth) 
  p_col_namses = c("window_start", "window_end",
                   "mu", "mu_2.5", "mu_97.5",
                   "alpha0", "alpha0_2.5", "alpha0_97.5",
                   "beta1", "beta1_2.5", "beta1_97.5",
                   "gamma", "gamma_2.5", "gamma_97.5",
                   "xi", "xi_2.5", "xi_97.5",
                   "phi", "phi_2.5", "phi_97.5",
                   "tau_1", "tau_1_2.5", "tau_1_97.5",
                   "tau_2", "tau_2_2.5", "tau_2_97.5",
                   "WAIC", "LOOIC", "Rbsq",
                   "MAE_ret", "MAPE_ret", "MSE_ret",
                   "MAE_ind", "MAPE_ind", "MSE_ind")
  p = data.frame(matrix(vector(), n_steps + 5, length(p_col_namses), 
                        dimnames = list(c(),p_col_namses)),
                 stringsAsFactors=F)
  
  for (i in 1:n_steps){
    p[i, "window_start"] = as.character(as.Date(names(f)[i]) - months(subset_duration))
    p[i, "window_end"] = as.character(as.Date(names(f)[i]))
    p[i, c("mu", "mu_2.5", "mu_97.5")] = f[[i]]$summary$summary[,c("mean", "2.5%", "97.5%")][c("mu"),]
    p[i, c("alpha0", "alpha0_2.5", "alpha0_97.5")] = f[[i]]$summary$summary[,c("mean", "2.5%", "97.5%")][c("alpha0"),]
    p[i, c("beta1", "beta1_2.5", "beta1_97.5")] = f[[i]]$summary$summary[,c("mean", "2.5%", "97.5%")][c("beta1"),]
    p[i, c("gamma", "gamma_2.5", "gamma_97.5")] = f[[i]]$summary$summary[,c("mean", "2.5%", "97.5%")][c("gamma"),]
    p[i, c("xi", "xi_2.5", "xi_97.5")] = f[[i]]$summary$summary[,c("mean", "2.5%", "97.5%")][c("xi"),]
    p[i, c("phi", "phi_2.5", "phi_97.5")] = f[[i]]$summary$summary[,c("mean", "2.5%", "97.5%")][c("phi"),]
    p[i, c("tau_1", "tau_1_2.5", "tau_1_97.5")] = f[[i]]$summary$summary[,c("mean", "2.5%", "97.5%")][c("tau_1"),]
    p[i, c("tau_2", "tau_2_2.5", "tau_2_97.5")] = f[[i]]$summary$summary[,c("mean", "2.5%", "97.5%")][c("tau_2"),]
    p[i, c("WAIC")] = f[[i]]$waic$waic
    p[i, c("LOOIC")] = f[[i]]$looic$looic
    #p[i, c("Rbsq")] = f[[i]]$Rbsq
    p[i, c("MAE_ret", "MAPE_ret", "MSE_ret", "MAE_ind", "MAPE_ind", "MSE_ind")] = f[[i]][c("MAE_ret", "MAPE_ret", "MSE_ret", "MAE_ind", "MAPE_ind", "MSE_ind")]
    
  }
  
  p[n_steps + 1, "window_end"] = "MEAN"
  p[n_steps + 2, "window_end"] = "SE"
  p[n_steps + 3, "window_end"] = "SE, %"
  
  standard_error <- function(x, ...) sqrt(var(x, ...)/length(x))
  options(digits=3)
  for (i in 3:dim(p)[2]) {
    p[(n_steps + 1), i] = mean(p[1:n_steps, i])
    p[(n_steps + 2), i] = standard_error(p[1:n_steps, i])
    if (i>=3) {p[(n_steps + 3), i] = round(100 * p[(n_steps + 2), i] / p[(n_steps + 1), i], digits = 2)}
  }
  p[(n_steps + 4), 1] = "iter"
  p[(n_steps + 4), 2] = iter
  p[(n_steps + 5), 1] = "chais"
  p[(n_steps + 5), 2] = chains
  
  
  setwd('/Users/AM/Documents/_CU Masters/2020 fall Bayesian_7393/Final_Project/output')
  write.xlsx(p, 
             paste(model_name, as.character(Sys.time()), ".xlsx", sep="_"),
             sheetName=model_name)
  ```
  
  ```{r prediction of absolute numbers, include=FALSE}
  start_date = "2017-01-22"
  subset_duration = 18 # in months, incl 1 extra month for T_out (validation)
  step_size = 2 # in months
  n_steps = 1
  t_0 = as.Date(start_date)
  t_1 = as.Date(t_0) + months(subset_duration) #stored end_date, for subset
  subset = build_vix9_rv_subset(t_0, t_1)
  
  #subset$vix_9d_out = NA
  temp = data_frame()
  for (i in 1:9) {
    temp = rbind(temp, f_GARCH[[i]]$returns[,c("date", "price_r_out")])
  }
  subset = left_join(subset, temp, by = "date")[,c(1,3,6)]
  names(subset)[3] = "vix_9d_predicted"
  names(subset)[2] = "vix_9d_true"
  
  
  rv_long <- melt(subset, id="date")
  ggplot(data=rv_long, aes(x=date, y=value, colour=variable)) + 
    geom_line(alpha=.9) +
    theme_ipsum() +
    geom_vline(xintercept = as.Date(c("2017-07-22", "2017-09-22", "2017-11-22", "2018-01-22", "2018-03-22", "2018-05-22")), 
               show.legend = T, colour = "steel blue", linetype="dotted") +
    #ylim(0,0.0007) +
    scale_x_date(limit=c(as.Date("2017-07-15"),as.Date("2018-06-20"))) + 
    theme(legend.position="bottom")
  remove(rv_long)
  ```
  
  