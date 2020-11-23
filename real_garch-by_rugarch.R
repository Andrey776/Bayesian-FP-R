library(lubridate)

library(rugarch)
library(xts)


ret <- replicate(1, rnorm(100))
RV <- replicate(1, rnorm(100))
date <- c(1:100)


times<-Sys.time()+date
#ret_xts<-xts(ret,order.by = times)
#RV_xts <- xts(RV,order.by = times)
#subset$date_time = as_datetime(subset$date)

subset1=subset[-(c(1)),]
subset1$date = as_datetime(subset1$date)

subset$vix_lin_ret[2]*2
class(subset1$date[2])

ret_xts = xts(subset1$vix_lin_ret, order.by = subset1$date)
RV_xts =  xts(subset1$rv5, order.by = subset1$date)

ret_xts[2]*2
class(ret_xts[2])  
  
attspec <- ugarchspec(mean.model = list(armaOrder = c(0, 0), include.mean = FALSE), 
                      variance.model = list(model = 'realGARCH', garchOrder = c(1, 1)))

fit <- ugarchfit(spec=attspec, data=ret_xts, 
                 solver = 'hybrid', realizedVol = RV_xts)

show(fit)
