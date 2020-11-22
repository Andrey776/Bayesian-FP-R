
library(rugarch)
library(xts)


ret <- replicate(1, rnorm(100))
RV <- replicate(1, rnorm(100))
date <- c(1:100)


times<-Sys.time()+date
#ret_xts<-xts(ret,order.by = times)
#RV_xts <- xts(RV,order.by = times)
r <- subset$vix_lin_ret[-(c(1))]
x <- subset$rv5[-(c(1))]

ret_xts = xts(r)
RV_xts = xts(x)

attspec <- ugarchspec(mean.model = list(armaOrder = c(0, 0), include.mean = FALSE), variance.model = list(model = 'realGARCH', garchOrder = c(1, 1)))

fit <- ugarchfit(spec=attspec, data=ret_xts, solver = 'hybrid', realizedVol = RV_xts)
show(fit)
