M<-cor(SP_500_corr_df[,-c(1)])
corrplot(M, type = "upper", tl.col = "steel blue")


library(invgamma)
x = seq(0.0001, 1, length= 10000)
plot(x, dinvgamma(x, shape =10, scale = 4), type = "l", col = "red")
lines(x, dinvgamma(x, shape =10, scale = 2), col = "blue")
lines(x, dinvgamma(x, shape =10, scale = 1), col = "green")

dinv