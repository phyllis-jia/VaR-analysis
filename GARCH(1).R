

### Maximumum likelihood estimation on SP500
rm(list=ls())
#install.packages('fGarch')
library('fGarch')

Ret   <- diff(log(sp500$Close))
retwindow <- Ret[T-50:T]
fit2 <- garchFit( formula = ~garch(1, 1), data = retwindow, trace = FALSE)
sigma <- fit2@sigma.t
plot(sigma, type='l')

T <- length(Ret)
omega <- fit2@fit$coef[2]
alpha <- fit2@fit$coef[3]
beta  <- fit2@fit$coef[4]

sqrt(omega + alpha*Ret[T]^2 + beta*sigma[T]^2)




### Maximum likelihood estimation for AAPL
### Download stock prices from Yahoo Finance
rm(list=ls())
library(quantmod)
getSymbols('AAPL', from ="2000-01-03", to = "2017-02-21")
AAPL <- AAPL$AAPL.Adjusted
Ret <- diff(log(AAPL))
Ret <- Ret[-1,]
T   <- length(Ret)
retwindow <- Ret[T-10:T]
fit3 <- garchFit( formula = ~garch(1, 1), data = retwindow, trace = FALSE)
sigma <- sqrt(fit3@h.t)
plot(sigma, type='l')




### Garch simulation and maximum likelihood estimation

rm(list=ls())
library('fGarch')
spec   <- garchSpec(model = list(omega = 1e-6, alpha = 0.1, beta=0.8))
dd     <- garchSim(spec, n = 100000)
fit1   <- garchFit( formula = ~garch(1,2), data=dd, trace=FALSE)
sigma  <- sqrt(fit1@h.t)




