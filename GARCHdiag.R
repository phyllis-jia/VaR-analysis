


### Diagnostic check of GARCH models: Autocorrelation of standardized returns

rm(list=ls())
library('fGarch')
sp500 <- read.csv('SP500.csv', head=TRUE)
Ret   <- diff(log(sp500$Close))

fit2 <- garchFit( formula = ~garch(1, 1), data = Ret, trace = FALSE)
sigma <- sqrt(fit2@h.t)

Retstand <- Ret/sigma
acf(Ret^2, 20)
acf(Retstand^2, 20)



