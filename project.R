rm(list=ls()) 
##question1.1
install.packages('Ecdat')
library(SP500, package='Ecdat')
##question1.2
install.packages('fGarch')
library('fGarch')
Ret <- SP500$r500
Retwindow <- Ret[1305:1804]
fit2 <- garchFit(formula= ~garch(1,1),data=Retwindow, trace=FALSE)
sigma <- fit2@sigma.t
### 4 parameters from the model
mu    <- fit2@fit$coef[1]
omega <- fit2@fit$coef[2]
alpha <- fit2@fit$coef[3]
beta  <- fit2@fit$coef[4]
###mu=0.001273546, omega=8.440923e-06,alpha=0.07168575,beta=0.8470616 
##question1.3
plot(sigma, type='l')
##question1.4
sigmapred <- sqrt(omega + alpha*Retwindow[500]^2 + beta*sigma[500]^2)
probability <- pnorm(-0.228,mean=0,sd=sigmapred)
###Conditional probability is 1.608031e-30
##question1.5
Retstand <- Retwindow/sigma
Retstandsquare <- Retstand^2
plot(Retstand, type='l')

par(mfrow=c(2,2))
autocorr <- acf(Retstand,100)$acf[2:101]
plot(autocorr, type='l', col='blue')
abline(0,0,col='red')
autocorr <- acf(Retstandsquare,100)$acf[2:101]
plot(autocorr, type='l', col='blue')
abline(0,0,col='red')
###Yes, it fits well. 
###Because the acf of return square is nonstationary and the acf of Retstand square, which is standardlized by Garch modle, is stationary. 
###So we can say the Garch(1,1) model fits well.
##question1.6
Retstand <- Retwindow/sigma
qqnorm(Retstand,main='standardlized S&P500 Returns')
qqline(Retstand,col='red')
###No, it isn't normal distribution. It has fatter tail. Showing on the plot, the left side is lower than and the right side is higher than the normal distribution line.
##question1.7
fit3 <- garchFit( formula = ~garch(1, 1), data = Retwindow, trace = FALSE, cond.dist='std')
###New omega, alpha, beta and shape(degree of freedom) 
omega1 <- fit3@fit$coef[2]
alpha1 <- fit3@fit$coef[3]
beta1  <- fit3@fit$coef[4]
shape1 <- fit3@fit$coef[5]
T <- length(Retwindow)
var1_fhs  <- numeric(T)
sigmapred1 <- predict(fit2, n.ahead=1)$standardDeviation
quantile <- (-0.228/sigmapred1)*sqrt(shape1/(shape1-2))
pt(quantile,shape1)
###conditional probablity is 2.305818e-05 

  