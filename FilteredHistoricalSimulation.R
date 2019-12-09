
#### This code illustrates a new method for estimating VAR: Filtered Historical Simulation


rm(list=ls())                                  ### Clean the R workspace

sp500 <- read.csv('D:/r/r code/S&P500.csv', header=TRUE)       ### Load the daily return data of SP500 index
price <- sp500$Close                           ### Extract the price information

T     <- length(price)

ret   <- log(price[2:T]) - log(price[1:(T-1)]) ### Calculate the log return 

ind   <- which(ret!=0)                         ### Only keep those returns that are not zero (i.e. not on holidays)

ret   <- ret[ind]
T     <- length(ret)



#### Filtered Historical simulation
library('fGarch')
var1_hs  <- numeric(T)
var1_fhs  <- numeric(T)
var1_garch <- numeric(T)


for (i in 251:T){
  retwindow   <- ret[(i-250):(i-1)]
  var1_hs[i] <- -quantile(retwindow, probs=0.01, na.rm=TRUE)
  
  fit2 <- garchFit( formula = ~garch(1, 1), data = retwindow, trace = FALSE)
  sigmapred <- predict(fit2, n.ahead=1)$standardDeviation
  sigma <- sqrt(fit2@h.t)
  retstand <- retwindow/sigma
  var1_fhs[i]  <- -sigmapred*quantile(retstand, probs=0.01)
  
  var1_garch[i] <- -qnorm(0.01, mean=0, sd=sigmapred)
}

plot(var1_fhs, col='red', type='l', ylim=c(0,0.15), lwd=4)
points(var1_garch, col='blue', type='l', lwd=4)













