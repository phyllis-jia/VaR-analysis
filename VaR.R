


rm(list=ls())                                  ### Clean the R workspace

sp500 <- read.csv('sp500.csv', header=TRUE)       ### Load the daily return data of SP500 index
price <- sp500$Close                           ### Extract the price information

T     <- length(price)

ret   <- log(price[2:T]) - log(price[1:(T-1)]) ### Calculate the log return 

ind   <- which(ret!=0)                         ### Only keep those returns that are not zero (i.e. not on holidays)

ret   <- ret[ind]
T     <- length(ret)



#### How much does the initial value matter in the RiskMetrics model?
sigma2_A  <- numeric(T)
sigma2_B  <- numeric(T)
sigma2_C  <- numeric(T)
sigma2_B[1] <- 0.0134^2
sigma2_C[1] <- 0.05^2


for (i in 2:T){
  sigma2_A[i] <- 0.94*sigma2_A[i-1] + 0.06*ret[i-1]^2
  sigma2_B[i] <- 0.94*sigma2_B[i-1] + 0.06*ret[i-1]^2
  sigma2_C[i] <- 0.94*sigma2_C[i-1] + 0.06*ret[i-1]^2
  }

plot(sigma2_A, col='black', type='l')
points(sigma2_B, col='blue', type='l')
points(sigma2_C, col='red', type='l')








#### Parametric VaR with the RiskMetrics Model
#### Reproduce Figure 1.5

sigma2 <- numeric(T)
var1   <- numeric(T)

for (i in 2:T){
  sigma2[i] <- 0.94*sigma2[i-1] + 0.06*ret[i-1]^2
  var1[i]   <- -qnorm(0.01, mean=0, sd=sqrt(sigma2[i]))
}

plot(var1, col='black', type='l', ylim=c(0,0.14))








#### Historical simulation
var1_250  <- numeric(T)      
var5_250  <- numeric(T)

for (i in 251:T){
  var1_250[i] <- -quantile(ret[(i-250):(i-1)], probs=0.01) 
  var5_250[i] <- -quantile(ret[(i-250):(i-1)], probs=0.05)   
}

plot(var1_250, col='red', type='l', ylim=c(0,0.1))
points(var5_250, col='blue', type='l')






#### What if you use a window of 1000 days?
var1_1000  <- numeric(T)

for (i in 1001:T){
  var1_1000[i] <- -quantile(ret[(i-1000):(i-1)], probs=0.01) 
}

plot(var1_250, col='red', type='l', ylim=c(0,0.1))
points(var1_1000, col='blue', type='l')









