######################
rm(list = ls()) # remove objects from global environment

#Example 1: Portfolio variance and VaR
#Consider a $1,000,000 portfolio with 60% in GOOG and 40% in AAPL. 
#Assume that annualized volatilities of GOOG and AAPL are 20% and 25% correspondingly. 
#Assume that the correlation is 0.70.  Find the portfolio volatility and 1% VaR assuming Normal distribution.  
p=.01
value=1000000
w1=.60
w2=.40
sig1=20
sig2=25
rho=.70
#Compute portfolio standard deviation
sigp=sqrt(w1^2*sig1^2+w2^2*sig2^2+2*w1*w2*rho*sig1*sig2)
sigp

# Use matrix form to compute portfolio standard deviation
w=c(w1,w2)
sig12=rho*sig1*sig2
SIG=matrix(c(sig1^2,sig12,sig12,sig2^2), nrow=2,ncol=2)
SIG

sigp=sqrt(t(w)%*%SIG%*%w) # matrix multiplication
sigp

#compute 1 day volatility from annualized for VaR:
sigp=sigp/sqrt(252)
sigp

#Compute VaR
VaR_norm = - qnorm(p) *sigp/100 * value # divide by 100 since sigp is in percentage
VaR_norm

##############
rm(list = ls()) # remove objects from global environment

#Example 2: Using Factor Models for Variance
#Find the volatilities of two stocks (GOOG and AAPL) and correlation between two stocks with betas and annualized idiosyncratic volatilities and market volatility given by:	
beta1=1.2
beta2=1.5	
sig_error1=10 
sig_error2=15	
sig_market =15

# Compute volatilities using equation (8.3)
sig1 =sqrt(beta1^2*sig_market^2+sig_error1^2)
sig1
sig2 =sqrt(beta2^2*sig_market^2+sig_error2^2)
sig2
# Compute correlation using equation (8.4)
rho=	beta1*beta2*sig_market^2/(sig1*sig2)
rho

# Use matrix form to compute covariance matrix for two stocks equation (8.5)
beta=cbind(beta1,beta2)
SIGF=sig_market^2
SIG_error=diag(c(sig_error1^2,sig_error2^2)) #diagonal error variance matrix 
SIG=t(beta)%*%SIGF%*%beta+SIG_error 
SIG
sqrt(diag(SIG))

rho=SIG[1,2]/sqrt(SIG[1,1]*SIG[2,2])
rho

############################
### Intro to Monte Carlo
############################

rm(list = ls()) # remove objects from global environment

# Create plots of Normal density and Cumulative Distribution. set values between -3 and 3  
x=seq(from=-3,to=3, by=.1)  # sequence of n numbers between -3 and 3.
p=dnorm(x,mean=0,sd=1)   #find Normal PDF of x
P=pnorm(x,mean=0,sd=1)  #find Normal CDF of x
plot(x,p,type='l') # plot distribution with lines connecting points
title('PDF of the Standard Normal Distribution')
plot(x,P, type='l')
title('CDF of the Standard Normal Distribution')

# Monte Carlo simulations
# 1. Simple univariate examples
  #Generation of Random Variables and Distributions

# Standard Normal Distribution
set.seed(12345) #set seed so that monte carlo simulations are the same each time you run the code
n=1000
r=rnorm(n, mean=0, sd=1) # simulate n=1000 standard normal random variables
r=rnorm(n) # same: simulate 1000 independent realizations from a standard normal distribution
plot(r,type='l')
title('Generated Normal Random Variables')
library(fBasics)
basicStats(r)


# Student t distribution
set.seed(12345)
n=1000
df=5
r=rt(n, df) #generate n Student t random variables with nf degrees of freedom
plot(r,type='l')
title('Generated t Random Variables')
library(fBasics)
basicStats(r)

# Uniform distribution
set.seed(12345)
n=1000
r=runif(n, min=0, max=1) # generate n uniform random variables between 0 and 1
plot(r,type='l')
title('Generated Uniform Random Variables')
library(fBasics)
basicStats(r)

# Monte Carlo simulations
# 2. Multivariate examples
#Generation of Random Variables and Distributions

# Multivariate Standard Normal Distribution with independendent variables
n=1000
mu=c(0,0)
SIG=matrix(c(1,0,0,1),2,2) # here correlation is equal 0
SIG
library(MASS)
set.seed(12345) #set seed so that monte carlo simulations are the same each time you run the code
r=mvrnorm(n,mu,SIG) # generate n=1000 simulations for two random variables
dim(r)
plot(r[,1],type='l',col=1)
lines(r[,2], type='l', col=2)
title('Generated Independent Normal Random Variables')
plot(as.data.frame(r)) #scatterplot
title('Generated Independent Normal Random Variables ')

library(fBasics)
basicStats(r)
cov(r) # covariance 
cor(r) # correlation (here cov and cor are the same since we used unit variances for random variables)

# Multivariate  Normal Distribution with dependendent variables
n=1000
mu=c(0,0)# or rep(0,2)
SIG=matrix(c(1,0.8,0.8,1),2,2) # here correlation is equal 0.8
SIG

library(MASS)
set.seed(12345) #set seed so that monte carlo simulations are the same each time you run the code
r=mvrnorm(n,mu,SIG) # generate n=1000 simulations for two random variables
dim(r)
plot(r[,1],type='l',col=1)
lines(r[,2], type='l', col=2)

plot(as.data.frame(r)) #scatterplot
title('Correl=0.8, Generated  Normal RV')

library(fBasics)
basicStats(r)
cov(r) # covariance 
cor(r) # correlation is close to 0.8

############################
####Monte Carlo for actual portfolio of returns
##portfolio of GOOG and AAPL actual returns
############################

library(quantmod)  # <=== load the package "quantmod"
library(timeSeries)
getSymbols("AAPL",from="2010-01-01",to="2018-08-13") # <== specify the data span
getSymbols("GOOG",from="2010-01-01",to="2018-08-13") # <== specify the data span
AAPL=last(AAPL, "4 years") # select the last 4 years of data
GOOG=last(GOOG, "4 years") # select the last 4 years of data

ret_AAPL=diff(log(AAPL$AAPL.Adjusted))*100 # returns are in percentage 
ret_GOOG=diff(log(GOOG$GOOG.Adjusted))*100 # returns are in percentage 
ret_AAPL=na.omit(ret_AAPL) # remove missing values NA
ret_GOOG=na.omit(ret_GOOG) # remove missing values NA



nsim=10000
w=c(0.6,0.4) #portfolio weights
# mean returns could be included or ignored
#mu=cbind( mean(ret_GOOG),mean(ret_AAPL))
mu=c(0,0)
# daily covariance matrix
SIG=cov(cbind(ret_GOOG,ret_AAPL)) # covariance between GOOG and AAPL
SIG
# Correlation between GOOG and AAPL
cor(cbind(ret_GOOG,ret_AAPL)) 

plot(as.data.frame(cbind(ret_GOOG,ret_AAPL))) #scatterplot
plot(cbind(ret_GOOG,ret_AAPL)) #plot over time

library(MASS)
set.seed(12345) #set seed so that monte carlo simulations are the same each time you run the code
sim_returns=mvrnorm(nsim,mu,SIG) # generate nsim=10000 scenarios for two stocks
sim_portfolio= sim_returns%*%w  #compute portfolio returns for nsim scenarios

# Find VaR for $1,000,000 portfolio
value=1000000
VaR=-quantile(sim_portfolio,p=0.01)/100*value # divide by 100 since returns are in percentage
VaR

