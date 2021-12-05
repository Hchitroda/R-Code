#################################################################################
#Load Data from yahoo.finance
library(quantmod)  # <=== load the package "quantmod"
getSymbols("^DJI",from="1985-01-01",to="2018-08-20") # <== specify the data span
ret_DJ=diff(log(DJI[,6]))
library(timeSeries)
ret_DJ=na.omit(ret_DJ)

library(fBasics)
y=as.timeSeries(ret_DJ)
histPlot(y)
densityPlot(y)

qqnorm(y, xlab='Normal quantile', ylab='empirical quantile')
qqline(y, col="blue")
################################################################################
## Value at Risk
value=1000000 #VaR computed for $1million portfolio. Multiply by the value of portfolio  	
p=0.01 # probablity for VaR


#Historical quantile for last 1 years
y=last(ret_DJ, '252 days') # select the last 1 years of data using 252 days
quantile(y,p)
VaR1 = -quantile(y,p)*value	
VaR1


#Historical quantile for last 2 years
y=last(ret_DJ, "504 days") # select the last 2 years of data using 504 obs
quantile(y,p)
VaR2 = -quantile(y,p)*value	
VaR2

#Historical quantile for last 10 years
y=last(ret_DJ, "10 years") # select the last 10 years of data
quantile(y,p)
VaR3 = -quantile(y,p)*value	
VaR3

#Historical quantile for the whole sample
y=ret_DJ
#quantile(y,p)
VaR4 = -quantile(y,p)*value	
VaR4


# Expected Shortfall ES
y=coredata(y)
ysort = sort(y) # sort returns
np = round(length(y)*p)     # p percent smallest
#VaR4 = -ysort[np]*value
#VaR4
ES4 = -mean(ysort[1:np])*value
ES4

# Volatility Based VaR
# Use all of the data to estimate GARCH

library(fGarch)
#GARCH(1,1)volatility estimate with Normal Distr
m1=garchFit(~garch(1,1),data=y,trace=F, include.mean=FALSE)
m1
k=1
M1=predict(m1,k) # k day forecast
#M1
sig=M1$standardDeviation[1]
sig
#par=m1@fit$matcoef				
#s2=par[1]+par[2]*y[n]^2+par[3]*m1@h.t[n]
#sig=sqrt(s2)
VaR_norm = - qnorm(p) *sig *  value
VaR_norm

# GARCH(1,1) volatility estimate with t-distribution
m2=garchFit(~garch(1,1),data=y,trace=F, include.mean=FALSE,cond.dist="std")
m2
k=1
M2=predict(m2,k) # k day forecast
#M2
sigt=M2$standardDeviation[1]
sigt

parameters=m2@fit$matcoef[,1]
parameters
df=parameters[4]
multiplier=- qt(p,df)/sqrt(df/(df-2))
multiplier
VaR_t = multiplier *sigt *  value
VaR_t

#GARCH(1,1)volatility estimate with bootstap residuals (Hull and White)
res=y/sqrt(m1@h.t)
quantile(res,p)
VaR_boot = -quantile(res,p)*sig*value  
VaR_boot

# ES or CVaR with bootstrap residuals
# Expected Shortfall ES
res_sort=sort(coredata(res)) # sort res
np = round(length(res_sort)*p)     # p percent smallest
ES_6 = -mean(res_sort[1:np])*sig*value
ES_6

#GJR-GARCH  volatility estimate with Normal Distr
#install.packages("rugarch")
library(rugarch)
#spec.gjrGARCH = ugarchspec(variance.model=list(model="gjrGARCH", garchOrder=c(1,1)), mean.model=list(armaOrder=c(1,1), include.mean=TRUE), distribution.model="std")
spec.gjrGARCH = ugarchspec(variance.model=list(model="gjrGARCH", garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0), include.mean=F))
m3 <- ugarchfit(data=y, spec=spec.gjrGARCH)
m3
M3 = ugarchforecast(m3, n.ahead=10)
M3
#head(sigma(M3))
#head(fitted(M3))
#plot(M3,which="all")
s=sigma(M3)[1]
s
VaR5 = -s * qnorm(p) *  value
VaR5


#GJR-GARCH(1,1)volatility estimate with bootstap residuals VaR
res=y/sigma(m3)
VaR6 = -quantile(res,p)*s*value  
VaR6




######Multi-Day VaR##########
VaR_norm10 = - sqrt(10)*qnorm(p) *sig *  value
VaR_norm10

#####Backtesting####################

###Backtesting for DJ index in the last 10 years
library(quantmod)  # <=== load the package "quantmod"
getSymbols("^DJI",from="1985-01-01",to="2018-08-20") # <== specify the data span
ret_DJ=diff(log(DJI$DJI.Adjusted))*100
library(timeSeries)
ret_DJ=na.omit(ret_DJ) # remove missing values NA

y=last(ret_DJ, "15 years") # select the last 15 years of data
dates=index(y)
y=coredata(y)       #remove the time column from the y data

#Set backtest up 
value=1 #VaR computed for $1 portfolio 	
p=0.01 # probablity for VaR

T = length(y)		# about 2000obs or about 10 years. Set more than 4 years (1000obs) for testing window. T=TE+1000 or more	
TE = 1000   #estimation period					
np = round(TE*p)     # p percent smallest
VaR = matrix(nrow=T,ncol=4)	


lambda = 0.94 # set smoothing parameter for EWMA model
library("MTS")
m1=EWMAvol(y[1:TE], lambda = 0.94) # this is EWMA model with smoothing .94
sig2_ewma=m1$Sigma.t[TE] ##estimated daily variance at time TE

library(fGarch)
# Rolling Window estimation of VaR  
for (t in (TE+1):T){
  t1 = t-TE;								
  t2 = t-1;									
  yt = y[t1:t2] 
  
  #Riskmetrics (EMWA) volatility estimate with Normal Distribution
  sig2_ewma	 = lambda * sig2_ewma  + (1-lambda) * y[t-1]^2 # update forecast one day
  VaR[t,1] = -qnorm(p) * sqrt(sig2_ewma)/100 * value 
  
  
  #GARCH(1,1)volatility estimate with Normal Distribution
  m2=garchFit(formula = ~ garch (1,1), yt ,trace=FALSE, include.mean=FALSE)
  sig2_garch=predict(m2,1)$standardDeviation # 1 day forecast
  VaR[t,2] = -qnorm(p)*sqrt(sig2_garch)/100*  value
  
  #GARCH(1,1)volatility estimate with Student t-distribution
  m3=garchFit(formula = ~ garch (1,1), yt ,include.mean=FALSE,cond.dist="std",trace=F)
  sig2_garcht=predict(m3,1)$standardDeviation # 1 day forecast
  df=m3@fit$matcoef[,1][4] # degrees of freedom for Student t
  VaR[t,3]= - qt(p,df)/sqrt(df/(df-2))*sig2_garcht/100 * value
  
  #GARCH(1,1)volatility estimate with bootstap residuals VaR
  res=yt/sqrt(m2@h.t)
  VaR[t,4] = -quantile(res,p)*sig2_garch/100*value  
}


# Backtesting analysis 
# Violation ratio if >1.5 underestimated risk actual VaR>1% (undercapitalized), if Violation ratio<0.5 overestimated risk (overcapitalized) )
for (i in 1:4){
  VR = sum(y[(TE+1):T]/100< -VaR[(TE+1):T,i])/(p*(T-TE)) 
  m=c("EWMA-Normal",  "GARCH-Normal","GARCH-t","GARCH-boot")
  cat(i,m[i],"VR=",VR,"\n")		
}


#ret_var=cbind(y/100,-VaR)
#ret_var=xts(ret_var,dates)

dates=dates[(TE+1):T]
ret_var=cbind(y[(TE+1):T]/100,-VaR[(TE+1):T,])
ret_var=xts(ret_var,dates)

#ret_DJ=xts(y[(TE+1):T]/100,dates)
#VaR_ts=xts(VaR[(TE+1):T,],dates)

plot(ret_var,  main='')
title("VaR Backtesting")
legend("right",legend=c( "EWMA",  "GARCH","GARCH_t","GARCH_bt"),lty=2:5,col=2:5,horiz=F)

#matplot(cbind(y/100,-VaR),type='l',col=1:5,las=1,ylab="",lty=1:5)
#legend("topleft",legend=c("Returns","EWMA",  "GARCH","GARCH_t","GARCH_bt"),lty=1:5,col=1:5,bty="1")

#matplot(cbind(y/100,-VaR[,1:2]),type='l',col=1:3,las=1,ylab="",lty=1:3)
#legend("topleft",legend=c("Returns","EWMA",  "GARCH"),lty=1:3,col=1:3,bty="1")

#matplot(cbind(y/100,-VaR[,3:4]),type='l',col=1:3,las=1,ylab="",lty=1:3)
#legend("topleft",legend=c("Returns","GARCH_t","GARCH_bt"),lty=1:5,col=1:5,bty="1")

#matplot(ret_var,ylab="VaR",col=c(1,2,3,4,5), type='l')
#legend("topleft", inset=0, legend=c("EWMA",  "GARCH","GARCH_t","GARCH_bt"),pch=1,  col=c(2,3,4,5), horiz=T)



# Backtesting with Kupiec Test
#####Bernouilli pdf(x)=p^x(1-p)^x
library(Rlab)
ytest=y[(TE+1):T]
VaRtest=VaR[(TE+1):T,]
v=VaRtest*0
m=c("EWMA-Normal", "GARCH-Normal","GARCH-t","GARCH-boot")
for (i in 1:4){
  q= y[(TE+1):T]/100< -VaR[(TE+1):T,i]	
  v=VaRtest*0
  v[q,i]=1 # hit sequence of violations
  l0=sum(log(dbern(v[,i], p)))
  p1=sum(v)/length(v[,i])
  l1=sum(log(dbern(v[,i], p1)))
  Kupiec=-2*(l0-l1)
  pval=1-pchisq(Kupiec,1)
  cat(i,m[i],'Kupiec stat and p-value',Kupiec,pval,"\n")
}
