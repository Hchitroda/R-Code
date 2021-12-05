
#import xts data from finance.yahoo using quantmod
library(quantmod)  # <=== load the package "quantmod"
library("timeSeries")
getSymbols("AAPL",from="2010-01-01",to="2018-08-13") # <== specify the data span
#chartSeries(AAPL[,6],theme="white") # <==  use "white" background for the plot.
AAPL=last(AAPL, "5 years") # select the last 5 years of data
price_AAPL=AAPL[,6]
ret_AAPL=diff(log(price_AAPL))
plot(price_AAPL)
plot(ret_AAPL)
#ret_AAPL=ret_AAPL[2:length(ret)] # exclude the first missing value from returns
ret_AAPL=removeNA(ret_AAPL) # exclude missing value from returns

getSymbols("DEXUSEU",src="FRED") #<== Load Dollar verus Euro daily exchange rates from FRED.
summary(DEXUSEU) # notice there are NA missing observations
EURO=last(DEXUSEU, "5 years") # select the last 5 years of data
summary(EURO) # notice there are  NA missing observations
dim(EURO)
# exclude all mising observations from EURO for further analysis
EURO=removeNA(EURO)
summary(EURO) # notice there are no missing observations now
dim(EURO)
ret_EURO=diff(log(EURO))
length(ret_EURO)
ret_EURO=removeNA(ret_EURO) # exclude missing value from returns
length(ret_EURO)
plot(EURO)
plot(ret_EURO)

# Correlogram acf function
q=acf(price_AAPL, lag=20)  # 20 lags of autocorrelation for price
q[1:20]
plot(q[1:20])

q=acf(ret_AAPL, lag=20) # 20 lags of autocorrelation for returns
plot(q[1:20])


q=acf(EURO, lag=20)  # 20 lags of autocorrelation for price
q[1:20]
plot(q[1:20])

q=acf(ret_EURO, lag=20)  # 20 lags of autocorrelation for returns
q[1:20]
plot(q[1:20])

##linear regression for AR(1) in differenced form for AAPL
Y=log(price_AAPL)

m1=lm(diff(Y)~lag(Y)) 
summary(m1)

##linear regression for AR(2) in differenced form for AAPL
#m2=lm(diff(Y)~lag(Y)+lag(diff(Y))) 
#summary(m2)

###############################################
# There are several libraries you could use. Some libraries allow vhoice of Case 1: no constant, Case 2: with constant (drift), Case 3: with constant and trend
# library(tseries) does not have this choice uses case 3 by deafault.

#Unit root test
library(fUnitRoots) 
adfTest(coredata(price_AAPL),lags=10, type="c") # type="nc" no constant, type="c" with constant, type="ct" constant and trend
adfTest(coredata(ret_AAPL),lags=2, type="c")
adfTest(coredata(EURO),lags=2, type="c") # type="nc" no constant, type="c" with constant, type="ct" constant and trend
adfTest(coredata(ret_EURO),lags=2, type="c")

#tseries library for Unit root test (note specification with time trend)
library(tseries) #incorporates a constant and a linear trend
adf.test(price_AAPL)
adf.test(ret_AAPL)
adf.test(EURO)
adf.test(ret_EURO)



library(urca)
?ur.df # see help on this command
#ur.df(y, type = c("none", "drift", "trend"), lags = 1,
#      selectlags = c("Fixed", "AIC", "BIC")) 

## below I am choosing specification with "drift" (no trend) and let the package choose optimal number of lags by minimizing BIC crietion.

## Look at the first Value of test-statistics and critical values listed in tau2
# for price_APPLE example below, Value of test-statistic is: 0.156, 10% tau2=-2.57 (critical value), so DO NOT REJECT UNIT ROOT  
adf=ur.df(y=price_AAPL, type = "drift", selectlags = "BIC") 
summary(adf)
adf=ur.df(y=ret_AAPL, type = "drift", selectlags = "BIC") 
summary(adf)
adf=ur.df(y=EURO, type = "drift", selectlags = "BIC") 
summary(adf)
adf=ur.df(y=ret_EURO, type = "drift", selectlags = "BIC") 
summary(adf)

####################################################################################
## Question 2. Load data from a file and test for cointegration
library("timeSeries")
## Session-->set working directory-->to source file location
da=read.csv("macro.csv",header=T)
head(da)
dim(da)

#[1] 254   7
##unit root test for each variable: use either tseries library or fUnitRoots library. I sue here 1Y and 3Y TBONDS

#Unit root test
library(fUnitRoots) 
adfTest(da$USTB6M,type="c") # type="nc" no constant, type="c" with constant, type="ct" constant and trend
adfTest(da$USTB1Y,type="c") # type="nc" no constant, type="c" with constant, type="ct" constant and trend
adfTest(da$USTB3Y,type="c") # type="nc" no constant, type="c" with constant, type="ct" constant and trend
adfTest(da$USTB10Y,type="c") # type="nc" no constant, type="c" with constant, type="ct" constant and trend


# create monthly time series with ts command start in 1986 Month=3
USTB6M=ts(da$USTB6M,frequency=12,start=c(1986,3))
USTB1Y=ts(da$USTB1Y,frequency=12,start=c(1986,3))
USTB3Y=ts(da$USTB3Y,frequency=12,start=c(1986,3))

#plot time series on one graph
plot(USTB6M,main ="T-Bond yields", ylab="yield", col=1)
lines(USTB1Y, col=2)
lines(USTB3Y, col=3)
legend("topright", inset=0.03, legend=c("USTB6M","USTB1Y","USTB3Y"),pch=1,  col=c(1,2,3), horiz=F)


#library(fUnitRoots)
#adfTest(da$USTB1Y, lags=2, type="c") # type="nc" no constant, type="c" with constant, type="ct" constant and trend
#adfTest(da$USTB3Y, lags=2, type="c") # type="nc" no constant, type="c" with constant, type="ct" constant and trend

library(egcm)
egcm(da$USTB1Y, da$USTB6M)
plot(egcm(da$USTB1Y, da$USTB6M))
summary(egcm(da$USTB1Y, da$USTB6M)) # Here we find cointegration

library(egcm)
egcm(da$USTB1Y, da$USTB3Y)
plot(egcm(da$USTB1Y, da$USTB3Y))
summary(egcm(da$USTB1Y, da$USTB3Y)) # Here we do not find cointegration

##########################################################################################
## Estimate VAR or VECM
#VAR --if no cointegration. Need to difference non-stationary time series
dTB1Y=diff(da$USTB1Y)
dTB3Y=diff(da$USTB3Y)
library(vars)
VARselect(cbind(dTB1Y,dTB3Y),lag.max = 8) # Select number of lags
var.TB=VAR(cbind(dTB1Y,dTB3Y), p=1, type="const")
summary(var.TB)

## Granger Casuality
causality(var.TB, cause ="dTB1Y")
causality(var.TB, cause ="dTB3Y")

# Imulse Response
plot(irf(var.TB,impulse = "dTB1Y"))
plot(irf(var.TB,impulse = "dTB3Y"))

#VECM --if there is cointegration. 
#library(urca)
#vecm.TB = ca.jo(da[, c("USTB1Y", "USTB6M")], ecdet = "none", type = "trace", K = 2, spec = "transitory") #Johansen test of cointegration
#summary(vecm.TB)
#vecm.r1 <- cajorls(vecm.TB, r = 1)
#vecm.r1


library(tsDyn)
#Fit a VECM with Engle-Granger 2OLS estimator:
vecm.eg<-VECM(da[, c("USTB1Y", "USTB6M")], lag=1)
summary(vecm.eg)

#Fit a VECM with Johansen MLE estimator:
vecm.jo<-VECM(da[, c("USTB1Y", "USTB6M")], lag=1, estim="ML")
summary(vecm.jo)

