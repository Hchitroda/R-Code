# Limited Dependent variables/ choice models (Probit and Logit Models)
#install.packages("mfx") <--this is a package to compute marginal effects for the models

#setwd("C:/Teaching/FIN 657 Risk Econometrics/R")
##############################################################
###Bankruptcy data
da2=read.csv("HWFAILED.csv",header=T)
head(da2)
summary(da2)
# delete missing data rows
da2=na.omit(da2)
summary(da2)

attach(da2)
#Y=da2$yd
Y=cbind(yd)
X=cbind(tdta,gempl,opita,invsls,lsls,lta,nwcta,cacl,qacl,ebita,reta,fata)

#yd     = "Distress Dummy"
#tdta   = "Debt to Assets"
#gempl  = "Employee Growth Rate" 
#opita  = "Op. Income to Assets"
#invsls = "Inventory to Sales" 
#lsls   = "Log of Sales" 
#lta    = "Log of Assets" 
#nwcta  = "Net Working Cap to Assets"
#cacl   = "Current Assets to Current Liab" 
#qacl   = "Quick Assets to Current Liab" 
#ebita  = "EBIT to Assets" 
#reta   = "Retained Earnings to Assets" 
#fata   = "Fixed Assets to Assets"


# Descriptive statistics
summary(Y)
summary(X)


table(Y)
table(Y)/sum(table(Y)) # proportion for Y=0 and Y=1

# t-test for explanatory variables by groups
# let's test tdta variable
plot(tdta ~ yd, data = da2)#<--plot to see how tdta is separated into two groups: yd=0 and yd=1
#with(da2, t.test(tdta[yd == 0], tdta[yd == 1])) 
t.test(tdta ~ yd, data = da2) # <--t = -6.0972, df = 139.54, p-value = 9.993e-09 <---this means significant difference in means with on average 
                              #<--higher Debt ratio for yd=1 (bankrupt firms)

# Correlation matrix 
cor(X)
##<--notice very high correlation between 
  ##opita ~ ebita = 	97.5%, 
  #Lsls ~ Lta = 		95.7% 
  #Cacl ~ qacl = 	88.2%\

#################################
# OLS Regression 
olsreg = lm(yd~tdta+gempl+opita+invsls+lsls+lta+nwcta+cacl+qacl+ebita+reta+fata,data=da2)
summary(olsreg)

# OLS Regression adjusted for heteroscedsticity
library("lmtest")
library("sandwich")
VCOV=vcovHC(olsreg, type="HC") #<--type = c("HC3", "const", "HC", "HC0", "HC1", "HC2", "HC4", "HC4m", "HC5")
coeftest(olsreg, vcov. = VCOV) #<--print out corrected standard errors and tstats

# take out highly correlated variables (to avoid multicollinearity) and keep significant variables at 10%
olsreg = lm(yd~tdta+gempl+opita+invsls+lsls,data=da2)
#summary(olsreg)
VCOV=vcovHC(olsreg, type="HC") #<--type = c("HC3", "const", "HC", "HC0", "HC1", "HC2", "HC4", "HC4m", "HC5")
coeftest(olsreg, vcov. = VCOV) #<--print out corrected standard errors and tstats

#################################

  ## Logit model 
library("lmtest")
library("sandwich")
logit<- glm(yd~tdta+gempl+opita+invsls+lsls+lta+nwcta+cacl+qacl+ebita+reta+fata,data=da2, family=binomial (link = "logit"))
summary(logit) 
VCOV=vcovHC(logit, type="HC") #in the package Sandwich was used to correct for Heteroscedasticity
coeftest(logit, vcov. = VCOV) #<--print out corrected standard errors and tstats

  # take out highly correlated variables (to avoid multicollinearity) and keep significant variables at 11%
logit<- glm(yd~tdta+gempl+opita+invsls+lsls,data=da2, family=binomial (link = "logit"))
  #summary(logit) 
VCOV=vcovHC(logit, type="HC") #in the package Sandwich was used to correct for Heteroscedasticity
coeftest(logit, vcov. = VCOV) #<--print out corrected standard errors and tstats


  # The stargazer() function from the package –stargazer allows a publication quality of the logit model.
  # The model will be saved in the working directory under the name ‘logit.htm’ which you can open with Word or any other word processor.
#library(stargazer)
#stargazer(logit, type="html", out="logit.htm")

##logit<- glm(Y ~ X, family=binomial (link = "logit"))
##summary(logit) 
# McFadden's Pseudo R-squared
logit0<-update(logit, formula= Y ~ 1)
McFadden<- 1-as.vector(logLik(logit)/logLik(logit0))
McFadden

# Logit model odds ratios
exp(logit$coefficients)  # >1 outcome of Y=1 is more likely than outcome <1
##logitor(formula=Y ~ X, data=da2)

#################################

# Probit model 
probit<- glm(yd~tdta+gempl+opita+invsls+lsls+lta+nwcta+cacl+qacl+ebita+reta+fata,data=da2, family=binomial (link = "probit"))
summary(probit) 
##probit<- glm(Y ~ X, family=binomial (link="probit"))
##summary(probit)
library("lmtest")
library("sandwich")
VCOV=vcovHC(probit, type="HC") #in the package Sandwich was used to correct for Heteroscedasticity
coeftest(probit, vcov. = VCOV) #<--print out corrected standard errors and tstats

# take out highly correlated variables (to avoid multicollinearity) and keep significant variables at 10%
probit<- glm(yd~tdta+gempl+opita+invsls+lsls,data=da2, family=binomial (link = "probit"))
#summary(probit) 
VCOV=vcovHC(probit, type="HC") #in the package Sandwich was used to correct for Heteroscedasticity
coeftest(probit, vcov. = VCOV) #<--print out corrected standard errors and tstats

# McFadden's Pseudo R-squared
probit0<-update(probit, formula= Y ~ 1)
McFadden<- 1-as.vector(logLik(probit)/logLik(probit0))
McFadden

#################################
#Marginal Effects
# Regression marginal effects
coef(olsreg)

X=cbind(tdta,gempl,opita,invsls,lsls)
# Logit and PROBIT models marginal effects
library(mfx)
mlogit=logitmfx(formula=Y ~ X, data=da2, atmean = FALSE, robust = FALSE)
mlogit

mprobit=probitmfx(formula=Y ~ X, data=da2, atmean = FALSE, robust = FALSE ) ## default marginal effects represent the partial effects for the average observation.
mprobit
# standard deviations of variables
sd(tdta)
sd(gempl)
sd(opita)
sd(invsls)
sd(lsls)
# Create dF/dx*sd(x)
# for example, 0.84*0.221201=.1858
#################################
#Prediction

# Regression predicted probabilities
polsreg<- predict(olsreg)
summary(polsreg)

# Logit model predicted probabilities
plogit<- predict(logit, type="response")
summary(plogit)

#Getting predicted probabilities holding all predictors or independent variables to their means.
# If you instead plug in numbers for specific company ratios you can get predicted probability for this company.
invlogit = function (x) {1/(1+exp(-x))}
invlogit(coef(logit)[1]+
           coef(logit)[2]*mean(da2$tdta)+
           coef(logit)[3]*mean(da2$gempl)+
           coef(logit)[4]*mean(da2$opita)+
           coef(logit)[5]*mean(da2$invsls)+
           coef(logit)[6]*mean(da2$lsls))

# Probit model predicted probabilities
pprobit<- predict(probit, type="response")
summary(pprobit)

#Getting predicted probabilities holding all predictors or independent variables to their means.
# If you instead plug in numbers for specific company ratios you can get predicted probability for this company.
invprobit = function (x) {pnorm(x,mean=0,sd=1)} #find Normal CDF of x
invprobit(coef(probit)[1]+
           coef(probit)[2]*mean(da2$tdta)+
           coef(probit)[3]*mean(da2$gempl)+
           coef(probit)[4]*mean(da2$opita)+
           coef(probit)[5]*mean(da2$invsls)+
           coef(probit)[6]*mean(da2$lsls))

# Percent correctly predicted values
a=table(true = Y, pred = round(fitted(probit)))
a
b=table(true = Y, pred = round(fitted(logit)))
b
#probit percent correctly predicted Y=0
a[1,1]/(sum(Y==0))
#probit percent correctly predicted Y=1
a[2,2]/(sum(Y==1))

#logit percent correctly predicted Y=0
b[1,1]/(sum(Y==0))
#logit percent correctly predicted Y=1
b[2,2]/(sum(Y==1))

#################################

