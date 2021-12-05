# Module 2: Dummy Variables

### You should use your working directory ###
###Click: Session-->Set Working Doirectory-->To Source File Location


### IPO/SEO example
equity = read.csv("equity.csv")   

# Plot Value against SEO dummy variable
plot(value ~ seo, data = equity)

# More fancy plot library ggplot2 needs to be installed
library("ggplot2")
ggplot(aes(x=seo,y=value), data=equity)+geom_point()
  

# t-test for explanatory variables by groups

t.test(value ~ seo, data = equity) # t = -3.4843, df = 244.6, p-value = 0.0005845 <---this means significant difference in means with on average 
#we find on average higher Value  for SEO=1  firms compared to SEO=0 (IPO firms)
#mean in group 0 (SEO) mean in group 1 (IPO)
#191.7952        829.5748 

t.test(income ~ seo, data = equity)
#t = -3.0945, df = 265.31, p-value = 0.002182
#we find on average higher income for SEO=1  firms compared to SEO=0 (IPO firms)
#  mean in group 0 mean in group 1 
#1.598795       15.455752 

with(equity, t.test(assets[seo == 0], assets[seo == 1])) 
# not significant difference in the means of assets of two groups

# a more fancy package for regression with various options
library(fRegression)
#example(regFit)

m1 = regFit(value~seo+debt+sales+income+assets+seo:debt+seo:sales+seo:income+seo:assets,data=equity, use = "lm")
summary(m1)
plot(m1)
1
2
0

# limit value of firms to be below 10000 and run a regression again

equity1=equity[equity$value<10000,] # removing outliers
summary(equity$value)
summary(equity1$value)

m2 = regFit(value~seo+debt+sales+income+assets+seo:debt+seo:sales+seo:income+seo:assets,data=equity1, use = "lm")
summary(m2)
plot(m2)
1
2
0

# limit value of firms to be below 10000 and run a regression of Value on Sales, show regression line. Here we see heteroscedasticity as the variance 
#of observations increases with Sales

ggplot(aes(x=sales,y=value), data=equity)+
  geom_point()+
  geom_smooth(method="lm",size=1.1,se=F)+
  ylim(0,10000)

# Heteroscedasticity correction
#install.packages("car")
library("car")
m1 = lm(value~seo+debt+sales+income+assets+seo:debt+seo:sales+seo:income+seo:assets,data=equity1)
summary(m1)
VCOV=hccm(m1, type="hc0") #<--type=c("hc3", "hc0", "hc1", "hc2", "hc4")

#install.packages("lmtest")
library("lmtest")
coeftest(m1, vcov. = VCOV) #<--print out corrected standard errors and tstats


## another package for heteroscedasticity correction
#install.packages("sandwich")
library("sandwich")
VCOV=vcovHC(m1, type="HC") #<--type = c("HC3", "const", "HC", "HC0", "HC1", "HC2", "HC4", "HC4m", "HC5")
coeftest(m1, vcov. = VCOV) #<--print out corrected standard errors and tstats
