###simulate regression f(x)=2+3x
x=rnorm(100)
e<-rnorm(100)
y<-2+3*x+e
plot(x,y)
abline(lm(y~x),col="blue")
abline(a=2,b=3,col="red")

curve(2+3*x,-2,2,col="red")
i=1
while (i<=10){
x=rnorm(100)
e<-rnorm(100,0,1)
y<-2+3*x+e
abline(lm(y~x),col="blue")
i<-i+1
}
################################
###simple linear regression
library(MASS)
library(ISLR)
View(Boston)
names(Boston)
lm.fit<-lm(medv~lstat)
lm.fit=lm(medv~lstat,data=Boston)
attach(Boston)
lm.fit=lm(medv∼lstat)
lm.fit
summary(lm.fit)
names(lm.fit)
coef(lm.fit)
confint(lm.fit)
predict(lm.fit,data.frame(lstat=(c(5,10,15)))
predict(lm.fit,data.frame(lstat=(c(5,10,15))),
                interval="prediction")
plot(lstat,medv)
abline(lm.fit)
abline(lm.fit,lwd=3)
abline(lm.fit,lwd=3,col="red") 
plot(lstat,medv,col="red")
plot(lstat,medv,pch=20)
plot(lstat,medv,pch="+")
plot(1:20,1:20,pch=1:20)
plot(predict(lm.fit), residuals(lm.fit)) 
plot(predict(lm.fit), rstudent(lm.fit))
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))
#######Multiple linear regression
lm.fit=lm(medv∼lstat+age,data=Boston) 
summary(lm.fit)
########################################
###qualitative predictors
credit<-read.csv(file="http://www-bcf.usc.edu/~gareth/ISL/Credit.csv")
View(credit)
pairs(credit[,-1])
summary(lm(Balance~Gender,data=credit))
summary(lm(Balance~Ethnicity,data=credit))
######interaction
ads<-read.csv(file="http://www-bcf.usc.edu/~gareth/ISL/Advertising.csv")
plot(ads$TV,ads$Sales)
abline(lm(Sales~TV,data=ads),col="red")
plot(ads$Radio,ads$Sales)
abline(lm(Sales~Radio,data=ads),col="red")
plot(ads$Newspaper,ads$Sales)
abline(lm(Sales~Newspaper,data=ads),col="red")
fit<-lm(Sales~TV+Radio+TV*Radio,data=ads)
summary(fit)
############polynomial regression
data(Auto)
summary(lm(mpg~horsepower+I(horsepower^2),data=Auto))
####collinearity
plot(credit$Limit,credit$Age,col="red")
plot(credit$Limit,credit$Rating,col="red")
c1<-lm(Balance~Age+Limit,data=credit)
c2<-lm(Balance~Rating+Limit,data=credit)
summary(c1)
summary(c2)
library(car)
vif(c2)
