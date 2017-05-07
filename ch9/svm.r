library(e1071)
#library(LiblineaR)
#support vector classifier
#linear decision boundary; linear kernel
set.seed(1)
x=matrix(rnorm(20*2),ncol=2) 
y=c(rep(-1,10), rep(1,10))
plot(x,col=(3-y))
x[y==1,]=x[y==1,]+1
plot(x, col=(3-y))
dat=data.frame(x=x, y=as.factor(y))

svmfit=svm(y~., data=dat,gamma=0, kernel="linear", cost=10,
           scale=FALSE)
##cost :Lagran C constraints
plot(svmfit, dat)#X means support vectors
svmfit$index# the index of support vectors
summary(svmfit)
##reduce cost to 0.1 wider margin, more support vectors
svmfit=svm(y~., data=dat, kernel="linear", cost=0.1, scale=FALSE)
plot(svmfit , dat)
svmfit$index
summary(svmfit)
##how to choose tuning parameter C
##choosing best tunning parameter c by CV
set.seed(1)
tune.out=tune(svm,y~.,data=dat,kernel="linear",
              ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.out)
bestmod=tune.out$best.model
summary(bestmod)
####predict
xtest=matrix(rnorm(20*2), ncol=2)
ytest=sample(c(-1,1), 20, rep=TRUE)
xtest[ytest==1,]=xtest[ytest==1,] + 1
testdat=data.frame(x=xtest, y=as.factor(ytest))
ypred=predict(bestmod,testdat)
table(predict=ypred, truth=testdat$y)
##redcuce cost to 0.01
svmfit=svm(y~., data=dat, kernel="linear", cost=.01, scale=FALSE)
ypred=predict(svmfit ,testdat)
table(predict=ypred, truth=testdat$y)

##linear seperate
x[y==1,]=x[y==1,]+0.5
plot(x, col=(y+5)/2, pch=19)
dat=data.frame(x=x,y=as.factor(y))
##with large cost
svmfit=svm(y~., data=dat, kernel="linear", cost=1e05)
summary(svmfit)
plot(svmfit , dat)
ypred=predict(svmfit ,testdat)
table(predict=ypred, truth=testdat$y)
##cost=1
svmfit=svm(y~., data=dat, kernel="linear", cost=1)
summary(svmfit)
plot(svmfit ,dat)
##########################
#Excercise
#how to classify Smarket using SVM
library(ISLR)
names(Smarket)
View(Smarket)
train =(Smarket$Year <2005)
TrainD<-Smarket[train,]
View(TrainD) 
TestD<-Smarket[!train,]
svmf1<-svm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume ,
           data=TrainD,kernel="linear", gamma=0,cost=1, scale=FALSE)
summary(svmf1)
plot(svmf1,TrainD,Lag1~Lag2)
plot(svmf1,TrainD,Lag1~Volume)
##choosing best cost
set.seed(1)
tune.out=tune(svm,Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=TrainD,kernel="linear",
              ranges=list(cost=c( 50,100,500,1000)),tune.control=(cross=5))
summary(tune.out)
bestmod=tune.out$best.model
summary(bestmod)
##predict
ypred=predict(bestmod,TestD)
table(predict=ypred, truth=TestD$Direction)
################################################
###support vector machine
##non-linear decision boundary; nonlinear kernel
set.seed(1)
x=matrix(rnorm(200*2), ncol=2)
x[1:100,]=x[1:100,]+2
x[101:150,]=x[101:150,]-2
y=c(rep(1,150),rep(2,50))
dat=data.frame(x=x,y=as.factor(y))
plot(x, col=y)
train=sample(200,100)
svmfit=svm(y~., data=dat[train,], kernel="radial", gamma=1,
           cost =1)
plot(svmfit , dat[train ,])
summary(svmfit)
svmfit=svm(y~., data=dat[train,], kernel="radial",gamma=1, cost=1e05)
plot(svmfit ,dat[train ,])
summary(svmfit)
set.seed(1)
tune.out=tune(svm, y~., data=dat[train,], kernel="radial",
              ranges=list(cost=c(0.1,1,10,100,1000),
                          gamma=c(0.5,1,2,3,4))) 
summary(tune.out)
table(true=dat[-train,"y"], 
      pred=predict(tune.out$best.model, newx=dat[-train ,]))

#####ROC curves
library(ROCR)
svmfit.opt=svm(y~., data=dat[train,], kernel="radial", gamma=2, cost=1)
pred<-predict(svmfit.opt,dat[train,])
#prediction(ROCR.class(pred))
pred2<-prediction(pred,dat[train,]$y,labels)
par(mfrow=c(1,2))


rocplot=function(pred, truth, ...){
  predob = prediction(pred, truth)
  perf = performance (predob , "tpr", "fpr")
  plot(perf ,...)}
##training data
svmfit.opt=svm(y~., data=dat[train,], kernel="radial", gamma=2, cost=1,decision.values=T)
fitted=attributes(predict(svmfit.opt,dat[train,],decision.values=TRUE))$decision.values
par(mfrow=c(1,2))
rocplot(fitted ,dat[train,"y"],main="Training Data")
svmfit.flex=svm(y~., data=dat[train,], kernel="radial", gamma=50, cost=1, decision.values=T)
fitted=attributes(predict(svmfit.flex,dat[train,],decision.values=T))$decision.values
rocplot(fitted ,dat[train,"y"],add=T,col="red")
##testing data
fitted=attributes(predict(svmfit.opt,dat[-train,],decision.values=T))$decision.values
rocplot(fitted ,dat[-train,"y"],main="Test Data")
fitted=attributes(predict(svmfit.flex,dat[-train,],decision.values=T))$decision.values
rocplot(fitted ,dat[-train,"y"],add=T,col="red")
#####SVM with multiple classes
##using one versus one
set.seed(1)
x=rbind(x, matrix(rnorm(50*2), ncol=2)) 
y=c(y, rep(0,50))
x[y==0,2]=x[y==0,2]+2
dat=data.frame(x=x, y=as.factor(y))
View(dat)
par(mfrow=c(1,1))
plot(x,col=(y+1))
svmfit=svm(y~., data=dat, kernel="radial", cost=10, gamma=1) 
plot(svmfit , dat)
par(mfrow=c(1,1))

##############################################
###excercise 7
Data(Auto)
View(Auto)
#attach(Auto)
Auto$grade<-cut(Auto$mpg,c(0,median(Auto$mpg),max(Auto$mpg)))
levels(Auto$grade)<-c("0","1")
View(Auto)
train<-sample(dim(Auto)[1],292)
Train_Auto<-Auto[train,]
Test_Auto<- Auto[-train,]
#detach(Auto)
##linear kernel with varioius cost
tune.Auto=tune(svm,grade~cylinders+displacement+horsepower+weight+acceleration,data=Train_Auto,kernel="linear",
               ranges=list(cost=c( 0.01, 0.1, 0.5, 1,5,10,50,100,500)))
summary(tune.Auto)
bestmod=tune.Auto$best.model
summary(bestmod)
table(true=Test_Auto$grade, 
      pred=predict(bestmod, newdata=Test_Auto))

##radial and polynomial kernel with different gamma, degree,cost
tune.Auto2=tune(svm,grade~cylinders+displacement+horsepower+weight+acceleration,data=Train_Auto,kernel="radial",
                ranges=list(cost=c(0.1,1,5,10,50,100,500),gamma=c(0,1,1,5,10),degree=c(2,3)))
summary(tune.Auto2)
bestmod2=tune.Auto2$best.model
summary(bestmod2)
table(true=Test_Auto$grade, 
      pred=predict(bestmod2, newdata=Test_Auto))
##plot
plot(bestmod2,Train_Auto,horsepower~weight)
