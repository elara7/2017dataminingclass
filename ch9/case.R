library(e1071)
library(ISLR)
library(ROCR)
# prepare data
names(Smarket)
set.seed(1)
train =(Smarket$Year <2005)
TrainD<-Smarket[train,c("Lag1","Lag2","Lag3","Lag4","Lag5","Volume","Direction")]
TestD<-Smarket[!train,c("Lag1","Lag2","Lag3","Lag4","Lag5","Volume","Direction")]

library(ROCR)
rocplot=function(pred, truth, ...){
  predob = prediction(pred, truth)
  perf = performance (predob , "tpr", "fpr")
  plot(perf ,...)}

# linear
tune.linear=tune(svm,Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=TrainD,kernel="linear",
              ranges=list(cost=c(50,100,500,1000)),tune.control=(cross=5))
summary(tune.linear)
bestmod.linear=tune.linear$best.model
summary(bestmod.linear)
## linear predict train
ypred.train.linear=predict(bestmod.linear,TrainD[,c("Lag1","Lag2","Lag3","Lag4","Lag5","Volume")])
table(predict=ypred.train.linear, truth=TrainD$Direction)
fitted.train.linear=attributes(predict(bestmod.linear,TrainD,decision.values=TRUE))$decision.values
rocplot(fitted.train.linear ,TrainD$Direction,main="Training Data")
## linear predict test
ypred.test.linear=predict(bestmod.linear,TestD[,c("Lag1","Lag2","Lag3","Lag4","Lag5","Volume")])
table(predict=ypred.test.linear, truth=TestD$Direction)
fitted.test.linear=attributes(predict(bestmod.linear,TestD,decision.values=TRUE))$decision.values
rocplot(fitted.test.linear ,TestD$Direction,main="Testing Data")

# radial
tune.radial=tune(svm,Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=TrainD,kernel="radial",
                 ranges=list(cost=c(0.1,1, 50,100,500,1000),gamma=c(1,2,3)),tune.control=(cross=5))
summary(tune.radial)
bestmod.radial=tune.radial$best.model
summary(bestmod.radial)
## radial predict train
ypred.train.radial=predict(bestmod.radial,TrainD[,c("Lag1","Lag2","Lag3","Lag4","Lag5","Volume")])
table(predict=ypred.train.radial, truth=TrainD$Direction)
fitted.train.radial=attributes(predict(bestmod.radial,TrainD,decision.values=TRUE))$decision.values
rocplot(fitted.train.radial ,TrainD$Direction,main="Training Data")
## radial predict test
ypred.test.radial=predict(bestmod.radial,TestD[,c("Lag1","Lag2","Lag3","Lag4","Lag5","Volume")])
table(predict=ypred.test.radial, truth=TestD$Direction)
fitted.test.radial=attributes(predict(bestmod.radial,TestD,decision.values=TRUE))$decision.values
rocplot(fitted.test.radial ,TestD$Direction,main="Testing Data")

# polynomial
tune.polynomial=tune(svm,Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=TrainD,kernel="polynomial",
                     ranges=list(cost=c(50,100,500,1000),degree=3),tune.control=(cross=5))
summary(tune.polynomial)
bestmod.polynomial=tune.polynomial$best.model
summary(bestmod.polynomial)
## polynomial predict train
ypred.train.polynomial=predict(bestmod.polynomial,TrainD[,c("Lag1","Lag2","Lag3","Lag4","Lag5","Volume")])
table(predict=ypred.train.polynomial, truth=TrainD$Direction)
fitted.train.polynomial=attributes(predict(bestmod.polynomial,TrainD,decision.values=TRUE))$decision.values
rocplot(fitted.train.polynomial ,TrainD$Direction,main="Training Data")
## polynomial predict test
ypred.test.polynomial=predict(bestmod.polynomial,TestD[,c("Lag1","Lag2","Lag3","Lag4","Lag5","Volume")])
table(predict=ypred.test.polynomial, truth=TestD$Direction)
fitted.test.polynomial=attributes(predict(bestmod.polynomial,TestD,decision.values=TRUE))$decision.values
rocplot(fitted.test.polynomial ,TestD$Direction,main="Testing Data")
