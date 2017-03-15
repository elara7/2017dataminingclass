#####resampling
##validation set approach
library(ISLR)
set.seed(1)
train=sample(392,196) # 设定训练集位置
lm.fit=lm(mpg~horsepower,data=Auto,subset=train) # 用训练集建模,subset填入标签即可
attach(Auto)
mean((mpg-predict(lm.fit,Auto))[-train]^2) # 用测试集预测算MSE
lm.fit2=lm(mpg~poly(horsepower ,2),data=Auto,subset=train) # 用训练集建模，d = 2
mean((mpg-predict(lm.fit2,Auto))[-train]^2) # 计算测试集MSE
lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train) # d = 3
mean((mpg-predict(lm.fit3,Auto))[-train]^2)
detach(Auto)

vsa <- matrix(nrow = 50, ncol = 10)
seed <- seq(1, 1000, 20)
for(i in 1:50){
  set.seed(seed[i])
  train=sample(392,196) # 设定训练集位置
  for(j in 1:10){
  lm.fit=lm(mpg~poly(horsepower ,j),data=Auto,subset=train) # 用训练集建模,subset填入标签即可
  attach(Auto)
  vsa[i,j] <- mean((mpg-predict(lm.fit,Auto))[-train]^2) # 用测试集预测算MSE
  detach(Auto)
  }
}


##leave one out CV
library(boot)
glm.fit=glm(mpg~horsepower,data=Auto) # 全样本建模
cv.err=cv.glm(Auto,glm.fit) # leave one out 这样写就是LOOCV,默认K=样本量。可以自己设定K折
cv.err # seed一堆没用
cv.err$delta # 第一个数值是交叉验证的预测误差，第二个数值是调整后的（修正了不用LOOCV带来的偏差，因为K越小，偏差越大，方差越小）
####k-fold CV 选d
set.seed(17)
cv.error.10=rep(0,10)
for (i in 1:10){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
   cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1] }
 cv.error.10
#############################
##bootstrap
##portofolio
alpha.fn=function(data,index){
   X=data$X[index]
   Y=data$Y[index]
   return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y))) 
}

alpha.fn(Portfolio,1:100) #返回 index 为1：100的数据部分的alpha估计
set.seed(1)
alpha.fn(Portfolio,sample(100,100,replace=T)) # 100个里面放回抽样，bootstrap，返回bootstrap数据的alpha估计
boot(Portfolio,alpha.fn,R=1000) # 将数据带入 alpha.fn 函数，重复计算1000次
b <- boot(Portfolio,alpha.fn,R=1000) #t0,原始数据的统计量，t每次bootstrap的统计量
###
boot.fn=function(data,index)
  return(coef(lm(mpg~horsepower ,data=data,subset=index)))
boot(Auto,boot.fn,1000)
summary(lm(mpg~horsepower ,data=Auto))$coef
