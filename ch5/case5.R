##validation set approach
library(ISLR)

vsa <- matrix(nrow = 10, ncol = 10)
for(i in 1:10){
  set.seed(i+10)
  train=sample(392,196) # 设定训练集位置
  for(j in 1:10){
    lm.fit=lm(mpg~poly(horsepower ,j),data=Auto,subset=train) # 用训练集建模,subset填入标签即可
    attach(Auto)
    vsa[i,j] <- mean((mpg-predict(lm.fit,Auto))[-train]^2) # 用测试集预测算MSE
    detach(Auto)
  }
}

plot(1:10, vsa[1,], type = "l", ylim = c(14, 36), ylab = "Mean Squared Error", xlab = "Degree of Polynomial")
for(i in 2:10){
lines(1:10, vsa[i,],type = "l", col = i*10)
}


####10-fold CV
library(boot)
set.seed(17)
cv.error.10=matrix(nrow = 10, ncol = 10)
for (j in 1:10){
  for (i in 1:10){
    glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
    cv.error.10[j,i]=cv.glm(Auto,glm.fit,K=10)$delta[1] 
  }
  }

plot(1:10, cv.error.10[1,], type = "l", ylim = c(18, 25), ylab = "Mean Squared Error", xlab = "Degree of Polynomial")
for(i in 2:10){
  lines(1:10, cv.error.10[i,],type = "l", col = i*10)
}

#alpha
#sim
par(mfrow  = c(1,3))
library(MASS)
alpha <- NULL
mu <- c(0,0)
Sigma <- matrix(c(1,0.5,0.5,1.25),nrow = 2)
for(i in 1:1000){
  simdata <- mvrnorm(100,mu,Sigma)
  simdata <- as.data.frame(simdata)
  colnames(simdata) <- c("X","Y")
  alpha[i] <- (var(simdata$Y)-cov(simdata$X,simdata$Y))/(var(simdata$X)+var(simdata$Y)-2*cov(simdata$X,simdata$Y))
  }
hist(alpha,main = "Histogram of alpha(True)",xlab = "alpha")
abline(v=0.6,col="red")

#bootstrap
set.seed(1)
alpha.fn=function(data,index){
  X=data$X[index]
  Y=data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y))) 
}
b <- boot(Portfolio,alpha.fn,R=1000) 
hist(b$t, main = "Histogram of alpha(Bootstrap)",xlab = "alpha")
abline(v=0.6,col="red")

boxplot(alpha,b$t,names = c("True","Bootstrap"), ylab = "alpha")
abline(h=0.6, col="red")
