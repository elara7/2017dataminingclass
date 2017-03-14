#6
#(a)
beta0 <- -6
beta1 <- 0.05
beta2 <- 1
X1 <- 40
X2 <- 3.5

e <- exp(beta0 + beta1 * X1 + beta2 * X2)
p <- e/(1+e)
p
# [1] 0.3775407

#(b)

px <- 0.5
l <- log(px/(1-px))
time <- (l - beta0 + beta2 * X2)/beta1
time
# [1] 190

#12
#(a)
Power <- function(){
  print(2^3)
}

Power()
# [1] 8


#(b)
Power2 <- function(x, a){
  print(x^a)
}

Power2(3,3)
# [1] 27

#(c)
Power2(10,3)
# [1] 1000
Power2(8,17)
# [1] 2.2518e+15
Power2(131,3)
# [1] 2248091

#(d)
Power3 <- function(x, a){
  return(x^a)
}

#(e)
x <- 1:10
y <- Power3(x,2)
plot(x,y,type="l", xlab = "x-axis", ylab = "y-axis", main = "Figure of f(x) = x^2")
plot(x,log(y),type="l", xlab = "log x-axis", ylab = "log y-axis", main = "Figure of f(x) = x^2 with log y-axis")
plot(log(x),y,type="l", xlab = "log x-axis", ylab = "log y-axis", main = "Figure of f(x) = x^2 with log x-axis")
plot(log(x),log(y),type="l", xlab = "log x-axis", ylab = "log y-axis", main = "Figure of f(x) = x^2 with log xy-axis")

#(f)
PlotPower <- function(x,a){
  plot(x,x^a, xlab = "x-axis", ylab = "y-axis", type = "l")
}
PlotPower(1:10, 3)


# scenario1
library(MASS)
#生成testdata
Sigma1 <- matrix(c(2,0,0,2),2,2)
Sigma2 <- matrix(c(1,0,0,1),2,2)
u1 <- c(0,0)
u2 <- c(1,1)
class1_x <- mvrnorm(n=20, u1, Sigma1)
class2_x <- mvrnorm(n=20, u2, Sigma2)
class1_y <- rep(1,20)
class2_y <- rep(0,20)
testdata <- rbind(cbind(class1_y, class1_x), cbind(class2_y, class2_x))
colnames(testdata) <- c("y","x1","x2")
testdata <- as.data.frame(testdata)

lda.error <- NULL
qda.error <- NULL
glm.error <- NULL
KNN1.error <- NULL
KNNCV.error <- NULL
# 生成train
library(class)
for (i in 1:100){
  class1_x <- mvrnorm(n=100, u1, Sigma1)
  class2_x <- mvrnorm(n=100, u2, Sigma2)
  class1_y <- rep(1,100)
  class2_y <- rep(0,100)
  traindata <- rbind(cbind(class1_y, class1_x), cbind(class2_y, class2_x))
  colnames(traindata) <- c("y","x1","x2")
  traindata <- as.data.frame(traindata)
  #lda
  lda.fit <- lda(y ~ x1 +  x2, data = traindata)
  lda.pred_test <- predict(lda.fit, testdata)$class
  lda.cfmatrix_test <- table(lda.pred_test ,testdata$y)
  lda.test_error <- (lda.cfmatrix_test[1,2]+lda.cfmatrix_test[2,1])/nrow(testdata)
  lda.error <- c(lda.error, lda.test_error)
  #qda
  qda.fit <- qda(y ~ x1 +  x2, data = traindata)
  qda.pred_test <- predict(qda.fit, testdata)$class
  qda.cfmatrix_test <- table(qda.pred_test ,testdata$y)
  qda.test_error <- (qda.cfmatrix_test[1,2]+qda.cfmatrix_test[2,1])/nrow(testdata)
  qda.error <- c(qda.error, qda.test_error)
  #logistic
  glm.fit <- glm(y ~ x1 +  x2, data = traindata,
                 family=binomial)
  glm.prob_test <- predict(glm.fit,newdata = testdata, type="response")
  glm.pred_test <- rep(0, nrow(testdata))
  glm.pred_test[glm.prob_test > 0.5] <- 1
  glm.cfmatrix_test <- table(glm.pred_test ,testdata$y)
  glm.test_error <- (glm.cfmatrix_test[1,2]+glm.cfmatrix_test[2,1])/nrow(testdata)
  glm.error <- c(glm.error, glm.test_error)
  #KNN
  train_x <- traindata[,c("x1", "x2")]
  test_x <- testdata[,c("x1", "x2")]
  train_y <- traindata$y
  test_y <- testdata$y
  #KNN1
  knn1.pred <- knn(train_x,test_x,train_y ,k = 1)
  knn1.cfmatrix_test <- table(knn1.pred,test_y)
  knn1.test_error <- (knn1.cfmatrix_test[1,2]+knn1.cfmatrix_test[2,1])/nrow(testdata)
  KNN1.error <- c(KNN1.error, knn1.test_error)
  #KNNcv
  knncv <- knn.cv(rbind(test_x,train_x),c(test_y,train_y) ,k = 3)
  knncv.cfmatrix_test <- table(knncv,c(test_y,train_y))
  knncv.test_error <- (knncv.cfmatrix_test[1,2]+knncv.cfmatrix_test[2,1])/nrow(testdata)
  KNNCV.error <- c(KNNCV.error, knncv.test_error)
}

error <- cbind(lda.error, qda.error, glm.error, KNN1.error, KNNCV.error)
boxplot(error)
