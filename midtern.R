#Q1

set.seed(1)
bobin <- function(){
  sample(1:6,6,replace = T)
}
result <- matrix(NA,nrow = 10, ncol = 6)
for (i in 1:10){
  result[i,] <- bobin()
}
result
#      [,1] [,2] [,3] [,4] [,5] [,6]
# [1,]    2    3    4    6    2    6
# [2,]    6    4    4    1    2    2
# [3,]    5    3    5    3    5    6
# [4,]    3    5    6    2    4    1
# [5,]    2    3    1    3    6    3
# [6,]    3    4    3    2    5    5
# [7,]    5    1    5    3    5    4
# [8,]    5    4    4    5    1    3
# [9,]    5    5    3    6    3    2
#[10,]    1    1    2    4    4    3

#Q2

fibonacci <- function(n) {
  f1 <- 1
  f2 <- 1

  if(n == 1){
    return(f1)
  }
  if(n == 2){
    return(c(f1, f2))
  }
  if(n >= 3){
    result <- c(f1, f2)
    for(i in 1:(n-2)){
      f3 <- f1 + f2
      f1 <- f2
      f2 <- f3
      result <- c(result,f3)
    }
  return(result)
  }
}
fibonacci(10)

# [1]  1  1  2  3  5  8 13 21 34 55

#Q3

library(ISLR)
default <- Default
default$default <- 0
default$default[Default$default=="Yes"] <- 1
default$student <- 0
default$student[Default$student=="Yes"] <- 1
set.seed(1)
testindex <- sample(1:nrow(default), nrow(default)*0.3, replace = F)
train <- default[-testindex,]
test <- default[testindex,]

#Q3 (1)
library(class)
folds <- 5
train_n <- nrow(train)
index <- sample(rep(1:folds,train_n/folds),train_n, replace = F)
label <- unique(index)
error <- NULL
for(k in 1:10){
  knn.error <- NULL
  for(i in label){
    knn.pred <- knn(train = train[index!=i,-1],test = train[index==i,-1],cl = train[index!=i,1],k=k)
    knn.cfmatrix_cv <- table(knn.pred,train[index==i,1])
    knn.cv_error <- (knn.cfmatrix_cv[1,2]+knn.cfmatrix_cv[2,1])/nrow(train[index==i,])
    knn.error <- c(knn.error, knn.cv_error)
  }
  error <- c(error,mean(knn.error))
}
error
#[1] 0.04900000 0.04585714 0.03614286 0.03542857 0.03357143 0.03414286 0.03328571 0.03328571 0.03371429 0.03371429
which.min(error)
#[1] 7

#Q3 (2)
#KNN
knn.pred <- knn(train = train[,-1],test = test[,-1],cl = train$default,k=7)
knn.cfmatrix_test <- table(knn.pred,test$default)
knn.test_error <- (knn.cfmatrix_test[1,2]+knn.cfmatrix_test[2,1])/nrow(test)
knn.test_error
#[1] 0.03066667

#LDA
lda.fit <- lda(default~.,data = train)
lda.pred_test <- predict(lda.fit, test[,-1])$class
lda.cfmatrix_test <- table(lda.pred_test, test$default)
lda.test_error <- (lda.cfmatrix_test[1,2]+lda.cfmatrix_test[2,1])/nrow(test)
lda.test_error 
#[1] 0.02666667

#QDA
qda.fit <- qda(default~.,data = train)
qda.pred_test <- predict(qda.fit, test[,-1])$class
qda.cfmatrix_test <- table(qda.pred_test, test$default)
qda.test_error <- (qda.cfmatrix_test[1,2]+qda.cfmatrix_test[2,1])/nrow(test)
qda.test_error
#[1] 0.027

#logistic
glm.fit <- glm(default~., data = train, family = binomial)
glm.prob_test <- predict(glm.fit,newdata = test[,-1],type = "response")
glm.pred_test <- rep(0, nrow(test))
glm.pred_test[glm.prob_test > 0.5] <- 1
glm.cfmatrix_test <- table(glm.pred_test ,test$default)
glm.test_error <- (glm.cfmatrix_test[1,2]+glm.cfmatrix_test[2,1])/nrow(test)
glm.test_error
#[1] 0.02733333

which.min(c(knn.test_error, lda.test_error, qda.test_error, glm.test_error))
#[1] 2
#lda is the best one

#Q4
library(ncvreg)
library(MASS)
summa <- function(beta0, betahat){
  n0 <- length(beta0)
  nhat <- length(betahat)
  if (n0 != nhat){
    stop("the length of betas are different")
  }
  
  TP = TN = FN = FP = 0
  
  betapre <- rep(0,nhat)
  betapre[betahat!=0] <- 1
  
  for (i in 1:n0){
    if(betapre[i]==1 & beta0[i]==1){TP = TP + 1}
    if(betapre[i]==1 & beta0[i]==0){FP = FP + 1}
    if(betapre[i]==0 & beta0[i]==1){FN = FN + 1}
    if(betapre[i]==0 & beta0[i]==0){TN = TN + 1}
  }
  
  cfmatrix <- matrix(c(TP,FN,FP,TN),nrow = 2)
  rownames(cfmatrix) <- c("estimate sig","estimate non sig")
  colnames(cfmatrix) <- c("true sig","true non sig")
  FDR <- FP/(TN+FP)
  FNR <- FN/(TP+FN)
  return(list(matrix = cfmatrix, FDR = FDR, FNR = FNR))
}

simdata <- function(n, p, ro, mu){
  
  sigma = matrix(NA,nrow = p,ncol = p)
  for (i in 1:p){
    for (j in 1:p){
      sigma[i,j] = ro^abs(i-j)
    }
  }
  return(mvrnorm(n=n,mu=mu,Sigma=sigma))
}

set.seed(1)
n=100
p=100
mu=rep(0,p)
FDR.lasso <- NULL
FNR.lasso <- NULL
FDR.MCP <- NULL
FNR.MCP <- NULL
FDR.SCAD <- NULL
FNR.SCAD <- NULL
beta <- c(1,1,1,1,1,0,0,0,0,0,0.8,0.8,0.8,0.8,0.8,0,0,0,0,0,rep(0,80))

for (i in 1:100){
  X <- simdata(n, p, 0.5, mu)
  e <- rnorm(n = n, mean = 0, sd = 1)
  Y <- X%*%beta + e
  mod.lasso <- cv.ncvreg(X,Y,family="gaussian",penalty = "lasso" )
  coef.lasso <- coef(mod.lasso)[-1]
  summa.lasso <- summa(beta,coef.lasso)
  FDR.lasso <- c(FDR.lasso, summa.lasso$FDR)
  FNR.lasso <- c(FNR.lasso, summa.lasso$FNR)
  
  mod.MCP <- cv.ncvreg(X,Y,family="gaussian",penalty = "MCP" )
  coef.MCP <- coef(mod.MCP)[-1]
  summa.MCP <- summa(beta,coef.MCP)
  FDR.MCP <- c(FDR.MCP, summa.MCP$FDR)
  FNR.MCP <- c(FNR.MCP, summa.MCP$FNR)
  
  mod.SCAD <- cv.ncvreg(X,Y,family="gaussian",penalty = "SCAD" )
  coef.SCAD <- coef(mod.SCAD)[-1]
  summa.SCAD <- summa(beta,coef.SCAD)
  FDR.SCAD <- c(FDR.SCAD, summa.SCAD$FDR)
  FNR.SCAD <- c(FNR.SCAD, summa.SCAD$FNR)
}

#(1)
mean(FDR.lasso)
#[1] 0.1
sd(FDR.lasso)
#[1] 0.04376586
mean(FNR.lasso)
#[1] 0
sd(FNR.lasso)
#[1] 0

mean(FDR.MCP)
#[1] 0.02977778
sd(FDR.MCP)
#[1] 0.02135689
mean(FNR.MCP)
#[1] 0
sd(FNR.MCP)
#[1] 0

mean(FDR.SCAD)
#[1] 0.073
sd(FDR.SCAD)
#[1] 0.03377024
mean(FNR.SCAD)
#[1] 0
sd(FNR.SCAD)
#[1] 0

boxplot(FDR.lasso,FDR.MCP,FDR.SCAD, names = c('lasso','MCP','SCAD'),ylab = 'FDR', main = "FDR")
boxplot(FNR.lasso,FNR.MCP,FNR.SCAD, names = c('lasso','MCP','SCAD'),ylab = 'FNR', main = "FNR")
