set.seed(1)
n = 4000
p = 10
sigma <- diag(1,nrow=10)
library(MASS)
X <- mvrnorm(n = n, mu = rep(0,p), Sigma = sigma)
colnames(X) <- c(paste0("X",1:10))
Y <- rep(0,n)
index <- apply(X,1,FUN = function(x){
  sum(x^2)>9.34
})
Y[index] <- 1
casedata <- as.data.frame(cbind(Y,X))
casedata$Y <- factor(casedata$Y)
train <- sample(1:4000,size = 2000, replace = FALSE)

# boosting
library(gbm)
err_boosting <- vector(length =100)
for(i in 1:100){
  boost <- gbm(Y~., data = casedata[train,], distribution = "adaboost",
                    n.trees = i, 
                    interaction.depth = 3)
  pred <- predict(boost, newdata = casedata[-train,-1],n.trees = i,type="response")
  res_boosting <- table(Y[-train],pred)
  err_boosting[i] <- (res_boosting[1,2]+res_boosting[2,1])/sum(res_boosting)
)

# boosting 的预测值很奇怪，无法获得0-1，画不出来

# bagging
library(randomForest)
err_bagging <- vector(length =100)
for(i in 1:100){
  bag <- randomForest(Y~. ,data = casedata[train,],
                      ntree = i, 
                      mtry = p,
                      importance = TRUE)
  bag_pred <- predict(bag, newdata=casedata[-train,-1])
  res_bag <- table(Y[-train],bag_pred)
  err_bagging[i] <- (res_bag[1,2]+res_bag[2,1])/sum(res_bag)
}

plot(1:100, err_bagging, type="l",main="bagging",xlab="number of tree", ylab="test error")
abline(h=err_bagging[1])

err_rf <- vector(length = 100)
for(i in 1:100){
  rf <- randomForest(Y~. ,data = casedata[train,],
                      ntree = i, 
                      mtry = sqrt(p),
                      importance = TRUE)
  rf_pred <- predict(rf, newdata=casedata[-train,-1])
  res_rf <- table(Y[-train],rf_pred)
  err_rf[i] <- (res_rf[1,2]+res_rf[2,1])/sum(res_rf)
}

plot(1:100, err_rf,type="l",main = "rf",xlab="number of tree", ylab="test error")
abline(h=err_rf[1])
  