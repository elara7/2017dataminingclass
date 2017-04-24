library(randomForest)
train <- sample(nrow)
bag.boston <- randomForest(medv~. ,data = Boston, subset = train, 
             ntree = 200, #森林中数的个数
             mtry = dim(Boston)[2]-1, #每次抽取变量个数，列减去1表示全部取，相当于bagging
             importance = TRUE # 显示重要性
             )

bag.boston
boston.test <- Boston[-train,]$medv
yhat.bag <- predict(bag.boston, newdata=Boston[-train,])
plot(yhat.bag, boston.test$medv)
abline(0,1)

rf.boston <- randomForest(medv~. ,data=Boston, subset =  mtry = 6, importance= TRUE)

yhat.bag <- predict(rf.boston,newdata=Boston)

boston.test <- Boston[-train, ]

plot(yhat.bag, boston.test$medv)

importance(rf.boston)
varImpPlot(rf.boston)

rfcv # 变量选择，类似lasso

tuneRF # 选择最优的mtry

partialPlot # 随着自变量变化的时候因变量如何变化的图，相当于普通线性回归的边际效果

# 还有个缺失值填补法


# boosting
library(gbm)
boost.boston <- gbm(medv~., data = Boston[train,], distribution = "gaussian",#表示回归，还有adaboost
    n.trees = 5000, 
    interaction.depth = 
      )

summary(boost.boston)

plot(boost.boston, i ="rm") #rm是变量，表示随着变量变化，值会怎么变 partialplot
plot(boost.boston, i ="istat")

yhat.boost <- predict(boost.boston, newdata=Boston[-train,], )