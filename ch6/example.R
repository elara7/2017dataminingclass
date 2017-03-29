USArrests <- USArrests
USA2 <- scale(USArrests)

pr.out=prcomp(USArrests, scale=TRUE)
pr.out
summary(pr.out) # loadings 前面系数正负没什么太大意义
pr.out$rotation
loadings(pr.out)
biplot(pr.out)
pr.out$x
pr.out$rotation = -pr.out$rotation
pr.out$x = -pr.out$x
biplot(pr.out)
pr.out$sdev
pr.var=pr.out$sdev^2
pve=pr.var/sum(pr.out$sdev^2)


USArrests <- scale(USArrests)
pc <- princomp(USArrests, cor = TRUE)
summary(pc)
loadings(pc)
screeplot(pc, type = 'lines')
biplot(pc)


######################################

# best subset
library(ISLR)
data(Hitters)
View(Hitters)
Hitters = na.omit(Hitters)
library(leaps)
regfit.full=regsubsets(Salary~., Hitters)
summary(regfit.full)
regfit.full=regsubsets(Salary~.,Hitters,nvmax=19) # nvmax 最多选出19个变量
reg.summary=summary(regfit.full)
reg.summary
plot(reg.summary$rss)
plot(reg.summary$rsq)
plot(reg.summary$adjr2)
plot(reg.summary$bic)
which.max(reg.summary$adjr2)
plot(reg.summary$adjr2)
points(11,reg.summary$adjr2[11], col ='red', cex = 2)
plot(reg.summary$cp)
points(6, reg.summary$adjr2)

# stepwise

regfit.bwd = regsubsets(Salary~. ,data= Hitters, method = "backward")
summary(regfit.bwd)

coef(regfit.full, 7) # 7表示选7个变量
coef(regfit.fwd,7)
coef(regfit.bwd,7)

# 用validation set选择最优模型

set.seed(1)
train = sample(c(TRUE,FALSE), nrow(Hitters), rep = TRUE)
test = (!train)
regfit.best = regsubsets(Salary~., data=Hitters[train,],nvmax = 19) #训练模型
test.mat = model.matrix(Salary~., data=Hitters[test,]) #将测试集转换为design matrix，Salary是因变量
val.errors=rep(NA, 19)
for(i in 1:19){
  coefi = coef(regfit.best, id = i) #id为选择的变量个数，提取系数
  pred=test.mat[, names(coefi)]%*%coefi # 提取coef中有的变量，计算预测值
  val.errors[i]=mean((Hitters$Salary[test]-pred)^2)
}
val.errors
which.min(val.errors)
plot(1:19, val.errors,type="l")

# CV

predict.regsubsets=function(object,newdata,id,...){
  form = as.formula(object$call [[2]])#变量名
  mat=model.matrix(form, newdata)
  coefi= coef(object, id = id)
  xvars = names(coefi)
  mat[,xvars]%*%coefi
}
regfit.best=regsubsets(Salary~., data=Hitters, nvmax = 19)
k = 10
set.seed(1)
folds = sample(1:k, nrow(Hitters), replace = T)
cv.errors = matrix(NA, k, 19, dimnames = list(NULL, paste())) #10次测试（行），19个模型（列）

for(j in 1:k){
  best.fit=regsubsets(Salary~., data=Hitters[folds!=j,],nvmax=19) #去掉测试，建模
  for(i in 1:19){
    pred = predict(best.fit,Hitters[folds==j,],id = i)# 用测试机，测试
    cv.errors[j,i]=mean((Hitters$Salary[folds==j]-pred)^2)
  }
}

mean.cv.errors=apply(cv.errors, 2, mean)
which.min(mean.cv.errors)
reg.best = regsubsets(Salary~., data=Hitters, nvmax = 19)
coef(reg.best,11)


# ridge
library(glmnet)
x=model.matrix(Salary~.,Hitters)[,-1]# 转换为design Matrix的时候拆分出自变量
y=Hitters$Salary#因变量
grid=10^seq(10,-2,length=100)#lambda格点
grid
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
dim(coef(ridge.mod))
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))
ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))
plot(ridge.mod)
cv.out <- cv.glmnet(x,y,alpha=0,lambda=grid)
plot(cv.out) # 最小值和最小值周围一个标准差都会画线
coef(cv.out)
(bestridge <- cv.out$lambda.min)

# MCP

library(ncvreg)
fit1 <- ncvreg(x,y,family="gaussian",penalty = "MCP" )
plot(fit1)
fit11 <- cv.ncvreg(x,y,family="gaussian",penalty = "MCP")
plot(fit11)
coef(fit11)
fit2 <- ncvreg(x,y,family="gaussian",penalty = "SCAD")
plot(fit2)

sigma = matrix(NA,p,p)
for(i in 1:p){
  for(j in 1:p)
    
}