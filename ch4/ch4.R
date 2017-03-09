library(ISLR)
data(Default)
View(Default)
glm.de<-glm(default~balance,family="binomial",data=Default)
summary(glm.de)
predict(glm.de,newdata=data.frame(balance=1000),type="response")
predict(glm.de,newdata=data.frame(balance=2000),type="response")

glm.de2<-glm(default~student,family="binomial",data=Default)
summary(glm.de2)

glm.de3<-glm(default~balance+income+student,family="binomial",data=Default)
summary(glm.de3)
############### 
##Example: The stock market data
library(ISLR)
names(Smarket)
View(Smarket)
dim(Smarket)
pairs(Smarket) # 变量相关性矩阵
cor(Smarket) # 错误示范
cor(Smarket[,c(-1,-9)])
##logistic regression
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume , data=Smarket ,family=binomial)
summary(glm.fit)
glm.probs=predict(glm.fit,type="response")
glm.probs[1:10]
glm.pred=rep("Down",1250)
glm.pred[glm.probs >.5]="Up"
table(glm.pred,Smarket$Direction)
train =( Smarket$Year <2005)
Smarket.2005= Smarket [!train ,]
dim(Smarket.2005)
attach(Smarket)
Direction.2005= Direction [! train ]
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume , data=Smarket ,family=binomial,subset=train)
glm.probs=predict(glm.fit,Smarket.2005,type="response") # type为 link 的话是log(p(x)/(1-p(x))), response为p(x)
glm.pred=rep("Down",252)
glm.pred[glm.probs >.5]="Up"
table(glm.pred,Direction.2005)

####linear discriminant analysis
library(MASS)
lda.fit=lda(Direction~Lag1+Lag2,data=Smarket ,subset=train)
lda.fit
plot(lda.fit)
lda.pred=predict(lda.fit, Smarket.2005)
lda.pred # class就是类， posterior是各个样本对应类的后验概率
lda.class=lda.pred$class
table(lda.class ,Direction.2005)
####quadratic discriminant analysis
qda.fit=qda(Direction~Lag1+Lag2,data=Smarket ,subset=train)
qda.fit
qda.class=predict(qda.fit,Smarket.2005)$class 
table(qda.class ,Direction.2005)
####KNN
library(class)
train.X=cbind(Lag1,Lag2)[train,]
test.X=cbind(Lag1,Lag2)[!train,]
train.Direction =Direction [train]
set.seed(1)
knn.pred=knn(train.X,test.X,train.Direction ,k=1)
table(knn.pred,Direction.2005)
knn.pred=knn(train.X,test.X,train.Direction ,k=3)
table(knn.pred,Direction.2005)
knn.cv(train.X,train.Direction ,k=3)
