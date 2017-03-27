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


library(ISLR)
data(Hitters)
View(Hitters)
Hitters = na.omit(Hitters)
library(leaps)
regfit.full=regsubsets(Salary~., Hitters)
summary(regfit.full)
regfit.full=regsubsets(Salary~.,Hitters,nvmax=19)
reg.summary=summary(regfit.full)
reg.summary
plot(reg.summary$rss)
plot(reg.summary$rsq)
plot(reg.summary$adjr2)
plot(reg.summary$bic)
