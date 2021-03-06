library(MASS)
d <- Boston
barplot(d$medv)
boxplot(d$medv)
plot(density(d$medv))
min(d$medv)
max(d$medv)
median(d$medv)
quantile(d$medv,0.25)
quantile(d$medv,0.75)
sd(d$medv)

plot(d$medv, d$lstat)

cor(d$medv, d$lstat)
attach(d)
model <- lm(medv~lstat)
summary(model)
predict(model,data.frame(lstat=(c(5,10,15))))
detach(d)