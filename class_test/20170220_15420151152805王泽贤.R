n <- 100

(1)

X <- rnorm(n,2,sqrt(2))
e <- rnorm(n,0,1)
Y <- 2+3*X+e
plot(X,Y)
coef <- lm(Y~X)$coefficients
fitY <- cbind(rep(1,n),X) %*% coef
points(X,Y)
abline(coef[1],coef[2],col="red")
abline(2,3, col="blue")

(2)

coefm <- matrix(rep(0,1000*2),nrow = 1000)
for(i in 1:1000){
X <- rnorm(n,2,sqrt(2))
e <- rnorm(n,0,1)
Y <- 2+3*X+e
coefm[i,] <- lm(Y~X)$coefficients
}
boxplot(coefm[,1])
boxplot(coefm[,2])
mean(coefm[,1])
mean(coefm[,2])
median(coefm[,1])
median(coefm[,2])
