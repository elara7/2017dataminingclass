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
beta0 <- 1
beta1 <- 2
beta2 <- 3
beta <- c(beta0, beta1, beta2)
library(MASS)
Sigma <- matrix(c(1,0,0,1),2,2)
class1_x <- cbind(mvrnorm(n=20, c(0,2), Sigma))
class2_x <- cbind(mvrnorm(n=20, c(4,1), Sigma))
class1_y <- rep(1,20)
class2_y <- rep(0,20)
mydata <- rbind(cbind(class1_y, class1_x), cbind(class2_y, class2_x))
colnames(mydata) <- c("y","x1","x2")
mydata <- as.data.frame(mydata)
glmfit <- glm(y ~ x1 + x2,family=binomial,data=mydata)
lda.fit <- lda(y~x1+x2,data=mydata)
qda.fit=qda(y~x1+x2 ,data=mydata)
